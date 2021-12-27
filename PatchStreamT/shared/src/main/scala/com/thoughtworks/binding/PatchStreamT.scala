package com.thoughtworks.binding

import scalaz.-\/
import scalaz.Applicative
import scalaz.DList
import scalaz.Equal
import scalaz.FingerTree
import scalaz.Functor
import scalaz.Free
import scalaz.IList
import scalaz.Maybe
import scalaz.Memo
import scalaz.Monad
import scalaz.MonadPlus
import scalaz.Monoid
import scalaz.Nondeterminism
import scalaz.Reducer
import scalaz.StreamT
import scalaz.StreamT.Done
import scalaz.StreamT.Skip
import scalaz.StreamT.Yield
import scalaz.UnitReducer
import scalaz.\/-

import scala.concurrent.Future
import scala.collection.IndexedSeqView
import scalaz.StreamT.Step
import scala.annotation.unchecked.uncheckedVariance
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.binding.StreamT.*

opaque type PatchStreamT[M[_], +A] = CovariantStreamT[M, PatchStreamT.Patch[A]]
object PatchStreamT:

  // Copied from https://github.com/scalaz/scalaz/pull/2234
  extension [V, A](tree: FingerTree[V, A])
    private def measureMonoid(implicit V: Monoid[V]): V =
      tree.fold(V.zero, (v, _) => v, (v, _, _, _) => v)

  def apply[M[_], A]
      : CovariantStreamT[M, PatchStreamT.Patch[A]] =:= PatchStreamT[M, A] =
    summon

  sealed trait Patch[+A]

  object Patch:
    // TODO: Support move items
    // final case class Move[A](oldIndex: Int, offset: Int, moveCount: Int) extends Patch[A]
    final case class ReplaceChildren[+A](
        newItems: Iterable[A]
    ) extends Patch[A]

    final case class Splice[+A](
        index: Int,
        deleteCount: Int,
        newItems: Iterable[A]
    ) extends Patch[A]

  extension [M[_], A](upstream: PatchStreamT[M, A])

    /** Return a [[CovariantStreamT]], whose each element is a
      * [[scalaz.FingerTree]] representing the snapshot of the sequence at that
      * time.
      *
      * @note
      *   The measurement of the [[scalaz.FingerTree]] is the size.
      * @example
      *   Given a [[PatchStreamT]] created from an iterable,
      * {{{
      * import scala.concurrent.Future
      * import scalaz.std.scalaFuture.given
      * val bindingSeq = PatchStreamT.fromIterable[Future, String](Seq("foo", "bar", "baz"))
      * }}}
      * when taking [[snapshots]],
      * {{{
      * val snapshots = bindingSeq.snapshots
      * }}}
      * then it should contains an empty value and an initial value
      * {{{
      * import com.thoughtworks.dsl.keywords.Await
      * import com.thoughtworks.dsl.macros.Reset.Default.*
      * `*`[Future] {
      *   val snapshotLazyList = !Await(snapshots.toLazyList)
      *   inside(snapshotLazyList) {
      *     case LazyList(empty, initial) =>
      *       empty.toList should be(Nil)
      *       initial.toList should be(List("foo", "bar", "baz"))
      *   }
      * }
      * }}}
      */
    def snapshots(using
        Applicative[M]
    ): CovariantStreamT[M, FingerTree[Int, A]] =
      import scalaz.std.anyVal.intInstance
      given scalaz.Reducer[A, Int] = UnitReducer(x => 1)
      upstream.scanLeft(FingerTree.empty) { (fingerTree, patch) =>
        patch match
          case Patch.ReplaceChildren(newItems) =>
            newItems.foldLeft(FingerTree.empty[Int, A]) { (tree, a) =>
              tree :+ a
            }
          case Patch.Splice(
                index,
                deletedCount,
                newItems
              ) =>
            val (left, notLeft) =
              fingerTree.split(_ > index)
            val (deleted, right) =
              notLeft.split(_ > deletedCount)
            newItems.foldLeft(left) { (tree, a) =>
              tree :+ a
            } <++> right
      }

    def mergeWithEventLoop(eventLoop: CovariantStreamT[M, Nothing])(using
        Nondeterminism[M]
    ): PatchStreamT[M, A] =
      upstream.mergeWith(eventLoop)

    /** Returns a new data-binding sequence by applying a function to all
      * elements of this sequence and using the elements of the resulting
      * collections.
      *
      * Whenever `upstream` or one of the subsequence changes, the result
      * sequence changes accordingly. The time complexity to update the result
      * sequence, when `upstream` changes, is `O(log(n) + c)`, where `n` is the
      * size of `upstream`, and `c` is number of elements in `upstream`, mapped
      * to mutable subsequences by `f`, of which the indices in `upstream` are
      * changing. For example, if the size of `upstream` is 10 and 4 elements of
      * them mapped to mutable subsequences, when an element is prepended to
      * `upstream`, `c` is 4. However when an element is appended to `upstream`,
      * `c` is zero, because no element's index in `upstream is changed.
      *
      * The time complexity to update the result sequence, when one of the
      * subsequence changes, is `O(log(n))`, where `n` is the size of
      * `upstream`.
      *
      * @example
      *   Given a source [[PatchStreamT]] from an iterable,
      * {{{
      * import scala.concurrent.Future
      * import scalaz.std.scalaFuture.given
      * val bindingSeq = PatchStreamT.fromIterable[Future, String](Seq("foo", "bar"))
      * }}}
      *
      * when flat-mapping it to more [[PatchStreamT]],
      * {{{
      * val flatten = bindingSeq.flatMap { s =>
      *   PatchStreamT.fromIterable(s)
      * }
      * }}}
      * then it should returns a [[StreamT]] including each intermediate states
      * during flat-mapping
      * {{{
      * import com.thoughtworks.dsl.keywords.Await
      * import com.thoughtworks.dsl.macros.Reset.Default.*
      * `*`[Future] {
      *   val snapshotLazyList = !Await(flatten.snapshots.toLazyList)
      *   snapshotLazyList.map(_.toList.mkString) should be(
      *     LazyList(
      *       "",
      *       "foo",
      *       "foobar",
      *     )
      *   )
      * }
      * }}}
      */
    def flatMap[B](f: A => PatchStreamT[M, B])(using
        M: Nondeterminism[M]
    ): PatchStreamT[M, B] =
      val toStepB = { (a: A) =>
        f(a).step
      }
      type EventStep[B] =
        StreamT.Step[Patch[B], scalaz.StreamT[M, Patch[B]]]
      final case class SliceEvent(eventStep: EventStep[B], sliceIndex: Int)
      final case class Measure(
          numberOfSlices: Int,
          numberOfElements: Int,
          sliceEventBuilder: Option[
            (numberOfPreviousSlices: Int) => M[SliceEvent]
          ]
      )
      sealed trait Slice:
        def size: Int
      final case class ActiveSlice(size: Int, eventQueue: M[EventStep[B]])
          extends Slice
      final case class InactiveSlice(size: Int) extends Slice
      val emptyEventBuilder = { (numberOfPreviousSlices: Int) =>
        DList.mkDList[M[SliceEvent]](Free.pure)
      }
      given Monoid[Measure] with
        def zero = Measure(0, 0, None)
        def append(f1: Measure, f2: => Measure) =
          val numberOfSlices1 = f1.numberOfSlices
          Measure(
            f1.numberOfSlices + f2.numberOfSlices,
            f1.numberOfElements + f2.numberOfElements,
            (f1.sliceEventBuilder, f2.sliceEventBuilder) match
              case (None, None) =>
                None
              case (None, Some(builder2)) =>
                Some(Memo.immutableHashMapMemo {
                  (numberOfPreviousSlices: Int) =>
                    builder2(numberOfPreviousSlices + numberOfSlices1)
                })
              case (someBuilder1: Some[_], None) =>
                someBuilder1
              case (Some(builder1), Some(builder2)) =>
                Some(Memo.immutableHashMapMemo {
                  (numberOfPreviousSlices: Int) =>
                    M.map(
                      M.choose(
                        builder1(numberOfPreviousSlices),
                        builder2(numberOfPreviousSlices + numberOfSlices1)
                      )
                    ) {
                      case -\/((sliceEvent1, _)) => sliceEvent1
                      case \/-((_, sliceEvent2)) => sliceEvent2
                    }
                })
          )
      given Reducer[Slice, Measure] = UnitReducer { (slice: Slice) =>
        Measure(
          1,
          slice.size,
          slice match {
            case InactiveSlice(_) =>
              None
            case ActiveSlice(_, eventQueue) =>
              Some(Memo.immutableHashMapMemo { (numberOfPreviousSlices: Int) =>
                M.map(eventQueue)(SliceEvent(_, numberOfPreviousSlices))
              })
          }
        )
      }

      def mergeEventQueue(
          upstreamEventQueueOption: Option[M[EventStep[A]]],
          sliceTree: FingerTree[Measure, Slice]
      ): Option[M[EventStep[B]]] =
        def handleUpstreamEvent(upstreamEvent: EventStep[A]): EventStep[B] =
          upstreamEvent match
            case Yield(
                  // Work around https://github.com/lampepfl/dotty/issues/13998
                  patch: Patch[A],
                  s
                ) =>
              patch match
                case Patch.ReplaceChildren(newItems) =>
                  val newSliceTree = newItems.foldLeft(FingerTree.empty) {
                    (tree, a) =>
                      tree :+ ActiveSlice(0, toStepB(a))
                  }
                  val Some(next) = mergeEventQueue(Some(s().step), newSliceTree)
                  sliceTree.measure match
                    case Maybe.Just(measure) if measure.numberOfElements > 0 =>
                      Yield(Patch.ReplaceChildren(Nil), () => StreamT(next))
                    case _ =>
                      Skip(() => StreamT(next))
                case Patch.Splice(
                      patchIndex,
                      numberOfSlicesDeleted,
                      newItems
                    ) =>
                  val (left, notLeft) =
                    sliceTree.split(_.numberOfSlices > patchIndex)
                  val (deleted, right) =
                    notLeft.split(_.numberOfSlices > numberOfSlicesDeleted)
                  val Measure(`patchIndex`, mappedIndex, _) = left.measureMonoid
                  val Measure(
                    `numberOfSlicesDeleted`,
                    numberOfItemsDeleted,
                    _
                  ) = deleted.measureMonoid
                  val numberOfSlicesAdd = newItems.size
                  val newSliceTree = newItems.foldLeft(left) { (tree, a) =>
                    tree :+ ActiveSlice(0, toStepB(a))
                  } <++> right
                  val Some(next) = mergeEventQueue(Some(s().step), newSliceTree)
                  if (numberOfItemsDeleted > 0) {
                    Yield(
                      Patch.Splice(mappedIndex, numberOfItemsDeleted, Nil),
                      () => StreamT(next)
                    )
                  } else {
                    Skip(() => StreamT(next))
                  }
            case Skip(s) =>
              val Some(next) = mergeEventQueue(Some(s().step), sliceTree)
              Skip(() => StreamT(next))
            case Done() =>
              mergeEventQueue(None, sliceTree) match
                case Some(next) =>
                  Skip(() => StreamT(next))
                case None =>
                  Done()
        def handleSliceEvent(sliceEvent: SliceEvent): EventStep[B] =
          val sliceIndex = sliceEvent.sliceIndex
          val (left, oldSlice, right) =
            sliceTree.split1(_.numberOfSlices > sliceIndex)
          sliceEvent.eventStep match
            case Yield(patch, s) =>
              val newSlice = patch match
                case Patch.Splice(_, deleteCount, newItems) =>
                  ActiveSlice(
                    oldSlice.size + newItems.size - deleteCount,
                    s().step
                  )
                case Patch.ReplaceChildren(newItems) =>
                  ActiveSlice(
                    newItems.size,
                    s().step
                  )
              val newSliceTree = left.add1(newSlice, right)
              val Some(next) =
                mergeEventQueue(upstreamEventQueueOption, newSliceTree)
              val patchWithOffset = patch match
                case splice: Patch.Splice[_] =>
                  left.measure match
                    case Maybe.Empty() =>
                      splice
                    case Maybe.Just(leftMeasure) =>
                      splice.copy(index =
                        splice.index + leftMeasure.numberOfElements
                      )
                case Patch.ReplaceChildren(newItems) =>
                  val offset = left.measure match
                    case Maybe.Empty() =>
                      0
                    case Maybe.Just(leftMeasure) =>
                      leftMeasure.numberOfElements
                  Patch.Splice(offset, oldSlice.size, newItems)
              Yield(patchWithOffset, () => StreamT(next))
            case Skip(s) =>
              val newSliceTree = left.add1(
                ActiveSlice(oldSlice.size, s().step),
                right
              )
              val Some(next) =
                mergeEventQueue(upstreamEventQueueOption, newSliceTree)
              Skip(() => StreamT(next))
            case Done() =>
              val newSliceTree = left.add1(InactiveSlice(oldSlice.size), right)
              mergeEventQueue(upstreamEventQueueOption, newSliceTree) match
                case Some(next) =>
                  Skip(() => StreamT(next))
                case None =>
                  Done()
        val chooseSliceEventOption = sliceTree.measure.toOption.flatMap {
          measure =>
            measure.sliceEventBuilder.map(_(0))
        }
        upstreamEventQueueOption match
          case None =>
            chooseSliceEventOption match
              case None =>
                None
              case Some(chooseEvent) =>
                Some(M.map(chooseEvent)(handleSliceEvent))
          case Some(upstreamEventQueue) =>
            chooseSliceEventOption match
              case None =>
                Some(M.map(upstreamEventQueue) { upstreamEvent =>
                  handleUpstreamEvent(upstreamEvent)
                })
              case Some(chooseEvent) =>
                Some(M.map(M.choose(upstreamEventQueue, chooseEvent)) {
                  case \/-((_, sliceEvent)) =>
                    handleSliceEvent(sliceEvent)
                  case -\/((upstreamEvent, _)) =>
                    handleUpstreamEvent(upstreamEvent)
                })
      val Some(mergedEventQueue) =
        mergeEventQueue(Some(upstream.step), FingerTree.empty)
      StreamT(mergedEventQueue)
  def fromCovariantStreamT[M[_], A](
      stream: CovariantStreamT[M, A]
  )(using Functor[M]): PatchStreamT[M, A] =
    stream.map { a =>
      Patch.ReplaceChildren(collection.View.Single(a))
    }

  def fromIterable[M[_], A](iterable: Iterable[A])(using
      Applicative[M]
  ): PatchStreamT[M, A] =
    CovariantStreamT(Patch.ReplaceChildren[A](iterable) :: StreamT.empty)

  given [M[_]](using M: Nondeterminism[M]): Monad[[X] =>> PatchStreamT[M, X]]
    with
    def point[A](a: => A): PatchStreamT[M, A] =
      fromIterable(collection.View.Single(a))

    def bind[A, B](upstream: PatchStreamT[M, A])(
        f: A => PatchStreamT[M, B]
    ): PatchStreamT[M, B] =
      upstream.flatMap(f)

  given [M[_], A](using
      M: Applicative[M]
  ): Dsl.Lift.OneStep[Iterable[A], PatchStreamT[M, A]] =
    fromIterable(_)

  given [Keyword, M[_], Element, Value](using
      streamDsl: Dsl.Searching[Keyword, CovariantStreamT[M, PatchStreamT.Patch[
        Element
      ]], Value]
  ): Dsl.Derived.StackSafe[Keyword, PatchStreamT[M, Element], Value] =
    Dsl.Derived.StackSafe(streamDsl)

  given [M[_], A, From](using
      toStream: From <:< CovariantStreamT[M, A],
      M: Functor[M]
  ): Dsl.Lift.OneStep[From, PatchStreamT[M, A]] = from =>
    PatchStreamT.fromCovariantStreamT(toStream(from))
