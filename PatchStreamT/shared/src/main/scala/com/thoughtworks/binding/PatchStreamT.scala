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
import scalaz.ReaderT
import scalaz.Semigroup

opaque type PatchStreamT[M[_], +A] = CovariantStreamT[M, PatchStreamT.Patch[A]]
object PatchStreamT extends PatchStreamT.LowPriority0:

  // Copied from https://github.com/scalaz/scalaz/pull/2234
  extension [V, A](tree: FingerTree[V, A])
    private def measureMonoid(implicit V: Monoid[V]): V =
      tree.fold(V.zero, (v, _) => v, (v, _, _, _) => v)

  def apply[M[_], A]
      : CovariantStreamT[M, PatchStreamT.Patch[A]] =:= PatchStreamT[M, A] =
    summon

  /** A [[scalaz.FingerTree]] representing the snapshot of the sequence at a
    * certain time.
    * @note
    *   The measurement of this [[scalaz.FingerTree]] is the size.
    */
  opaque type Snapshot[+A] <: FingerTree[Int, A @uncheckedVariance] =
    FingerTree[Int, A @uncheckedVariance]
  private[binding] object Snapshot:
    def empty[A]: Snapshot[A] =
      import scalaz.std.anyVal.intInstance
      @inline given Reducer[A, Int] = UnitReducer(x => 1)
      FingerTree.empty

  sealed trait Patch[+A]:
    private[PatchStreamT] def newSize(oldSize: Int): Int
    private[binding] def applyTo[B >: A](
        snapshot: Snapshot[B]
    ): Snapshot[B]

  object Patch:
    // TODO: Support move items
    // final case class Move[A](oldIndex: Int, offset: Int, moveCount: Int) extends Patch[A]
    final case class ReplaceChildren[+A](
        newItems: Iterable[A]
    ) extends Patch[A]:
      private[PatchStreamT] def newSize(oldSize: Int) = newItems.size
      private[binding] def applyTo[B >: A](
          snapshot: Snapshot[B]
      ): Snapshot[B] =
        newItems.foldLeft(Snapshot.empty[B]) { (tree, a) =>
          tree :+ a
        }

    final case class Splice[+A](
        index: Int,
        deleteCount: Int,
        newItems: Iterable[A]
    ) extends Patch[A]:
      private[PatchStreamT] def newSize(oldSize: Int) =
        oldSize - deleteCount + newItems.size
      private[binding] def applyTo[B >: A](
          snapshot: Snapshot[B]
      ): Snapshot[B] =
        val (left, notLeft) =
          snapshot.split(_ > index)
        val (deleted, right) =
          notLeft.split(_ > deleteCount)
        newItems.foldLeft(left) { (tree, a) =>
          tree :+ a
        } <++> right

  extension [M[_], A](upstream: => PatchStreamT[M, A])
    def mergeWith(
        eventLoop: => CovariantStreamT[M, Nothing]
    )(using Nondeterminism[M]): PatchStreamT[M, A] =
      PatchStreamT(
        CovariantStreamT.mergeWith[M, Patch[A]](
          PatchStreamT.apply.flip(upstream)
        )(eventLoop)
      )
    def noSkip(using Monad[M]): PatchStreamT[M, A] =
      CovariantStreamT.noSkip(upstream)
    def memoize(using Functor[M]): PatchStreamT[M, A] =
      CovariantStreamT.memoize(upstream)
    def mergeMap[B](mapper: A => CovariantStreamT[M, B])(using
        M: Nondeterminism[M]
    ): CovariantStreamT[M, B] =
      type MappedStep = StreamT.Step[B, StreamT[M, B]]
      final case class MappedEvent(step: MappedStep, index: Int)
      final case class MappedMeasure(
          size: Int,
          mappedEventBuilder: Option[
            (index: Int) => M[MappedEvent]
          ]
      )
      given Monoid[MappedMeasure] with
        @inline def zero = MappedMeasure(0, None)
        def append(f1: MappedMeasure, f2: => MappedMeasure) =
          val index1 = f1.size
          MappedMeasure(
            f1.size + f2.size,
            (f1.mappedEventBuilder, f2.mappedEventBuilder) match
              case (None, None) =>
                None
              case (None, Some(builder2)) =>
                Some(Memo.immutableHashMapMemo { (index: Int) =>
                  builder2(index + index1)
                })
              case (someBuilder1: Some[Int => M[MappedEvent]], None) =>
                someBuilder1
              case (Some(builder1), Some(builder2)) =>
                Some(Memo.immutableHashMapMemo { (index: Int) =>
                  M.map(
                    M.choose(
                      builder1(index),
                      builder2(index + index1)
                    )
                  ) {
                    case -\/((sliceEvent1, _)) => sliceEvent1
                    case \/-((_, sliceEvent2)) => sliceEvent2
                  }
                })
          )
      @inline given Reducer[Option[
        M[StreamT.Step[B, StreamT[M, B]]]
      ], MappedMeasure] =
        UnitReducer { element =>
          MappedMeasure(
            1,
            element.map { activeElement =>
              Memo.immutableHashMapMemo { index =>
                M.map(activeElement)(MappedEvent(_, index))
              }
            }
          )
        }
      @inline def loop(
          upstreamEventQueueOption: Option[
            M[StreamT.Step[Patch[A], StreamT[M, Patch[A]]]]
          ],
          mappedTree: FingerTree[MappedMeasure, Option[
            M[StreamT.Step[B, StreamT[M, B]]]
          ]]
      ): CovariantStreamT[M, B] =
        def handleUpstreamEvent(
            upstreamEvent: StreamT.Step[Patch[A], StreamT[M, Patch[A]]]
        ): StreamT.Step[B, StreamT[M, B]] =
          Skip { () =>
            CovariantStreamT.apply.flip(upstreamEvent match
              case Yield(
                    // Work around https://github.com/lampepfl/dotty/issues/13998
                    patch: Patch[A],
                    s
                  ) =>
                patch match
                  case Patch.ReplaceChildren(newItems) =>
                    val newTree =
                      newItems.foldLeft(FingerTree.empty) { (tree, item) =>
                        tree :+ Some(mapper(item).step)
                      }
                    loop(Some(s().step), newTree)
                  case Patch.Splice(
                        patchIndex,
                        deleteCount,
                        newItems
                      ) =>
                    val (left, notLeft) =
                      mappedTree.split(_.size > patchIndex)
                    val (deleted, right) =
                      notLeft.split(_.size > deleteCount)
                    val newTree = newItems.foldLeft(left) { (tree, item) =>
                      tree :+ Some(mapper(item).step)
                    } <++> right
                    loop(Some(s().step), newTree)
              case Skip(s) =>
                loop(Some(s().step), mappedTree)
              case Done() =>
                loop(None, mappedTree)
            )
          }

        end handleUpstreamEvent
        def handleMappedEvents(
            event: MappedEvent
        ): StreamT.Step[B, StreamT[M, B]] =
          val index = event.index
          def replaceWithActiveStep(s: () => StreamT[M, B]) =
            val (left, oldStepM, right) =
              mappedTree.split1(_.size > index)
            val newTree = left.add1(
              Some(s().step),
              right
            )
            loop(upstreamEventQueueOption, newTree)
          end replaceWithActiveStep
          event.step match
            case Yield(b, s) =>
              Yield(
                b,
                () => CovariantStreamT.apply flip replaceWithActiveStep(s)
              )
            case Skip(s) =>
              Skip(() => CovariantStreamT.apply flip replaceWithActiveStep(s))
            case Done() =>
              Skip { () =>
                val (left, oldStepM, right) =
                  mappedTree.split1(_.size > index)
                val newTree = left.add1(
                  None,
                  right
                )
                CovariantStreamT.apply flip loop(
                  upstreamEventQueueOption,
                  newTree
                )
              }
        end handleMappedEvents
        val mappedEventBuilderOption =
          mappedTree.measure.toOption.flatMap(_.mappedEventBuilder)
        CovariantStreamT(upstreamEventQueueOption match
          case None =>
            mappedEventBuilderOption match
              case None =>
                StreamT.empty
              case Some(mappedEventBuilder) =>
                StreamT(M.map(mappedEventBuilder(0))(handleMappedEvents))
          case Some(upstreamEventQueue) =>
            mappedEventBuilderOption match
              case None =>
                StreamT(M.map(upstreamEventQueue) { upstreamEvent =>
                  handleUpstreamEvent(upstreamEvent)
                })
              case Some(mappedEventBuilder) =>
                StreamT(
                  M.map(M.choose(upstreamEventQueue, mappedEventBuilder(0))) {
                    case \/-((_, mappedEvent)) =>
                      handleMappedEvents(mappedEvent)
                    case -\/((upstreamEvent, _)) =>
                      handleUpstreamEvent(upstreamEvent)
                  }
                )
        )
      end loop
      loop(Some(upstream.step), FingerTree.empty)
    end mergeMap
    private def index(currentIndex: Int)(using
        M: Applicative[M]
    ): CovariantStreamT[M, Int] =
      CovariantStreamT(
        StreamT(
          M.pure(
            Yield(
              currentIndex,
              () => CovariantStreamT.apply flip indexTail(currentIndex)
            )
          )
        )
      )
    end index

    private def indexTail(currentIndex: Int)(using
        M: Applicative[M]
    ): CovariantStreamT[M, Int] =
      upstream.stepMap {
        case Yield(patch, tail) =>
          patch match
            case Patch.Splice(patchIndex, deleteCount, newItems) =>
              if currentIndex < patchIndex then
                Skip(() => PatchStreamT(tail()).indexTail(currentIndex))
              else if currentIndex < patchIndex + deleteCount then Done()
              else
                val newIndex = currentIndex - deleteCount + newItems.size
                Yield(newIndex, () => PatchStreamT(tail()).indexTail(newIndex))
            case Patch.ReplaceChildren(newItems) =>
              Done()
        case Skip(tail) =>
          Skip(() => PatchStreamT(tail()).indexTail(currentIndex))
        case Done() =>
          Done()
      }

    def zipWithIndex(using
        Applicative[M]
    ): PatchStreamT[M, (A, CovariantStreamT[M, Int])] =
      upstream.stepMap {
        case Yield(patch, tail) =>
          val mappedPatch = patch match
            case Patch.Splice(patchIndex, deleteCount, newItems) =>
              Patch.Splice(
                patchIndex,
                deleteCount,
                collection.View.ZipWithIndex(newItems).map {
                  case (item, relativeIndex) =>
                    (item, tail().index(patchIndex + relativeIndex))
                }
              )
            case Patch.ReplaceChildren(newItems) =>
              Patch.ReplaceChildren(collection.View.ZipWithIndex(newItems).map {
                case (item, relativeIndex) =>
                  (item, tail().index(relativeIndex))
              })
          Yield(
            mappedPatch,
            () => PatchStreamT(tail()).zipWithIndex
          )
        case Skip(tail) =>
          Skip(() => PatchStreamT(tail()).zipWithIndex)
        case Done() =>
          Done()
      }

    /** Return a [[CovariantStreamT]], whose each element is a
      * [[scalaz.FingerTree]] representing the snapshot of the sequence at that
      * time.
      *
      * @note
      *   The measurement of the [[scalaz.FingerTree]] is the size.
      * @example
      *   Given a [[PatchStreamT]] created from an iterable,
      *   {{{
      *   import scala.concurrent.Future
      *   import scalaz.std.scalaFuture.given
      *   val bindingSeq = PatchStreamT.fromIterable[Future, String](Seq("foo", "bar", "baz"))
      *   }}}
      *   when taking [[snapshots]],
      *   {{{
      *   val snapshots = bindingSeq.snapshots
      *   }}}
      *   then it should contains an empty value and an initial value
      *   {{{
      *   import com.thoughtworks.dsl.keywords.Await
      *   import com.thoughtworks.dsl.macros.Reset.Default.*
      *   `*`[Future] {
      *     val snapshotLazyList = !Await(snapshots.toLazyList)
      *     inside(snapshotLazyList) {
      *       case LazyList(empty, initial) =>
      *         empty.toList should be(Nil)
      *         initial.toList should be(List("foo", "bar", "baz"))
      *     }
      *   }
      *   }}}
      */
    def snapshots(using
        Applicative[M]
    ): CovariantStreamT[M, FingerTree[Int, A]] =
      upstream.scanLeft(Snapshot.empty) { (s, p) =>
        p.applyTo(s)
      }

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
      *   {{{
      *   import scala.concurrent.Future
      *   import scalaz.std.scalaFuture.given
      *   val bindingSeq = PatchStreamT.fromIterable[Future, String](Seq("foo", "bar"))
      *   }}}
      *   when flat-mapping it to more [[PatchStreamT]],
      *   {{{
      *   val flatten = bindingSeq.flatMap { s =>
      *     PatchStreamT.fromIterable(s)
      *   }
      *   }}}
      *   then it should returns a [[StreamT]] including each intermediate
      *   states during flat-mapping
      *   {{{
      *   import com.thoughtworks.dsl.keywords.Await
      *   import com.thoughtworks.dsl.macros.Reset.Default.*
      *   `*`[Future] {
      *     val snapshotLazyList = !Await(flatten.snapshots.toLazyList)
      *     snapshotLazyList.map(_.toList.mkString) should be(
      *       LazyList(
      *         "",
      *         "foo",
      *         "foobar",
      *       )
      *     )
      *   }
      *   }}}
      */
    def flatMap[B](f: A => PatchStreamT[M, B])(using
        M: Nondeterminism[M]
    ): PatchStreamT[M, B] =
      type EventStep[B] =
        StreamT.Step[Patch[B], scalaz.StreamT[M, Patch[B]]]
      final case class SliceEvent(eventStep: EventStep[B], sliceIndex: Int)
      final case class SliceMeasure(
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
      given Monoid[SliceMeasure] with
        def zero = SliceMeasure(0, 0, None)
        def append(f1: SliceMeasure, f2: => SliceMeasure) =
          val numberOfSlices1 = f1.numberOfSlices
          SliceMeasure(
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
              case (someBuilder1: Some[Int => M[SliceEvent]], None) =>
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
      given Reducer[Slice, SliceMeasure] = UnitReducer { (slice: Slice) =>
        SliceMeasure(
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

      def loop(
          upstreamEventQueueOption: Option[M[EventStep[A]]],
          sliceTree: FingerTree[SliceMeasure, Slice]
      ): StreamT[M, Patch[B]] =
        def handleUpstreamEvent(upstreamEvent: EventStep[A]): EventStep[B] =
          upstreamEvent match
            case Yield(
                  // Work around https://github.com/lampepfl/dotty/issues/13998
                  patch: Patch[A],
                  s
                ) =>
              patch match
                case Patch.ReplaceChildren(newItems) =>
                  val tail = { () =>
                    val newSliceTree =
                      newItems.foldLeft(FingerTree.empty) { (tree, a) =>
                        tree :+ ActiveSlice(0, f(a).step)
                      }
                    loop(Some(s().step), newSliceTree)
                  }
                  sliceTree.measure match
                    case Maybe.Just(measure) if measure.numberOfElements > 0 =>
                      Yield(Patch.ReplaceChildren(Nil), tail)
                    case _ =>
                      Skip(tail)
                case Patch.Splice(
                      patchIndex,
                      numberOfSlicesDeleted,
                      newItems
                    ) =>
                  val (left, notLeft) =
                    sliceTree.split(_.numberOfSlices > patchIndex)
                  val (deleted, right) =
                    notLeft.split(_.numberOfSlices > numberOfSlicesDeleted)
                  val SliceMeasure(`patchIndex`, mappedIndex, _) =
                    left.measureMonoid
                  val SliceMeasure(
                    `numberOfSlicesDeleted`,
                    numberOfItemsDeleted,
                    _
                  ) = deleted.measureMonoid
                  val tail = { () =>
                    val newSliceTree = newItems.foldLeft(left) { (tree, a) =>
                      tree :+ ActiveSlice(0, f(a).step)
                    } <++> right
                    loop(Some(s().step), newSliceTree)
                  }
                  if (numberOfItemsDeleted > 0) {
                    Yield(
                      Patch.Splice(mappedIndex, numberOfItemsDeleted, Nil),
                      tail
                    )
                  } else {
                    Skip(tail)
                  }
            case Skip(s) =>
              Skip { () =>
                loop(Some(s().step), sliceTree)
              }
            case Done() =>
              Skip { () =>
                loop(None, sliceTree)
              }
        end handleUpstreamEvent
        def handleSliceEvent(sliceEvent: SliceEvent): EventStep[B] =
          val sliceIndex = sliceEvent.sliceIndex
          val (left, oldSlice, right) =
            sliceTree.split1(_.numberOfSlices > sliceIndex)
          sliceEvent.eventStep match
            case Yield(patch, s) =>
              val patchWithOffset = patch match
                case splice: Patch.Splice[B] =>
                  left.measure match
                    case Maybe.Empty() =>
                      splice
                    case Maybe.Just(leftSliceMeasure) =>
                      splice.copy(index =
                        splice.index + leftSliceMeasure.numberOfElements
                      )
                case Patch.ReplaceChildren(newItems) =>
                  val offset = left.measure match
                    case Maybe.Empty() =>
                      0
                    case Maybe.Just(leftSliceMeasure) =>
                      leftSliceMeasure.numberOfElements
                  Patch.Splice(offset, oldSlice.size, newItems)
              Yield(
                patchWithOffset,
                { () =>
                  val newSlice =
                    ActiveSlice(patch.newSize(oldSlice.size), s().step)
                  val newSliceTree = left.add1(newSlice, right)
                  loop(upstreamEventQueueOption, newSliceTree)
                }
              )
            case Skip(s) =>
              Skip { () =>
                val newSliceTree = left.add1(
                  ActiveSlice(oldSlice.size, s().step),
                  right
                )
                loop(upstreamEventQueueOption, newSliceTree)
              }
            case Done() =>
              Skip { () =>
                val newSliceTree =
                  left.add1(InactiveSlice(oldSlice.size), right)
                loop(upstreamEventQueueOption, newSliceTree)
              }
        end handleSliceEvent
        val sliceEventBuilderOption =
          sliceTree.measure.toOption.flatMap(_.sliceEventBuilder)
        upstreamEventQueueOption match
          case None =>
            sliceEventBuilderOption match
              case None =>
                StreamT.empty
              case Some(sliceEventBuilder) =>
                StreamT(M.map(sliceEventBuilder(0))(handleSliceEvent))
          case Some(upstreamEventQueue) =>
            sliceEventBuilderOption match
              case None =>
                StreamT(M.map(upstreamEventQueue) { upstreamEvent =>
                  handleUpstreamEvent(upstreamEvent)
                })
              case Some(sliceEventBuilder) =>
                StreamT(
                  M.map(M.choose(upstreamEventQueue, sliceEventBuilder(0))) {
                    case \/-((_, sliceEvent)) =>
                      handleSliceEvent(sliceEvent)
                    case -\/((upstreamEvent, _)) =>
                      handleUpstreamEvent(upstreamEvent)
                  }
                )
      end loop
      CovariantStreamT(loop(Some(upstream.step), FingerTree.empty))
    end flatMap

  def fromIterableCovariantStreamT[M[_], A](
      stream: CovariantStreamT[M, Iterable[A]]
  )(using Functor[M]): PatchStreamT[M, A] =
    stream.map(Patch.ReplaceChildren(_))

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

  given [M[_], From, A](using
      M: Applicative[M],
      asIterable: From => Iterable[A]
  ): Dsl.Lift.OneStep[From, PatchStreamT[M, A]] = { from =>
    fromIterable(asIterable(from))
  }

  given [M[_], From, A](using
      M: Nondeterminism[M],
      asIterable: From => Iterable[CovariantStreamT[M, A]]
  ): Dsl.Lift.OneStep[From, PatchStreamT[M, A]] = { from =>
    fromIterable(asIterable(from)).flatMap(fromCovariantStreamT)
  }

  given [Keyword, M[_], Element, Value](using
      streamDsl: Dsl.Searching[Keyword, CovariantStreamT[M, PatchStreamT.Patch[
        Element
      ]], Value],
      M: Applicative[M]
  ): Dsl.Derived.StackSafe[Keyword, PatchStreamT[M, Element], Value] =
    ??? // TODO: Implement subject for BindingSeq
    // Dsl.Derived.StackSafe { (keyword, handler) =>
    //   streamDsl(
    //     keyword,
    //     { a =>
    //       PatchStreamT.Patch.ReplaceChildren(collection.View.Empty) #:: handler(
    //         a
    //       )
    //     }
    //   )
    // }

  private[PatchStreamT] trait LowPriority0:
    given [M[_], A, From](using
        toStream: From <:< CovariantStreamT[M, A],
        M: Functor[M]
    ): Dsl.Lift.OneStep[From, PatchStreamT[M, A]] = from =>
      PatchStreamT.fromCovariantStreamT(toStream(from))
