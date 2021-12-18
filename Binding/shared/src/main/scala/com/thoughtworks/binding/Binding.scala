package com.thoughtworks.binding

import scalaz.-\/
import scalaz.Applicative
import scalaz.DList
import scalaz.Equal
import scalaz.FingerTree
import scalaz.Free
import scalaz.IList
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

object Binding:
  opaque type BindingT[M[_], +A] = StreamT[
    M,
    // Ideally StreamT should be covariant. Mark it as `@unchecked` as a workaround.
    A @uncheckedVariance
  ]
  object BindingT:

    def fromStreamT[M[_], A]: StreamT[
      M,
      // Ideally StreamT should be covariant. Mark it as `@unchecked` as a workaround.
      A @uncheckedVariance
    ]
      =:= BindingT[M, A] =
      summon

    extension [M[_], A](binding: BindingT[M, A])
      def mergeWith(that: BindingT[M, A])(using
          Nondeterminism[M]
      ): BindingT[M, A] =
        (binding: StreamT[M, A]).mergeWith(that: StreamT[M, A])
    def mergeView[M[_], A](streams: IndexedSeqView[BindingT[M, A]])(using
        Nondeterminism[M]
    ): BindingT[M, A] =
      streams.size match {
        case 0 =>
          StreamT.empty
        case 1 =>
          streams.head
        case size =>
          val middle = size / 2
          mergeView(streams.slice(0, middle))
            .mergeWith(mergeView(streams.slice(middle, size)))
      }

    def mergeAll[M[_], A](
        streams: Iterable[BindingT[M, A]]
    )(using Nondeterminism[M]): BindingT[M, A] =
      mergeView(streams match {
        case indexedSeqView: IndexedSeqView[
              BindingT[M, A] @unchecked
            ] =>
          indexedSeqView
        case indexedSeqOps: IndexedSeqView.SomeIndexedSeqOps[
              BindingT[M, A] @unchecked
            ] =>
          IndexedSeqView.Id(indexedSeqOps)
        case _ =>
          streams.toIndexedSeq.view
      })

    def apply[M[_]: Applicative, A](a: A): BindingT[M, A] = a :: StreamT.empty[M, A]
    given [M[_]](using M: Nondeterminism[M]): Monad[[X] =>> BindingT[M, X]] with
      def point[A](a: => A) = BindingT(a)
      def bind[A, B](upstream: BindingT[M, A])(
          f: A => BindingT[M, B]
      ): BindingT[M, B] =
        given [B]: Equal[B] = Equal.equalA[B]
        // We could avoid the `asInstanceOf` by wrapping `f(_)` like this:
        //   upstream.flatMapLatest(f(_)).distinctUntilChanged
        // but it will create an unnecessary function object.
        // Use `asInstanceOf` anyway
        upstream
          .flatMapLatest(f.asInstanceOf[A => StreamT[M, B]])
          .distinctUntilChanged
      override def map[A, B](upstream: BindingT[M, A])(
          f: A => B
      ): BindingT[M, B] =
        given Equal[B] = Equal.equalA[B]
        upstream.map(f).distinctUntilChanged

  opaque type BindingSeqT[M[_], +A] = BindingT[M, BindingSeqT.Patch[A]]
  object BindingSeqT:

    extension [M[_], A](bindingSeq: BindingSeqT[M, A])
      def mergeWithEventLoop(eventLoop: BindingT[M, Nothing])(using
          Nondeterminism[M]
      ): BindingSeqT[M, A] =
        bindingSeq.mergeWith(eventLoop)

    def fromBindingT[M[_], A]
        : BindingT[M, BindingSeqT.Patch[A]] =:= BindingSeqT[M, A] =
      summon

    def apply[M[_]: Applicative, A](elements: A*): BindingSeqT[M, A] =
      Patch.Splice[A](0, 0, elements) :: StreamT.empty
    sealed trait Patch[+A]:
      private[BindingSeqT] def withOffset(offset: Int): Patch[A]
      private[BindingSeqT] def sizeIncremental: Int

    object Patch:
      // TODO: Support move items
      // final case class Move[A](oldIndex: Int, offset: Int, moveCount: Int) extends Patch[A]
      final case class Splice[+A](
          index: Int,
          deleteCount: Int,
          newItems: Iterable[A]
      ) extends Patch[A]:
        private[BindingSeqT] def withOffset(offset: Int) =
          copy(index = index + offset)
        private[BindingSeqT] def sizeIncremental = newItems.size - deleteCount

    extension [M[_], A](upstream: BindingSeqT[M, A])
      /** Returns a new data-binding sequence by applying a function to all
        * elements of this sequence and using the elements of the resulting
        * collections.
        *
        * Whenever `upstream` or one of the subsequence changes, the result
        * sequence changes accordingly. The time complexity to update the result
        * sequence, when `upstream` changes, is `O(log(n) + c)`, where `n` is
        * the size of `upstream`, and `c` is number of elements in `upstream`,
        * mapped to mutable subsequences by `f`, of which the indices in
        * `upstream` are changing. For example, if the size of `upstream` is 10
        * and 4 elements of them mapped to mutable subsequences, when an element
        * is prepended to `upstream`, `c` is 4. However when an element is
        * appended to `upstream`, `c` is zero, because no element's index in
        * `upstream is changed.
        *
        * The time complexity to update the result sequence, when one of the
        * subsequence changes, is `O(log(n))`, where `n` is the size of
        * `upstream`.
        */
      def flatMap[B](f: A => BindingSeqT[M, B])(using
          M: Nondeterminism[M]
      ): BindingSeqT[M, B] =
        val toStepB = { (a: A) =>
          f(a).step
        }
        type EventStep[B] =
          StreamT.Step[Patch[B], scalaz.StreamT[M, Patch[B]]]
        final case class SliceEvent(eventStep: EventStep[B], sliceIndex: Int)
        final case class Measure(
            numberOfSlices: Int,
            numberOfElements: Int,
            // TODO: Create an opaque type implementation for DList for better performance
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
                Some(Memo.immutableHashMapMemo {
                  (numberOfPreviousSlices: Int) =>
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
                    Patch.Splice(
                      patchIndex,
                      numberOfSlicesDeleted,
                      newItems
                    )
                    // Work around https://github.com/lampepfl/dotty/issues/13998
                    : Patch[A],
                    s
                  ) =>
                // TODO: Avoid unnecessary FingerTree operations to optimize the performance
                val (left, notLeft) =
                  sliceTree.split(_.numberOfSlices < patchIndex)
                val (deleted, right) =
                  notLeft.split(_.numberOfSlices < numberOfSlicesDeleted)
                val Measure(`patchIndex`, mappedIndex, _) = left.measureMonoid
                val Measure(`numberOfSlicesDeleted`, numberOfItemsDeleted, _) =
                  deleted.measureMonoid
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
            val (left, ActiveSlice(numberOfElementsInActiveSlice, _), right) =
              sliceTree.split1(_.numberOfSlices < sliceIndex)
            sliceEvent.eventStep match
              case Yield(patch, s) =>
                val newSliceTree =
                  left.add1(
                    ActiveSlice(
                      numberOfElementsInActiveSlice + patch.sizeIncremental,
                      s().step
                    ),
                    right
                  )
                val Some(next) =
                  mergeEventQueue(upstreamEventQueueOption, newSliceTree)
                val patchWithOffset = left.measure
                  .map { leftMeasure =>
                    patch.withOffset(leftMeasure.numberOfElements)
                  }
                  .getOrElse(patch)
                Yield(patchWithOffset, () => StreamT(next))
              case Skip(s) =>
                val newSliceTree = left.add1(
                  ActiveSlice(numberOfElementsInActiveSlice, s().step),
                  right
                )
                val Some(next) =
                  mergeEventQueue(upstreamEventQueueOption, newSliceTree)
                Skip(() => StreamT(next))
              case Done() =>
                val newSliceTree = left <++> right
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

    given [M[_]](using M: Nondeterminism[M]): Monad[[X] =>> BindingSeqT[M, X]]
      with
      def point[A](a: => A): BindingSeqT[M, A] =
        BindingSeqT(a)

      def bind[A, B](upstream: BindingSeqT[M, A])(
          f: A => BindingSeqT[M, B]
      ): BindingSeqT[M, B] =
        upstream.flatMap(f)

  /** The data binding expression of a sequence, essentially an asynchronous
    * stream of patches describing how the sequence is changing.
    */
  type BindingSeq[+A] = BindingSeqT[Future, A]
  object BindingSeq:
    export BindingSeqT._

  export BindingT._

type Binding[+A] = Binding.BindingT[Future, A]
