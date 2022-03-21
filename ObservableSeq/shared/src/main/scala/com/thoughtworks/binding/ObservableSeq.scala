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
import scalaz.ReaderT
import scalaz.Semigroup
import scala.concurrent.ExecutionContext
import scala.collection.immutable.Queue
import scala.collection.View

opaque type ObservableSeq[+A] <: Observable[ObservableSeq.Patch[A]] =
  Observable[ObservableSeq.Patch[A]]

object ObservableSeq:
  def apply[A]: Observable[ObservableSeq.Patch[A]] =:= ObservableSeq[A] = summon
  opaque type Operator[+A] <: ObservableSeq[A] &
    Observable.Operator[ObservableSeq.Patch[A]] =
    Observable.Operator[ObservableSeq.Patch[A]]
  end Operator
  object Operator:
    opaque type Empty <: ObservableSeq[Nothing] &
      Observable.Operator.Empty.type =
      Observable.Operator.Empty.type
    val Empty: Empty = Observable.Operator.Empty
    opaque type NonEmpty[+A] <: ObservableSeq.NonEmpty[A] & Operator[A] &
      Observable.Operator.NonEmpty[ObservableSeq.Patch[A]] =
      Observable.Operator.NonEmpty[ObservableSeq.Patch[A]]
    object NonEmpty:
      def apply[A]
          : Observable.Operator.NonEmpty[ObservableSeq.Patch[A]] =:= NonEmpty[
            A
          ] = summon
      opaque type Lazy[+A] <: NonEmpty[A] &
        Observable.Operator.NonEmpty.Lazy[ObservableSeq.Patch[A]] =
        Observable.Operator.NonEmpty.Lazy[ObservableSeq.Patch[A]]
      object Lazy:
        def apply[A]: Observable.Operator.NonEmpty.Lazy[
          ObservableSeq.Patch[A]
        ] =:= Lazy[A] = summon
      opaque type Eager[+A] <: NonEmpty[A] &
        Observable.Operator.NonEmpty.Eager[ObservableSeq.Patch[A]] =
        Observable.Operator.NonEmpty.Eager[ObservableSeq.Patch[A]]
      object Eager:
        def apply[A]: Observable.Operator.NonEmpty.Eager[
          ObservableSeq.Patch[A]
        ] =:= Eager[A] = summon
  val Empty = Operator.Empty
  opaque type NonEmpty[+A] <: ObservableSeq[A] &
    Observable.NonEmpty[ObservableSeq.Patch[A]] =
    Observable.NonEmpty[ObservableSeq.Patch[A]]
  object NonEmpty:
    def apply[A]: Observable.NonEmpty[ObservableSeq.Patch[A]] =:= NonEmpty[A] =
      summon
  end NonEmpty

  sealed trait Patch[+A]:
    private[binding] def newSize(oldSize: Int): Int
    private[binding] def applyTo[B >: A](
        snapshot: Patch.Snapshot[B]
    ): Patch.Snapshot[B]
  end Patch
  object Patch:
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
      end empty
    end Snapshot
    // TODO: Support move items
    // final case class Move[A](oldIndex: Int, offset: Int, moveCount: Int) extends Patch[A]
    final case class ReplaceChildren[+A](
        newItems: Iterable[A]
    ) extends Patch[A]:
      private[binding] def newSize(oldSize: Int) = newItems.size
      private[binding] def applyTo[B >: A](
          snapshot: Snapshot[B]
      ): Snapshot[B] =
        newItems.foldLeft(Snapshot.empty[B]) { (tree, a) =>
          tree :+ a
        }
    end ReplaceChildren

    final case class Splice[+A](
        index: Int,
        deleteCount: Int,
        newItems: Iterable[A]
    ) extends Patch[A]:
      private[binding] def newSize(oldSize: Int) =
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
    end Splice
  end Patch
  extension [A](observableSeq: ObservableSeq[A])
    def flatMap[B](f: A => ObservableSeq[B])(using
        ExecutionContext
    ): ObservableSeq[B] =
      type EventStep[B] = (Iterable[Patch[B]], ObservableSeq[B])
      final case class SliceEvent(eventStep: EventStep[B], sliceIndex: Int)
      final case class SliceMeasure(
          numberOfSlices: Int,
          numberOfElements: Int,
          sliceEventBuilder: Option[
            (numberOfPreviousSlices: Int) => Future[SliceEvent]
          ]
      )
      sealed trait Slice:
        def size: Int
      def sliceFromObservableSeq(size: Int, observableSeq: ObservableSeq[B]) =
        observableSeq match
          case nonEmpty: Observable.NonEmpty[Patch[B]] =>
            ActiveSlice(size, nonEmpty.next())
          case Observable.Empty =>
            InactiveSlice(size)
        end match
      end sliceFromObservableSeq
      final case class ActiveSlice(size: Int, eventQueue: Future[EventStep[B]])
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
              case (someBuilder1: Some[Int => Future[SliceEvent]], None) =>
                someBuilder1
              case (Some(builder1), Some(builder2)) =>
                Some(Memo.immutableHashMapMemo {
                  (numberOfPreviousSlices: Int) =>
                    Future.firstCompletedOf(
                      Seq(
                        builder1(numberOfPreviousSlices),
                        builder2(numberOfPreviousSlices + numberOfSlices1)
                      )
                    )(using ExecutionContext.parasitic)
                })
          )
        end append
      end given
      given Reducer[Slice, SliceMeasure] = UnitReducer { (slice: Slice) =>
        SliceMeasure(
          1,
          slice.size,
          slice match {
            case InactiveSlice(_) =>
              None
            case ActiveSlice(_, eventQueue) =>
              Some(Memo.immutableHashMapMemo { (numberOfPreviousSlices: Int) =>
                eventQueue.map(SliceEvent(_, numberOfPreviousSlices))(using
                  ExecutionContext.parasitic
                )
              })
          }
        )
      }
      end given

      def loop(
          upstreamEventQueue: ObservableSeq[A],
          sliceTree: FingerTree[SliceMeasure, Slice]
      ): ObservableSeq.Operator[B] =
        def handleUpstreamEvent(
            upstreamEvent: EventStep[A]
        ): (Iterable[Patch[B]], ObservableSeq.Operator[B]) =
          val (upstreamElements, upstreamTail) = upstreamEvent
          val (outputPatchQueue, newSliceTree) =
            upstreamElements.foldLeft((Queue.empty[Patch[B]], sliceTree)) {
              case (
                    (_, sliceTree),
                    Patch.ReplaceChildren(newItems)
                  ) =>
                val newSliceTree =
                  newItems.foldLeft(FingerTree.empty) { (tree, a) =>
                    tree :+ sliceFromObservableSeq(0, f(a))
                  }
                (Queue(Patch.ReplaceChildren(View.Empty)), newSliceTree)
              case (
                    (outputPatchQueue, sliceTree),
                    Patch.Splice(
                      patchIndex,
                      numberOfSlicesDeleted,
                      newItems
                    )
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
                val newSliceTree = newItems.foldLeft(left) { (tree, a) =>
                  tree :+ sliceFromObservableSeq(0, f(a))
                } <++> right

                val newOutputPatchQueue =
                  if numberOfItemsDeleted > 0 then
                    outputPatchQueue :+ Patch.Splice(
                      mappedIndex,
                      numberOfItemsDeleted,
                      Nil
                    )
                  else outputPatchQueue
                  end if
                (
                  newOutputPatchQueue,
                  newSliceTree
                )
            }
          (outputPatchQueue, loop(upstreamTail, newSliceTree))
        end handleUpstreamEvent
        def handleSliceEvent(
            sliceEvent: SliceEvent
        ): (Iterable[Patch[B]], ObservableSeq.Operator[B]) =
          val SliceEvent((patches, sliceTail), sliceIndex) = sliceEvent
          val (left, oldSlice, right) =
            sliceTree.split1(_.numberOfSlices > sliceIndex)
          val offset = left.measure match
            case Maybe.Empty() =>
              0
            case Maybe.Just(leftSliceMeasure) =>
              leftSliceMeasure.numberOfElements
          val (outputPatchQueue, newSliceSize) =
            patches.foldLeft((Queue.empty[Patch[B]], oldSlice.size)) {
              case (
                    (outputPatchQueue, sliceSize),
                    patch @ Patch.ReplaceChildren(newItems)
                  ) =>
                (
                  outputPatchQueue :+ Patch.Splice(offset, sliceSize, newItems),
                  patch.newSize(sliceSize)
                )
              case (
                    (outputPatchQueue, sliceSize),
                    patch: Patch.Splice[B]
                  ) =>
                (
                  outputPatchQueue :+ patch.copy(index = patch.index + offset),
                  patch.newSize(sliceSize)
                )
            }
          val newSlice = sliceFromObservableSeq(newSliceSize, sliceTail)
          val newSliceTree = left.add1(newSlice, right)
          (outputPatchQueue, loop(upstreamEventQueue, newSliceTree))
        end handleSliceEvent
        val sliceEventBuilderOption =
          sliceTree.measure.toOption.flatMap(_.sliceEventBuilder)

        upstreamEventQueue match
          case Observable.Empty =>
            sliceEventBuilderOption match
              case None =>
                Observable.Operator.Empty
              case Some(sliceEventBuilder) =>
                locally[Observable.Operator.NonEmpty.Lazy[Patch[B]]] { () =>
                  sliceEventBuilder(0).map(handleSliceEvent)
                }
          case nonEmpty: Observable.NonEmpty[Patch[A]] =>
            sliceEventBuilderOption match
              case None =>
                locally[Observable.Operator.NonEmpty.Lazy[Patch[B]]] { () =>
                  nonEmpty.next().map(handleUpstreamEvent)
                }
              case Some(sliceEventBuilder) =>
                locally[Observable.Operator.NonEmpty.Lazy[Patch[B]]] { () =>
                  Future
                    .firstCompletedOf(
                      Seq(
                        nonEmpty
                          .next()
                          .map(upstreamEvent =>
                            () => handleUpstreamEvent(upstreamEvent)
                          )(using ExecutionContext.parasitic),
                        sliceEventBuilder(0).map(sliceEvent =>
                          () => handleSliceEvent(sliceEvent)
                        )(using ExecutionContext.parasitic)
                      )
                    )(using ExecutionContext.parasitic)
                    .map(_())
                }
            end match
        end match
      end loop
      loop(observableSeq, FingerTree.empty)
    end flatMap
  end extension

end ObservableSeq
