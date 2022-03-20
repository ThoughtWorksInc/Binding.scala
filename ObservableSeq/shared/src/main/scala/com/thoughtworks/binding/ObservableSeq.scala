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

end ObservableSeq
