package com.thoughtworks.binding

import scalaz.-\/
import scalaz.Applicative
import scalaz.Equal
import scalaz.FingerTree
import scalaz.Functor
import scalaz.ICons
import scalaz.IList
import scalaz.INil
import scalaz.Isomorphism.IsoFunctor
import scalaz.Isomorphism.IsoFunctorTemplate
import scalaz.IsomorphismMonadPlus
import scalaz.Monad
import scalaz.MonadPlus
import scalaz.Monoid
import scalaz.Nondeterminism
import scalaz.StreamT
import scalaz.StreamT.Done
import scalaz.StreamT.Skip
import scalaz.StreamT.Step
import scalaz.StreamT.Yield
import scalaz.\/-
import scalaz.ReaderT
import scalaz.Reducer

import scala.concurrent.Future
private[binding] type StreamT[M[_], A] = scalaz.StreamT[M, A]
private[binding] object StreamT:
  export scalaz.StreamT.*

  extension [M[_], A](fa: StreamT[M, A])
    private def stepMap[B](f: Step[A, StreamT[M, A]] => Step[B, StreamT[M, B]])(
        implicit M: Functor[M]
    ): StreamT[M, B] = StreamT(M.map(fa.step)(f))

    // Copied from https://github.com/scalaz/scalaz/pull/2250
    private def scanLeftTail[B](
        head: B
    )(op: (B, A) => B)(implicit M: Functor[M]): StreamT[M, B] =
      stepMap[B] {
        case Yield(a, s) =>
          val next = op(head, a)
          Yield(next, () => s().scanLeftTail(next)(op))
        case Skip(s) =>
          Skip(() => s().scanLeftTail(head)(op))
        case Done() =>
          Done()
      }

    /** Produces a [[StreamT]] containing cumulative results of applying the
      * operator going left to right, including the initial value.
      *
      * @tparam B
      *   the type of the elements in the resulting collection
      * @param head
      *   the initial value
      * @param op
      *   the binary operator applied to the intermediate result and the element
      * @return
      *   collection with intermediate results
      */
    def scanLeft[B](head: B)(op: (B, A) => B)(implicit
        M: Applicative[M]
    ): StreamT[M, B] =
      head :: scanLeftTail(head)(op)

    /** Computes a prefix scan of the elements of the collection.
      *
      * Note: The neutral element `B` may be applied more than once.
      *
      * @tparam B
      *   element type of the resulting collection
      * @param op
      *   the associative operator for the scan
      *
      * @return
      *   a new [[StreamT]] containing the prefix scan of the elements in this
      *   $coll
      */
    def scan[B >: A](op: (B, B) => B)(implicit M: Functor[M]): StreamT[M, B] =
      stepMap[B] {
        case Yield(a, s) =>
          Yield(a, () => s().scanLeftTail[B](a)(op))
        case Skip(s) =>
          Skip(() => s().scan(op))
        case Done() =>
          Done()
      }

    // Copied from https://github.com/scalaz/scalaz/pull/2231
    /** The [[flatMapLatest]] operator behaves much like the [[mergeMap]] except
      * that whenever a new item is emitted by the source [[StreamT]], it will
      * not subscribe to and stop mirroring the [[StreamT]] that was generated
      * from the previously-emitted item, and begin only mirroring the current
      * one.
      *
      * <img style="max-width: 100%"
      * src="http://reactivex.io/documentation/operators/images/flatMapLatest.png"/>
      */
    def flatMapLatest[B](
        f: A => StreamT[M, B]
    )(implicit M: Nondeterminism[M]): StreamT[M, B] = {
      def flatMapLatestInitStep(
          fsa: M[Step[A, StreamT[M, A]]]
      ): M[Step[B, StreamT[M, B]]] = {
        M.map(fsa) {
          case Yield(a, s) =>
            Skip(() => StreamT(flatMapLatestStep(s().step, f(a).step)))
          case Skip(s) =>
            Skip(() => StreamT(flatMapLatestInitStep(s().step)))
          case Done() =>
            Done()
        }
      }
      def flatMapLatestStep(
          fsa: M[Step[A, StreamT[M, A]]],
          fsb: M[Step[B, StreamT[M, B]]]
      ): M[Step[B, StreamT[M, B]]] = {
        M.map(M.choose(fsa, fsb)) {
          case -\/((sa, fsb)) =>
            sa match {
              case Yield(a, s) =>
                Skip(() => StreamT(flatMapLatestStep(s().step, f(a).step)))
              case Skip(s) =>
                Skip(() => StreamT(flatMapLatestStep(s().step, fsb)))
              case Done() =>
                Skip(() => StreamT(fsb))
            }
          case \/-((fsa, sb)) =>
            sb match {
              case Yield(b, s) =>
                Yield(b, () => StreamT(flatMapLatestStep(fsa, s().step)))
              case Skip(s) =>
                Skip(() => StreamT(flatMapLatestStep(fsa, s().step)))
              case Done() =>
                Skip(() => StreamT(flatMapLatestInitStep(fsa)))
            }
        }
      }
      StreamT(flatMapLatestInitStep(fa.step))
    }
