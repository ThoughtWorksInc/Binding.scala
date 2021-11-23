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

import scala.concurrent.Future

// Copied from https://github.com/scalaz/scalaz/pull/2234
extension [V,A](tree: FingerTree[V, A])
  private def measureMonoid(implicit V: Monoid[V]): V =
    tree.fold(V.zero, (v, _) => v, (v, _, _, _) => v)

// Copied from https://github.com/scalaz/scalaz/pull/2231
/** The [[flatMapLatest]] operator behaves much like the [[mergeMap]] except that whenever a new item is emitted by the
  * source [[StreamT]], it will not subscribe to and stop mirroring the [[StreamT]] that was generated from the
  * previously-emitted item, and begin only mirroring the current one.
  *
  * <img style="max-width: 100%" src="http://reactivex.io/documentation/operators/images/flatMapLatest.png"/>
  */
extension [M[_], A](fa: StreamT[M, A])
  private def flatMapLatest[B](f: A => StreamT[M, B])(implicit M: Nondeterminism[M]): StreamT[M, B] = {
    def flatMapLatestInitStep(fsa: M[Step[A, StreamT[M, A]]]): M[Step[B, StreamT[M, B]]] = {
      M.map(fsa) {
        case Yield(a, s) =>
          Skip(() => StreamT(flatMapLatestStep(s().step, f(a).step)))
        case Skip(s) =>
          Skip(() => StreamT(flatMapLatestInitStep(s().step)))
        case Done() =>
          Done()
      }
    }
    def flatMapLatestStep(fsa: M[Step[A, StreamT[M, A]]], fsb: M[Step[B, StreamT[M, B]]]): M[Step[B, StreamT[M, B]]] = {
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
