package com.thoughtworks.binding

import scalaz.StreamT
import scala.concurrent.Future
import scalaz.Monad
import scalaz.Nondeterminism
import scalaz.Applicative
import scalaz.StreamT.Skip
import scalaz.StreamT.Done
import scalaz.StreamT.Yield
import scalaz.StreamT.Step
import scalaz.IList
import scalaz.-\/
import scalaz.\/-
import scalaz.Monoid
import scalaz.ICons
import scalaz.INil
import scalaz.Isomorphism.IsoFunctor
import scalaz.Isomorphism.IsoFunctorTemplate
import scalaz.MonadPlus
import scalaz.IsomorphismMonadPlus
import scalaz.Functor
import scalaz.Equal

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

opaque type BindingT[M[_], A] <: StreamT[M, A] = StreamT[M, A]

object BindingT:
  given [M[_]](using M: Nondeterminism[M]): Monad[[A] =>> BindingT[M, A]] with
    def point[A](a: => A) = StreamT.StreamTMonadPlus.point(a)
    def bind[A, B](fa: BindingT[M, A])(f: A => BindingT[M, B]): BindingT[M, B] =
      given Equal[B] = Equal.equalA[B]
      fa.flatMapLatest(f).distinctUntilChanged
    override def map[A, B](fa: BindingT[M, A])(f: A => B): BindingT[M, B] =
      given Equal[B] = Equal.equalA[B]
      fa.map(f).distinctUntilChanged
