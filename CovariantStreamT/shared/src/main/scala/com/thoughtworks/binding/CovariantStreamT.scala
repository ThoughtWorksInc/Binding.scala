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
import scalaz.Functor
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.binding.StreamT.*

// Ideally StreamT should be covariant. Mark it as `@unchecked` as a workaround.
opaque type CovariantStreamT[M[_], +A] >: StreamT[
  M,
  A @uncheckedVariance
] <: StreamT[
  M,
  A @uncheckedVariance
] = StreamT[
  M,
  A @uncheckedVariance
]
object CovariantStreamT:
  export com.thoughtworks.binding.StreamT.{runStreamT, scanLeft, apply}

  def apply[M[_], A]: StreamT[M, A] =:= CovariantStreamT[M, A] =
    summon

  extension [M[_], A](binding: CovariantStreamT[M, A])
    // Polyfill of https://github.com/scalaz/scalaz/pull/2249
    def collect[B](
        pf: PartialFunction[A, B]
    )(using M: Functor[M]): CovariantStreamT[M, B] =
      StreamT(M.map(binding.step) {
        case Yield(pf(b), s) =>
          Yield(b, () => s().collect(pf))
        case Yield(_, s) =>
          Skip(() => s().collect(pf))
        case Skip(s) =>
          Skip(() => s().collect(pf))
        case Done() =>
          Done()
      })

    def mergeWith(that: CovariantStreamT[M, A])(using
        Nondeterminism[M]
    ): CovariantStreamT[M, A] =
      (binding: StreamT[M, A]).mergeWith(that: StreamT[M, A])

  def mergeAll[M[_], A](
      streams: Iterable[CovariantStreamT[M, A]]
  )(using Nondeterminism[M]): CovariantStreamT[M, A] =
    if streams.isEmpty then StreamT.empty
    val indexedSeqOps = streams match {
      case indexedSeqOps: IndexedSeqView.SomeIndexedSeqOps[
            CovariantStreamT[M, A] @unchecked
          ] =>
        indexedSeqOps
      case _ =>
        streams.toIndexedSeq
    }
    def mergeView(begin: Int, end: Int): CovariantStreamT[M, A] =
      if begin + 1 == end then indexedSeqOps(begin)
      else
        val middle = (begin + end) / 2
        mergeView(begin, middle).mergeWith(mergeView(middle, end))
    mergeView(0, indexedSeqOps.length)

  def pure[M[_], A](a: A)(using Applicative[M]) = a :: StreamT.empty[M, A]

  given [M[_]](using
      M: Nondeterminism[M]
  ): Monad[[X] =>> CovariantStreamT[M, X]] with
    def point[A](a: => A) = CovariantStreamT.pure(a)
    def bind[A, B](upstream: CovariantStreamT[M, A])(
        f: A => CovariantStreamT[M, B]
    ): CovariantStreamT[M, B] =
      given [B]: Equal[B] = Equal.equalA[B]
      upstream.flatMapLatest(f).distinctUntilChanged
    override def map[A, B](upstream: CovariantStreamT[M, A])(
        f: A => B
    ): CovariantStreamT[M, B] =
      given Equal[B] = Equal.equalA[B]
      upstream.map(f).distinctUntilChanged

  // It should be Applicative[M] once the PR get merged: https://github.com/scalaz/scalaz/pull/2251
  given [M[_], A](using
      M: Monad[M]
  ): Dsl.Lift.OneStep[M[A], CovariantStreamT[M, A]] =
    StreamT.StreamTHoist.liftM(_)
