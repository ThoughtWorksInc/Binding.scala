package com.thoughtworks.binding

import scalaz.~>
import scalaz.-\/
import scalaz.Applicative
import scalaz.DList
import scalaz.Equal
import scalaz.FingerTree
import scalaz.Free
import scalaz.Hoist
import scalaz.IList
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
import scalaz.Functor
import com.thoughtworks.dsl.keywords.Suspend
import com.thoughtworks.dsl.Dsl

// Ideally StreamT should be covariant. Mark it as `@uncheckedVariance` as a workaround.
opaque type CovariantStreamT[M[_], +A] = StreamT[
  M,
  A @uncheckedVariance
]
object CovariantStreamT:
  given Hoist[CovariantStreamT] = StreamT.StreamTHoist
  given [F[_]](using Applicative[F]): MonadPlus[CovariantStreamT[F, _]] =
    StreamT.StreamTMonadPlus

  def apply[M[_], A]: StreamT[M, A] =:= CovariantStreamT[M, A] =
    summon

  extension [A](head: A)
    def ::[M[_]](binding: CovariantStreamT[M, A])(implicit
        M: Applicative[M]
    ): CovariantStreamT[M, A] =
      binding.::(head)

  extension [A](head: => A)
    def #::[M[_]](binding: => CovariantStreamT[M, A])(implicit
        M: Applicative[M]
    ): CovariantStreamT[M, A] =
      StreamT.#::(binding)(head)

  extension [M[_], A](binding: CovariantStreamT[M, A])
    def toLazyList(using Monad[M]): M[LazyList[A]] = binding.toLazyList
    def collect[B](pf: PartialFunction[A, B])(using
        Functor[M]
    ): CovariantStreamT[M, B] =
      binding.collect(pf)
    def filter(p: A => Boolean)(using Functor[M]): CovariantStreamT[M, A] =
      binding.filter(p)
    def headOption(using Monad[M]): M[Option[A]] = binding.headOption
    def step = binding.step
    private[binding] def stepMap[B](
        f: Step[A, CovariantStreamT[M, A]] => Step[B, CovariantStreamT[M, B]]
    )(using Functor[M]): CovariantStreamT[M, B] =
      StreamT.stepMap(binding)(f)
    def scanLeft[B](head: B)(op: (B, A) => B)(using
        Applicative[M]
    ): CovariantStreamT[M, B] =
      binding.scanLeft(head)(op)

    def map[B](f: A => B)(implicit m: Functor[M]): CovariantStreamT[M, B] =
      binding.map(f)

    def memoize(using Functor[M]): CovariantStreamT[M, A] =
      StreamT.memoize(binding)

    def noSkip(using Monad[M]): CovariantStreamT[M, A] =
      StreamT.noSkip(binding)

    def flatMapLatest[B](f: A => CovariantStreamT[M, B])(using
        Nondeterminism[M]
    ): CovariantStreamT[M, B] =
      binding.flatMapLatest(f)

    def distinctUntilChanged(using
        Functor[M],
        Equal[A]
    ): CovariantStreamT[M, A] =
      binding.distinctUntilChanged

    def trans[N[_]](t: M ~> N)(using Functor[M]): CovariantStreamT[N, A] =
      binding.trans(t)

    def mergeWith(that: => CovariantStreamT[M, A])(using
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

  def empty[M[_], A](implicit M: Applicative[M]): CovariantStreamT[M, A] =
    StreamT.empty
  def pure[M[_], A](a: A)(using Applicative[M]): CovariantStreamT[M, A] =
    a :: empty[M, A]

  // It should be Applicative[M] once the PR get merged: https://github.com/scalaz/scalaz/pull/2251
  given [M[_], A](using
      M: Monad[M]
  ): Dsl.Lift.OneStep[M[A], CovariantStreamT[M, A]] =
    StreamT.StreamTHoist.liftM(_)

  given [Keyword, Functor[_], Domain, Value](using
      dsl: Dsl.Searching[Keyword, CovariantStreamT[Functor, Domain], Value],
      applicative: Applicative[Functor]
  ): Dsl.Composed[Suspend[Keyword], CovariantStreamT[Functor, Domain], Value] =
    Dsl.Composed {
      (
          keyword: Suspend[Keyword],
          handler: Value => CovariantStreamT[Functor, Domain]
      ) =>
        CovariantStreamT(
          StreamT(
            applicative.pure(
              Skip(() => dsl(Suspend.apply.flip(keyword)(), handler))
            )
          )
        )
    }
