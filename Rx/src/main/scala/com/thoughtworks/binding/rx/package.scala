package com.thoughtworks.binding.rx

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

type Observable[A] = StreamT[Future, A]

object Observable:
  private def terminatingChooseSteps[F[_], B](
      chooseB: F[(Step[B, StreamT[F, B]], IList[F[Step[B, StreamT[F, B]]]])]
  )(using F: Nondeterminism[F]): F[Step[B, StreamT[F, B]]] =
    F.bind(chooseB) {
      case (Skip(next), fStepBs) =>
        terminatingChooseSteps(F.chooseAny(next().step, fStepBs))
      case (Yield(b, next), fStepBs) =>
        F.pure(Yield(b, () => StreamT(terminatingChooseSteps(F.chooseAny(next().step, fStepBs)))))
      case (Done(), ICons(fStepBHead, fStepBTail)) =>
        terminatingChooseSteps(F.chooseAny(fStepBHead, fStepBTail))
      case (Done(), INil()) =>
        F.pure(Done())
    }

  private def terminatingSteps[F[_], B](
      fStepBHead: F[Step[B, StreamT[F, B]]],
      fStepBTail: IList[F[Step[B, StreamT[F, B]]]]
  )(using F: Nondeterminism[F]): F[Step[B, StreamT[F, B]]] =
    terminatingChooseSteps(F.chooseAny(fStepBHead, fStepBTail))

  private def distinctUntilChangedFromLatestValue[F[_], A](fa: StreamT[F, A], latest: A)(using
      F: Functor[F]
  ): StreamT[F, A] =
    StreamT(F.map(fa.step) {
      case Skip(next) =>
        Skip(distinctUntilChangedFromLatestValue(next(), latest))
      case Done() =>
        Done()
      case Yield(`latest`, next) =>
        Skip(distinctUntilChangedFromLatestValue(next(), latest))
      case Yield(a, next) =>
        Yield(a, distinctUntilChangedFromLatestValue(next(), a))
    })

  extension [F[_], A](fa: StreamT[F, A])
    def distinctUntilChanged(using F: Functor[F]): StreamT[F, A] =
      StreamT(F.map(fa.step) {
        case Skip(next) =>
          Skip(next().distinctUntilChanged)
        case Done() =>
          Done()
        case Yield(a, next) =>
          Yield(a, distinctUntilChangedFromLatestValue(next(), a))
      })

    def mergeFlatMap[B](f: A => StreamT[F, B])(using F: Nondeterminism[F]): StreamT[F, B] =
      def livingSteps(
          fStepA: F[Step[A, StreamT[F, A]]],
          fStepBHead: F[Step[B, StreamT[F, B]]],
          fStepBTail: IList[F[Step[B, StreamT[F, B]]]]
      ): F[Step[B, StreamT[F, B]]] =
        def chooseSteps(
            fStepA: F[Step[A, StreamT[F, A]]],
            chooseB: F[(Step[B, StreamT[F, B]], IList[F[Step[B, StreamT[F, B]]]])]
        ): F[Step[B, StreamT[F, B]]] =
          F.bind(F.choose(fStepA, chooseB)) {
            case stepA -\/ chooseB =>
              stepA match
                case Yield(a, next) =>
                  livingSteps(next().step, f(a).step, ICons(fStepBHead, fStepBTail))
                case Skip(next) =>
                  chooseSteps(next().step, chooseB)
                case Done() =>
                  terminatingChooseSteps(chooseB)
            case fStepA \/- stepB =>
              stepB match
                case (Skip(next), fStepBs) =>
                  livingSteps(fStepA, next().step, fStepBTail)
                case (Yield(b, next), fStepBs) =>
                  F.pure(Yield(b, () => StreamT(livingSteps(fStepA, fStepBHead, fStepBTail))))
                case (Done(), ICons(fStepBHead, fStepBTail)) =>
                  livingSteps(fStepA, fStepBHead, fStepBTail)
                case (Done(), INil()) =>
                  StreamT.StreamTMonadPlus.bind(StreamT(fStepA))(f).step
          }
        chooseSteps(fStepA, F.chooseAny(fStepBHead, fStepBTail))
      def initSteps(fStepA: F[Step[A, StreamT[F, A]]]): F[Step[B, StreamT[F, B]]] =
        F.bind(fStepA) {
          case Yield(a, next) =>
            livingSteps(next().step, f(a).step, IList.empty)
          case Skip(next) =>
            initSteps(next().step)
          case Done() =>
            F.pure(Done())
        }
      StreamT(initSteps(fa.step))

opaque type MergeT[F[_], A] >: StreamT[F, A] <: StreamT[F, A] = StreamT[F, A]

opaque type DistinctUntilChangedT[F[_], A] <: StreamT[F, A] = StreamT[F, A]

object DistinctUntilChangedT:
  given [F[_]](using F: Functor[F]): IsomorphismMonadPlus[[A] =>> DistinctUntilChangedT[F, A], [A] =>> StreamT[F, A]]
    with
    def G = summon
    def iso = new IsoFunctorTemplate[[A] =>> DistinctUntilChangedT[F, A], [A] =>> StreamT[F, A]] {
      def to[A](distinct: DistinctUntilChangedT[F, A]) = distinct
      def from[A](stream: StreamT[F, A]) = Observable.distinctUntilChanged(stream)
    }

object MergeT:
  given [F[_]](using F: Nondeterminism[F]): Monad[[A] =>> MergeT[F, A]] with
    import F.nondeterminismSyntax._
    def point[A](a: => A): MergeT[F, A] =
      StreamT.StreamTMonadPlus(F).point(a)
    def bind[A, B](fa: MergeT[F, A])(f: A => MergeT[F, B]): MergeT[F, B] = {
      // import
      Observable.mergeFlatMap(fa)(f)
    }
