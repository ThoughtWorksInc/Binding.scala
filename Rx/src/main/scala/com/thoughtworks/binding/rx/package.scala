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

type ObservableT[F[_], A] = StreamT[F, A]
type Observable[A] = ObservableT[Future, A]
type ConcatT[F[_], A] = ObservableT[F, A]
type Concat[A] = ConcatT[Future, A]
opaque type MergeT[F[_], A] >: ObservableT[F, A] <: ObservableT[F, A] = ObservableT[F, A]
type Merge[A] = MergeT[Future, A]

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

object MergeT:
  given [F[_]](using F: Nondeterminism[F]): Monad[[A] =>> MergeT[F, A]] with
    import F.nondeterminismSyntax._
    def point[A](a: => A): MergeT[F, A] =
      StreamT.StreamTMonadPlus(F).point(a)

    def bind[A, B](fa: MergeT[F, A])(f: A => MergeT[F, B]): MergeT[F, B] =
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
        fStepA.flatMap {
          case Yield(a, next) =>
            livingSteps(next().step, f(a).step, IList.empty)
          case Skip(next) =>
            initSteps(next().step)
          case Done() =>
            F.pure(Done())
        }
      StreamT(initSteps(fa.step))
