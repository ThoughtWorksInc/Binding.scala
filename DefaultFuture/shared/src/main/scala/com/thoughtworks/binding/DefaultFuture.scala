package com.thoughtworks.binding

import scala.concurrent.*
import scalaz.*

/** A [[scala.concurrent.Future]] whose scalaz instances are not orphan */
opaque type DefaultFuture[+A] >: Future[A] <: Future[A] = Future[A]
object DefaultFuture:
  export scalaz.std.scalaFuture.*

  /** @note
    *   This instance is a polyfill of
    *   https://github.com/scalaz/scalaz/pull/2262
    */
  given [R](using
      ExecutionContext
  ): Nondeterminism[Kleisli[DefaultFuture, R, *]] =
    new KleisliNondeterminism[DefaultFuture, R]:
      def F = Nondeterminism[DefaultFuture]

  private trait KleisliFunctor[F[_], R] extends Functor[Kleisli[F, R, *]] {
    implicit def F: Functor[F]
    override def map[A, B](fa: Kleisli[F, R, A])(f: A => B): Kleisli[F, R, B] =
      fa map f
  }

  private trait KleisliApply[F[_], R]
      extends Apply[Kleisli[F, R, *]]
      with KleisliFunctor[F, R] {
    implicit def F: Apply[F]
    override def ap[A, B](fa: => Kleisli[F, R, A])(
        f: => Kleisli[F, R, A => B]
    ): Kleisli[F, R, B] =
      Kleisli[F, R, B](r => F.ap(fa(r))(f(r)))
  }

  private trait KleisliBind[F[_], R]
      extends Bind[Kleisli[F, R, *]]
      with KleisliApply[F, R] {
    implicit def F: Bind[F]
    override final def bind[A, B](fa: Kleisli[F, R, A])(
        f: A => Kleisli[F, R, B]
    ) =
      fa flatMap f
  }

  private trait KleisliApplicative[F[_], R]
      extends Applicative[Kleisli[F, R, *]]
      with KleisliApply[F, R] {
    implicit def F: Applicative[F]
    def point[A](a: => A): Kleisli[F, R, A] =
      Kleisli((r: R) => F.point(a))
  }

  private trait KleisliMonad[F[_], R]
      extends Monad[Kleisli[F, R, *]]
      with KleisliApplicative[F, R]
      with KleisliBind[F, R] {
    implicit def F: Monad[F]
  }
  private trait KleisliNondeterminism[F[_], R]
      extends Nondeterminism[Kleisli[F, R, *]]
      with KleisliMonad[F, R] {
    implicit def F: Nondeterminism[F]

    def chooseAny[A](
        head: Kleisli[F, R, A],
        tail: IList[Kleisli[F, R, A]]
    ): Kleisli[F, R, (A, IList[Kleisli[F, R, A]])] =
      Kleisli { r =>
        F.map(F.chooseAny(head(r), tail.map(_(r)))) {
          case (chosen, notChosen) =>
            (chosen, notChosen.map(fa => Kleisli(_ => fa)))
        }
      }

  }
