package com.thoughtworks.binding
package keywords
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.binding.CovariantStreamT
import com.thoughtworks.binding.StreamT.*
import scalaz.{Bind => _, *}

opaque type Bind[M[_], A] <: Dsl.Keyword.Opaque =
  Dsl.Keyword.Opaque.Of[CovariantStreamT[M, A]]

object Bind {
  def apply[M[_], A]: CovariantStreamT[M, A] =:= Bind[M, A] =
    Dsl.Keyword.Opaque.Of.apply
  given [M[_], A]: Dsl.IsKeyword[Bind[M, A], A] with {}

  given [M[_], A, B](using
      Nondeterminism[M]
  ): Dsl.Original[Bind[M, A], CovariantStreamT[M, B], A] = Dsl.Original {
    (keyword, handler) =>
      @inline given [B]: Equal[B] = Equal.equalA[B]
      keyword.flatMapLatest(handler).distinctUntilChanged
  }

  given [F[_[_], _], M[_], A, B](using
      Nondeterminism[F[M, _]],
      MonadTrans[F],
      Monad[M]
  ): Dsl.Original[Bind[M, A], CovariantStreamT[F[M, *], B], A] = Dsl.Original {
    (keyword, handler) =>
      @inline given [B]: Equal[B] = Equal.equalA[B]
      Bind.apply
        .flip(keyword)
        .trans(new (M ~> F[M, *]) {
          def apply[A](ma: M[A]) = MonadTrans[F].liftM(ma)
        })
        .flatMapLatest(handler)
        .distinctUntilChanged
  }

  /** @note
    *   We need this instance because the generic instance for monad transformer
    *   does not work for ReaderT / Kleisli, as Kleisli's type parameter order
    *   is incompatible with partial unification
    */
  given [E, M[_], A, B](using
      Nondeterminism[M],
      Nondeterminism[Kleisli[M, E, *]]
  ): Dsl.Original[Bind[M, A], CovariantStreamT[Kleisli[M, E, *], B], A] =
    Dsl.Original { (keyword, handler) =>
      @inline given [B]: Equal[B] = Equal.equalA[B]
      Bind.apply
        .flip(keyword)
        .trans(new (M ~> Kleisli[M, E, *]) {
          def apply[A](ma: M[A]) = Kleisli(_ => ma)
        })
        .flatMapLatest(handler)
        .distinctUntilChanged
    }

  extension [FA, M[_], A](inline fa: FA)(using
      inline notKeyword: util.NotGiven[
        FA <:< Dsl.Keyword
      ],
      inline asFA: FA <:< CovariantStreamT[M, A]
  )
    transparent inline def unary_! : A =
      Dsl.shift(Bind(asFA(fa))): A
    transparent inline def bind: A =
      Dsl.shift(Bind(asFA(fa))): A

}
