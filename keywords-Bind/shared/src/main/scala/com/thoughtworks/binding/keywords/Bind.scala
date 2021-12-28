package com.thoughtworks.binding
package keywords
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.binding.CovariantStreamT
import com.thoughtworks.binding.StreamT.*
import scalaz.Equal
import scalaz.Nondeterminism
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
      given [B]: Equal[B] = Equal.equalA[B]
      keyword.flatMapLatest(handler).distinctUntilChanged
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
