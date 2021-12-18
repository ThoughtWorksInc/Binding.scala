package com.thoughtworks.binding

import scalaz.Applicative

opaque type BindableSeq[-From, +Element] <: From => Binding.BindingSeq[
  Element
] = From => Binding.BindingSeq[Element]

object BindableSeq:

  def apply[From, Element]
      : (From => Binding.BindingSeq[Element]) =:= BindableSeq[From, Element] =
    summon
  given [Element](using
      Applicative[Binding.Awaitable]
  ): BindableSeq[Element, Element] = Binding.Constants(_)

  given [Keyword, Element, Value](using
      Dsl.IsKeyword[Keyword, Value],
      Dsl.Run[Keyword, BindingSeq[Element], Value]
  ): BindableSeq[Keyword, Element] =
    summon[Dsl.Run[Keyword, BindingSeq[Element], Value]]
