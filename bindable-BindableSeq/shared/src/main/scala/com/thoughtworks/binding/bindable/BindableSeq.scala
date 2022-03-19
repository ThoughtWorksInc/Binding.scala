package com.thoughtworks.binding
package bindable

import com.thoughtworks.dsl.Dsl
import LegacyBinding.BindingSeq
import scalaz.Applicative
import scalaz.Functor

opaque type BindableSeq[-From, +Element] <: From => LegacyBinding.BindingSeq[
  Element
] = From => LegacyBinding.BindingSeq[Element]

object BindableSeq extends JSBindableSeq:

  def apply[From, Element]
      : (From => LegacyBinding.BindingSeq[Element]) =:= BindableSeq[From, Element] =
    summon
  given [Element](using
      Applicative[DefaultFuture]
  ): BindableSeq[Element, Element] = LegacyBinding.Constants(_)

  given [Keyword, Element, Value](using
      Dsl.Run[Keyword, BindingSeq[Element], Value]
  ): BindableSeq[Keyword, Element] =
    summon[Dsl.Run[Keyword, BindingSeq[Element], Value]]

  given [Element, Value]: BindableSeq[LegacyBinding.BindingSeq[Element], Element] =
    identity

  given [Element](using
      Functor[DefaultFuture]
  ): BindableSeq[LegacyBinding[Element], Element] = BindableSeq(
    BindingSeq.fromCovariantStreamT
  )
