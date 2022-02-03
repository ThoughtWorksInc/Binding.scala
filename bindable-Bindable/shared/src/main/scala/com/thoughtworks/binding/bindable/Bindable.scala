package com.thoughtworks.binding
package bindable

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.keywords.FlatMap
import com.thoughtworks.dsl.keywords.Pure
import com.thoughtworks.dsl.keywords.Typed
import scalaz.Applicative
import scalaz.Functor

opaque type Bindable[-From, +Element] <: From => Binding[
  Element
] = From => Binding[Element]

object Bindable extends Bindable.LowPriority0:

  def apply[From, Element]
      : (From => Binding[Element]) =:= Bindable[From, Element] =
    summon

  private[Bindable] trait LowPriority0:
    given [Keyword, Element, Value](using
        converter: Value => Element,
        run: Dsl.Run[FlatMap[Keyword, Pure[Element]], Binding[Element], Element]
    ): Bindable[Typed[Keyword, Value], Element] = { typed =>
      run(FlatMap(Typed.apply.flip(typed), Pure.apply.liftCo(converter)))
    }

  given [Element](using
      Applicative[DefaultFuture]
  ): Bindable[Element, Element] = Binding.Constant(_)

  given [Keyword, Element, Value](using
      Dsl.Run[Keyword, Binding[Element], Value]
  ): Bindable[Keyword, Element] =
    summon[Dsl.Run[Keyword, Binding[Element], Value]]

  given [Element, Value]: Bindable[Binding[Element], Element] =
    identity
