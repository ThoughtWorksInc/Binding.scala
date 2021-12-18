package com.thoughtworks.binding

import scalaz.Applicative
object BindableSeq:
  type Aux[-From, Value0] = BindableSeq[From] {
    type Value = Value0
  }
  type Lt[-From, +Value0] = BindableSeq[From] {
    type Value <: Value0
  }
  given [A](using Applicative[Binding.Awaitable]): BindableSeq.Aux[A, A] =
    new BindableSeq[A]:
      type Value = A
      def toBindingSeq(a: A): Binding.BindingSeq[A] = Binding.Constants(a)

trait BindableSeq[-From]:
  type Value
  def toBindingSeq(from: From): Binding.BindingSeq[Value]
