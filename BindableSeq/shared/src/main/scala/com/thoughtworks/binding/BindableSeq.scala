package com.thoughtworks.binding
object BindableSeq:
  type Aux[-From, Value0] = BindableSeq[From] {
    type Value = Value0
  }
  type Lt[-From, +Value0] = BindableSeq[From] {
    type Value <: Value0
  }

trait BindableSeq[-From]:
  type Value
  def toBindingSeq(from: From): Binding.BindingSeq[Value]
