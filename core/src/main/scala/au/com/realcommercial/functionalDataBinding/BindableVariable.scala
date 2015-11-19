package au.com.realcommercial.functionalDataBinding

import scalaz.Cont

final case class BindableVariable[A](private var internalValue: A) {

  def value_=(a: A): Unit = {
    // TODO: reenter check
    internalValue = a
    listener(a)
  }

  def value = internalValue

  private var listener: A => Unit = null

  val binding: Binding[A] = Cont[Unit, A]{ continue =>
    // TODO: should not
    listener = continue
    this.value = internalValue
  }

}
