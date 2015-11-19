package au.com.realcommercial.functionalDataBinding

import scalaz.Cont

final case class BindableVariable[A](private var internalValue: A) {

  def value_=(a: A): Unit = {
    if (internalValue != a) {
      internalValue = a
      listener(a)
    }
  }

  def value = internalValue

  private var listener: A => Unit = null

  val binding: Binding[A] = Cont[Unit, A]{ continue =>
    listener = continue
    this.value = internalValue
  }

}
