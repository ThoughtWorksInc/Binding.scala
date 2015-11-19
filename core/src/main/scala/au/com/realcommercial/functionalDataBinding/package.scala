package au.com.realcommercial

import scalaz._

package object functionalDataBinding {
  type Binding[A] = Cont[Unit, A]

  implicit def castBinding[A, B >: A](a: Binding[A]): Binding[B] = a.asInstanceOf[Binding[B]]

  implicit def bindingMonad: Monad[Binding] = {
    IndexedContsT.ContsTMonad
  }

}
