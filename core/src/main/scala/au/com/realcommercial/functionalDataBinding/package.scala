package au.com.realcommercial

import scalaz._

package object functionalDataBinding {
  type Binding[A] = Cont[Unit, A]

  implicit def bindingMonad: Monad[Binding] = {
    IndexedContsT.ContsTMonad
  }

}
