package au.com.realcommercial.functionalDataBinding
package dom


import scala.annotation.tailrec
import scalaz.Applicative

class DomNodeSeq[A] {

}

object DomNodeSeq {

  def apply[A](elements: A*): DomNodeSeq[A] = ???

  implicit def ArrayBufferMutableSeq: MutableSeq[DomNodeSeq] = new MutableSeq[DomNodeSeq] {

    override def mutableSequence[G[_] : Applicative, A](fga: G[A]*): G[DomNodeSeq[A]] = {
      /*
            if (fga.isEmpty) {
              DomNodeSeq.empty[A].point[G]
            } else {
              val model = new DomNodeSeq[A](fga.length)
              for (i <- 0 until fga.length) {
                model += null.asInstanceOf[A]
              }

              val result = Applicative[G].point(model)

              @tailrec
              def curriedSetter(i: Int, result: Any): A => Any = {
                val next = { a: A =>
                  model(i) = a
                  result
                }

                if (i == fga.length - 1) {
                  next
                } else {
                  curriedSetter(i + 1, next)
                }
              }

              val f = curriedSetter(0, model)

              @tailrec
              def applyAll(i: Int, result: G[Any]): G[DomNodeSeq[A]] = {
                val next = fga(i) <*> result.asInstanceOf[G[A => Any]]
                if (i == 0) {
                  next.asInstanceOf[G[DomNodeSeq[A]]]
                } else {
                  applyAll(i - 1, next)
                }
              }

              applyAll(fga.length - 2, fga.last.map(f.asInstanceOf[A => Any]))
            }


          */
      ???
    }
  }

}