package au.com.realcommercial.functionalDataBinding

import scalaz.syntax.applicative._
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scalaz.{Bind, Applicative, Monad}

trait MutableSeq[F[_]] {

  def mutableSequence[G[_] : Applicative, A](elements: F[G[A]]): G[F[A]]

}

object MutableSeq {

  def apply[F[_]](implicit ev: MutableSeq[F]) = ev

  implicit def ArrayBufferMutableSeq: MutableSeq[ArrayBuffer] = new MutableSeq[ArrayBuffer] {

    override def mutableSequence[G[_] : Applicative, A](fga: ArrayBuffer[G[A]]): G[ArrayBuffer[A]] = {

      if (fga.isEmpty) {
        ArrayBuffer.empty[A].point[G]
      } else {
        val model = new ArrayBuffer[A](fga.length)
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
        def applyAll(i: Int, result: G[Any]): G[ArrayBuffer[A]] = {
          val next = fga(i) <*> result.asInstanceOf[G[A => Any]]
          if (i == 0) {
            next.asInstanceOf[G[ArrayBuffer[A]]]
          } else {
            applyAll(i - 1, next)
          }
        }

        applyAll(fga.length - 2, fga.last.map(f.asInstanceOf[A => Any]))
      }

    }
  }

}