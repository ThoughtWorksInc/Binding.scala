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

        def applyAll(i: Int): G[ArrayBuffer[A]] = {
          if (i == fga.length - 1) {
            fga(i).map { a =>
              model(i) = a
              model
            }
          } else {
            fga(i) <*> applyAll(i + 1).map { model =>
              { a =>
                model(i) = a
                model
              }
            }
          }
        }

        applyAll(0)

      }

    }
  }

}