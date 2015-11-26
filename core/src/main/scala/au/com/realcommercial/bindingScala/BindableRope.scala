package au.com.realcommercial.bindingScala

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


sealed trait BindableRope[A] {

  def toSeq: mutable.Seq[A]

  import au.com.realcommercial.bindingScala.BindableRope._

  def subscribe(subscriber: Subscriber[A]): Unit

  def unsubscribe(subscriber: Subscriber[A]): Unit

}

object BindableRope {

  trait Subscriber[A] {

    def insertBefore(parent: BindableRope[A], newChild: A, refChild: A): Unit

    def appendChild(parent: BindableRope[A], newChild: A): Unit

    def removeChild(parent: BindableRope[A], element: A): Unit

  }

  implicit final class Single[A](var data: A) extends BindableRope[A] {

    override def subscribe(subscriber: Subscriber[A]): Unit = {
      // Do nothing
    }

    override def unsubscribe(subscriber: Subscriber[A]): Unit = {
      // Do nothing
    }

    override def toSeq = new mutable.Seq[A] {

      override def length: Int = 1

      override def apply(idx: Int): A = {
        if (idx == 0) {
          data
        } else {
          throw new IndexOutOfBoundsException
        }
      }

      override def iterator: Iterator[A] = {
        Iterator(data)
      }

      override def update(idx: Int, elem: A): Unit = {
        if (idx == 0) {
          data = elem
        } else {
          throw new IndexOutOfBoundsException
        }
      }
    }
  }

  final class Proxy[A](data: BindableRope[A]) extends BindableRope[A] {

    override def subscribe(subscriber: Subscriber[A]): Unit = {
      data.subscribe(subscriber)
    }

    override def unsubscribe(subscriber: Subscriber[A]): Unit = {
      data.unsubscribe(subscriber)
    }

    override def toSeq = new mutable.Seq[A] {

      override def length: Int = {
        data.toSeq.length
      }

      override def apply(idx: Int): A = {
        data.toSeq.apply(idx)
      }

      override def iterator: Iterator[A] = {
        data.toSeq.iterator
      }

      override def update(idx: Int, elem: A): Unit = {
        data.toSeq.update(idx, elem)
      }
    }
  }


  abstract class RopeSeq[A] extends mutable.Seq[A] {

    protected def underlyingSeq: mutable.IndexedSeq[BindableRope[A]]

    @tailrec
    private def update(i: Int, rest: Int, elem: A): Unit = {
      if (i < underlyingSeq.length) {
        val child = underlyingSeq(i)
        val childLength = child.toSeq.length
        if (childLength > rest) {
          child.toSeq.update(rest, elem)
        } else {
          update(i + 1, rest - childLength, elem)
        }
      } else {
        throw new IndexOutOfBoundsException
      }
    }

    override def update(idx: Int, elem: A): Unit = {
      update(0, idx, elem)
    }

    override def length: Int = {
      underlyingSeq.view.map(_.toSeq.length).sum
    }

    @tailrec
    private def apply(i: Int, rest: Int): A = {
      if (i < underlyingSeq.length) {
        val child = underlyingSeq(i)
        val childLength = child.toSeq.length
        if (childLength > rest) {
          child.toSeq(rest)
        } else {
          apply(i + 1, rest - childLength)
        }
      } else {
        throw new IndexOutOfBoundsException
      }
    }

    override def apply(idx: Int): A = {
      apply(0, idx)
    }

    override def iterator: Iterator[A] = {
      for {
        child <- underlyingSeq.iterator
        element <- child.toSeq
      } yield element
    }
  }

  final class Fixed[A](data: Array[BindableRope[A]]) extends BindableRope[A] {

    val publisher = new Publisher[Subscriber[A]]

    override def subscribe(subscriber: Subscriber[A]): Unit = {
      publisher.subscribe(subscriber)

      // TODO: subscribe at children node
    }

    override def unsubscribe(subscriber: Subscriber[A]): Unit = {
      publisher.unsubscribe(subscriber)

      // TODO: subscribe at children node
    }

    override def toSeq = new RopeSeq[A] {
      override protected def underlyingSeq: mutable.IndexedSeq[BindableRope[A]] = data
    }
  }

  final class Growable[A](data: ArrayBuffer[BindableRope[A]]) extends BindableRope[A] {


    val publisher = new Publisher[Subscriber[A]]

    override def subscribe(subscriber: Subscriber[A]): Unit = {
      publisher.subscribe(subscriber)

      // TODO: subscribe at children node
    }

    override def unsubscribe(subscriber: Subscriber[A]): Unit = {
      publisher.unsubscribe(subscriber)

      // TODO: subscribe at children node
    }

    override def toSeq = new RopeSeq[A] with mutable.Buffer[A] {

      override protected def underlyingSeq: mutable.IndexedSeq[BindableRope[A]] = data

      override def +=(elem: A): this.type = ???

      override def clear(): Unit = ???

      override def remove(n: Int): A = ???

      override def +=:(elem: A): this.type = ???

      override def insertAll(n: Int, elems: Traversable[A]): Unit = ???
    }
  }


}