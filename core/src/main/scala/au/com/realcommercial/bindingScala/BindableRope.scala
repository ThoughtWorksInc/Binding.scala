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

    def replaceChild(parent: BindableRope[A], newChild: A, oldChild: A): Unit

  }

  final class Single[A](var data: A) extends BindableRope[A] {

    val publisher = new Publisher[Subscriber[A]]

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
          if (data != elem) {
            val oldChild = data
            data = elem
            for ((subscriber, _) <- publisher) {
              subscriber.replaceChild(Single.this, elem, oldChild)
            }
          }
        } else {
          throw new IndexOutOfBoundsException
        }
      }
    }

    override def subscribe(subscriber: Subscriber[A]): Unit = {
      publisher.subscribe(subscriber)
    }

    override def unsubscribe(subscriber: Subscriber[A]): Unit = {
      publisher.unsubscribe(subscriber)
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

  abstract class Multiple[A] extends BindableRope[A] {
    protected def underlyingSeq: mutable.Seq[BindableRope[A]]

    @tailrec
    private def findRaw(rawIndex: Int): Option[A] = {
      if (rawIndex < underlyingSeq.length) {
        underlyingSeq(rawIndex).toSeq.headOption match {
          case some: Some[A] => some
          case None => findRaw(rawIndex + 1)
        }
      } else {
        None
      }
    }

    def updateRaw(rawIndex: Int, newChildren: BindableRope[A]): Unit = {
      val oldChildren = underlyingSeq(rawIndex)
      if (oldChildren == null) {
        underlyingSeq(rawIndex) = newChildren
        for ((parent, _) <- forwarder) {
          for (newChild <- newChildren.toSeq) {
            parent.appendChild(this, newChild)
          }
        }
      } else {
        val positionOption = findRaw(rawIndex)
        underlyingSeq(rawIndex) = newChildren
        for ((parent, _) <- forwarder) {
          for (newChild <- newChildren.toSeq) {
            positionOption match {
              case None =>
                parent.appendChild(this, newChild)
              case Some(position) =>
                parent.insertBefore(this, newChild, position)
            }
          }
          if (oldChildren != null) {
            for (oldChild <- oldChildren.toSeq) {
              parent.removeChild(this, oldChild)
            }
          }
        }
      }
    }

    object forwarder extends Publisher[Subscriber[A]] with Subscriber[A] {
      override def insertBefore(parent: BindableRope[A], newChild: A, refChild: A): Unit = ???

      override def removeChild(parent: BindableRope[A], element: A): Unit = ???

      override def appendChild(parent: BindableRope[A], newChild: A): Unit = ???

      override def replaceChild(parent: BindableRope[A], newChild: A, oldChild: A): Unit = {
        for ((parent, _) <- this) {
          parent.replaceChild(Multiple.this, newChild, oldChild)
        }
      }
    }

    def subscribe(subscriber: Subscriber[A]): Unit = {
      if (forwarder.isEmpty) {
        for (child <- underlyingSeq) {
          child.subscribe(forwarder)
        }
      }
      forwarder.subscribe(subscriber)
    }

    def unsubscribe(subscriber: Subscriber[A]): Unit = {
      forwarder.unsubscribe(subscriber)
      if (forwarder.isEmpty) {
        for (child <- underlyingSeq) {
          child.unsubscribe(forwarder)
        }
      }
    }

    class RopeSeq extends mutable.Seq[A] {

      val forwarder = new Publisher[Subscriber[A]] with Subscriber[A] {
        override def insertBefore(parent: BindableRope[A], newChild: A, refChild: A): Unit = ???

        override def removeChild(parent: BindableRope[A], element: A): Unit = ???

        override def appendChild(parent: BindableRope[A], newChild: A): Unit = ???

        override def replaceChild(parent: BindableRope[A], newChild: A, oldChild: A): Unit = ???
      }

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

  }

  final class Fixed[A](data: Array[BindableRope[A]]) extends Multiple[A] {
    override protected def underlyingSeq = data

    override def toSeq = new RopeSeq

  }

  final class Growable[A](data: mutable.Buffer[BindableRope[A]]) extends Multiple[A] {

    override protected def underlyingSeq = data

    override def toSeq = new RopeSeq with mutable.Buffer[A] {

      override def +=(elem: A): this.type = ???

      override def clear(): Unit = ???

      override def remove(n: Int): A = ???

      override def +=:(elem: A): this.type = ???

      override def insertAll(n: Int, elems: Traversable[A]): Unit = ???
    }
  }

  def sourceSeq[A](elements: A*): mutable.Buffer[A] = {
    val data = ArrayBuffer.empty[BindableRope[A]]
    for (a <- elements) {
      data += new Single(a)
    }
    new Growable(data).toSeq
  }

}