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

  abstract sealed class Tree[A] extends BindableRope[A] {

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

    private object forwarder extends Publisher[Subscriber[A]] with Subscriber[A] {
      override def insertBefore(parent: BindableRope[A], newChild: A, refChild: A): Unit = {
        for ((parentSubscriber, _) <- this) {
          parentSubscriber.insertBefore(Tree.this, newChild, refChild)
        }
      }

      override def removeChild(parent: BindableRope[A], element: A): Unit = {
        for ((parentSubscriber, _) <- this) {
          parentSubscriber.removeChild(Tree.this, element)
        }
      }

      override def appendChild(parent: BindableRope[A], newChild: A): Unit = {
        val i = underlyingSeq.iterator
        underlyingSeq.iterator.dropWhile(_ != parent)

        @tailrec
        def findHead(): Option[A] = {
          if (i.hasNext) {
            val current = i.next
            val headOption = current.toSeq.headOption
            if (headOption.isDefined) {
              headOption
            } else {
              findHead()
            }
          } else {
            None
          }
        }

        findHead() match {
          case None =>
            for ((parentSubscriber, _) <- this) {
              parentSubscriber.appendChild(Tree.this, newChild)
            }
          case Some(refChild) =>
            for ((parentSubscriber, _) <- this) {
              parentSubscriber.insertBefore(Tree.this, newChild, refChild)
            }
        }
      }

      override def replaceChild(parent: BindableRope[A], newChild: A, oldChild: A): Unit = {
        for ((parentSubscriber, _) <- this) {
          parentSubscriber.replaceChild(Tree.this, newChild, oldChild)
        }
      }
    }

    override def subscribe(subscriber: Subscriber[A]): Unit = {
      if (forwarder.isEmpty) {
        for (child <- underlyingSeq) {
          child.subscribe(forwarder)
        }
      }
      forwarder.subscribe(subscriber)
    }

    override def unsubscribe(subscriber: Subscriber[A]): Unit = {
      forwarder.unsubscribe(subscriber)
      if (forwarder.isEmpty) {
        for (child <- underlyingSeq) {
          child.unsubscribe(forwarder)
        }
      }
    }

    private[BindableRope] class RopeSeq extends mutable.Seq[A] {

      override def update(idx: Int, elem: A): Unit = {
        val i = underlyingSeq.iterator
        @tailrec
        def loop(rest: Int, elem: A): Unit = {
          if (i.hasNext) {
            val child = i.next().toSeq
            val childLength = child.length
            if (childLength > rest) {
              child.update(rest, elem)
            } else {
              loop(rest - childLength, elem)
            }
          } else {
            throw new IndexOutOfBoundsException
          }
        }
        loop(idx, elem)
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

  abstract sealed class Leaf[A] extends BindableRope[A] {
    protected def underlyingSeq: mutable.Seq[A]

    val publisher = new Publisher[Subscriber[A]]

    override def subscribe(subscriber: Subscriber[A]): Unit = {
      publisher.subscribe(subscriber)
    }

    override def unsubscribe(subscriber: Subscriber[A]): Unit = {
      publisher.unsubscribe(subscriber)
    }

    private[BindableRope] class RopeSeq extends mutable.Seq[A] {

      override def update(idx: Int, elem: A): Unit = {
        val oldChild = underlyingSeq(idx)
        if (oldChild != elem) {
          for ((subscriber, _) <- publisher) {
            subscriber.replaceChild(Leaf.this, elem, oldChild)
          }
          underlyingSeq.update(idx, elem)
        }
      }

      override def length: Int = {
        underlyingSeq.length
      }

      override def apply(idx: Int): A = {
        underlyingSeq.apply(idx)
      }

      override def iterator: Iterator[A] = {
        underlyingSeq.iterator
      }

    }

  }

  final class FixedLeaf[A](data: Array[A]) extends Leaf[A] {
    override def underlyingSeq = data

    override def toSeq: RopeSeq with mutable.Seq[A] = new RopeSeq
  }

  final class FixedTree[A](data: Array[BindableRope[A]]) extends Tree[A] {

    override def underlyingSeq = data

    override def toSeq: RopeSeq with mutable.Seq[A] = new RopeSeq

  }


  sealed trait Growable[A] {

    private[BindableRope] trait GraowableSeq extends mutable.Buffer[A] {

      override def +=(elem: A): this.type = ???

      override def clear(): Unit = ???

      override def remove(n: Int): A = ???

      override def +=:(elem: A): this.type = ???

      override def insertAll(n: Int, elems: Traversable[A]): Unit = ???
    }

  }

  final class GrowableTree[A](override protected val underlyingSeq: mutable.Buffer[BindableRope[A]])
    extends Tree[A] with Growable[A] {


    override def toSeq: RopeSeq with GraowableSeq with mutable.Seq[A] = new RopeSeq with GraowableSeq
  }

  final class GrowableLeaf[A](override protected val underlyingSeq: mutable.Buffer[A])
    extends Leaf[A] with Growable[A] {

    override def toSeq: RopeSeq with GraowableSeq with mutable.Seq[A] = new RopeSeq with GraowableSeq
  }

  def sourceSeq[A](elements: A*): mutable.Buffer[A] = {
    new GrowableLeaf(mutable.Buffer(elements: _*)).toSeq
  }

}