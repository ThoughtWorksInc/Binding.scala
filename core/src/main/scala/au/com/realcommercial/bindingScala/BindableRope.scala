package au.com.realcommercial.bindingScala
/*
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


sealed trait BindableRope[+A] {

  def flatten: Seq[A]

  import au.com.realcommercial.bindingScala.BindableRope._

  def subscribe(subscriber: Subscriber[A]): Unit

  def unsubscribe(subscriber: Subscriber[A]): Unit

  def map[B](f: A => B): BindableRope[B] = new Mapped(this, f)

}

object BindableRope {

  trait Subscriber[-A] {

    def splice(source: BindableRope[A], index: Int, numberOfOldChildren: Int, newChildren: A*): Unit

    def insert(source: BindableRope[A], index: Int, newChildren: A*): Unit

    def remove(source: BindableRope[A], index: Int): Unit

    def update(source: BindableRope[A], index: Int, newChild: A): Unit

  }

  object Empty extends BindableRope[Nothing] {
    override def flatten: Seq[Nothing] = Nil

    override def subscribe(subscriber: Subscriber[Nothing]): Unit = {}

    override def unsubscribe(subscriber: Subscriber[Nothing]): Unit = {}
  }

  final class Single[A](private var data: A) extends BindableRope[A] {

    val publisher = new Publisher[Subscriber[A]]

    def update(index: Int, element: A): Unit = {
      if (index == 0) {
        if (data != element) {
          data = element
          for ((subscriber, _) <- publisher) {
            subscriber.update(Single.this, 0, element)
          }
        }
      } else {
        throw new IndexOutOfBoundsException
      }
    }

    override def flatten = new mutable.Seq[A] {

      override def length: Int = 1

      override def apply(index: Int): A = {
        if (index == 0) {
          data
        } else {
          throw new IndexOutOfBoundsException
        }
      }

      override def iterator: Iterator[A] = {
        Iterator(data)
      }

      override def update(index: Int, element: A): Unit = {
        Single.this.update(index, element)
      }
    }

    override def subscribe(subscriber: Subscriber[A]): Unit = {
      publisher.subscribe(subscriber)
    }

    override def unsubscribe(subscriber: Subscriber[A]): Unit = {
      publisher.unsubscribe(subscriber)
    }
  }

  final class Mapped[A, B](source: BindableRope[A], f: A => B) extends BindableRope[B] {

    val cache = {
      val cache = new ArrayBuffer[B]
      for (a <- source.flatten) {
        cache += f(a)
      }
      cache
    }

    private object forwarder extends Publisher[Subscriber[B]] with Subscriber[A] {

      override def insert(target: BindableRope[A], index: Int, newChildren: A*): Unit = {
        val mappedNewChildren = newChildren.map(f)
        for ((subscriber, _) <- forwarder) {
          subscriber.insert(Mapped.this, index, mappedNewChildren: _*)
        }
        cache.insertAll(index, mappedNewChildren)
      }

      override def remove(target: BindableRope[A], index: Int): Unit = {
        for ((subscriber, _) <- forwarder) {
          subscriber.remove(Mapped.this, index)
        }
        cache.remove(index)
      }

      override def update(target: BindableRope[A], index: Int, newChild: A): Unit = {
        val mappedNewChild = f(newChild)
        for ((subscriber, _) <- forwarder) {
          subscriber.update(Mapped.this, index, mappedNewChild)
        }
        cache.update(index, mappedNewChild)
      }

      override def splice(target: BindableRope[A], index: Int, numberOfOldChildren: Int, newChildren: A*): Unit = {
        val mappedNewChildren = newChildren.map(f)
        for ((subscriber, _) <- forwarder) {
          subscriber.splice(Mapped.this, index, numberOfOldChildren, mappedNewChildren: _*)
        }
        cache.remove(index, numberOfOldChildren)
        cache.insertAll(index, mappedNewChildren)
      }
    }

    override def subscribe(subscriber: Subscriber[B]): Unit = {
      if (forwarder.isEmpty) {
        source.subscribe(forwarder)
      }
      forwarder.subscribe(subscriber)
    }

    override def unsubscribe(subscriber: Subscriber[B]): Unit = {
      forwarder.unsubscribe(subscriber)
      if (forwarder.isEmpty) {
        source.unsubscribe(forwarder)
      }
    }

    override def flatten: Seq[B] = cache
  }

  final class Proxy[A](data: BindableRope[A]) extends BindableRope[A] {

    override def subscribe(subscriber: Subscriber[A]): Unit = {
      data.subscribe(subscriber)
    }

    override def unsubscribe(subscriber: Subscriber[A]): Unit = {
      data.unsubscribe(subscriber)
    }

    override def flatten = new Seq[A] {

      override def length: Int = {
        data.flatten.length
      }

      override def apply(index: Int): A = {
        data.flatten.apply(index)
      }

      override def iterator: Iterator[A] = {
        data.flatten.iterator
      }
    }
  }

  abstract sealed class Tree[A] extends BindableRope[A] {

    protected def underlyingSeq: mutable.Seq[BindableRope[A]]

    @tailrec
    private def findRaw(rawIndex: Int): Option[A] = {
      if (rawIndex < underlyingSeq.length) {
        underlyingSeq(rawIndex).flatten.headOption match {
          case some: Some[A] => some
          case None => findRaw(rawIndex + 1)
        }
      } else {
        None
      }
    }

    def update(rawIndex: Int, newChildren: BindableRope[A]): Unit = {
      val oldChildren = underlyingSeq(rawIndex)
      val flatIndex = underlyingSeq.view(0, rawIndex).map(_.flatten.size).sum
      val positionOption = findRaw(rawIndex)
      for ((parentSubscriber, _) <- forwarder) {
        parentSubscriber.splice(this, flatIndex, oldChildren.flatten.size, newChildren.flatten: _*)
      }
      underlyingSeq(rawIndex) = newChildren
    }

    private object forwarder extends Publisher[Subscriber[A]] with Subscriber[A] {

      private def flatIndexOf(element: BindableRope[A]): Int = {
        val i = underlyingSeq.iterator
        @tailrec
        def loop(count: Int): Int = {
          if (i.hasNext) {
            if (i.next() == element) {
              count
            } else {
              loop(count + element.flatten.size)
            }
          } else {
            throw new IllegalArgumentException(s"$element is not a child of $this")
          }
        }
        loop(0)
      }

      override def insert(target: BindableRope[A], index: Int, newChildren: A*): Unit = {
        val baseFlatIndex = flatIndexOf(target)
        val flatIndex = baseFlatIndex + index
        for ((parentSubscriber, _) <- this) {
          parentSubscriber.insert(Tree.this, flatIndex, newChildren: _*)
        }
      }

      override def remove(target: BindableRope[A], index: Int): Unit = {
        val baseFlatIndex = flatIndexOf(target)
        val flatIndex = baseFlatIndex + index
        for ((parentSubscriber, _) <- this) {
          parentSubscriber.remove(Tree.this, flatIndex)
        }
      }

      override def update(target: BindableRope[A], index: Int, newChild: A): Unit = {
        val baseFlatIndex = flatIndexOf(target)
        val flatIndex = baseFlatIndex + index
        for ((parentSubscriber, _) <- this) {
          parentSubscriber.update(Tree.this, flatIndex, newChild)
        }
      }

      override def splice(target: BindableRope[A], index: Int, numberOfOldChildren: Int, newChildren: A*): Unit = {
        val baseFlatIndex = flatIndexOf(target)
        val flatIndex = baseFlatIndex + index
        for ((parentSubscriber, _) <- this) {
          parentSubscriber.splice(Tree.this, flatIndex, numberOfOldChildren, newChildren: _*)
        }
      }
    }

    override final def subscribe(subscriber: Subscriber[A]): Unit = {
      if (forwarder.isEmpty) {
        for (child <- underlyingSeq) {
          child.subscribe(forwarder)
        }
      }
      forwarder.subscribe(subscriber)
    }

    override final def unsubscribe(subscriber: Subscriber[A]): Unit = {
      forwarder.unsubscribe(subscriber)
      if (forwarder.isEmpty) {
        for (child <- underlyingSeq) {
          child.unsubscribe(forwarder)
        }
      }
    }

    private[BindableRope] class RopeSeq extends Seq[A] {

      override def length: Int = {
        underlyingSeq.view.map(_.flatten.length).sum
      }

      @tailrec
      private def apply(i: Int, rest: Int): A = {
        if (i < underlyingSeq.length) {
          val child = underlyingSeq(i)
          val childLength = child.flatten.length
          if (childLength > rest) {
            child.flatten(rest)
          } else {
            apply(i + 1, rest - childLength)
          }
        } else {
          throw new IndexOutOfBoundsException
        }
      }

      override def apply(index: Int): A = {
        apply(0, index)
      }

      override def iterator: Iterator[A] = {
        for {
          child <- underlyingSeq.iterator
          element <- child.flatten
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

    final def update(index: Int, element: A): Unit = {
      val oldChild = underlyingSeq(index)
      if (oldChild != element) {
        underlyingSeq.update(index, element)
        for ((parentSubscriber, _) <- publisher) {
          parentSubscriber.update(Leaf.this, index, element)
        }
      }
    }

    final def length: Int = {
      underlyingSeq.length
    }

    final def apply(index: Int): A = {
      underlyingSeq.apply(index)
    }

    final def iterator: Iterator[A] = {
      underlyingSeq.iterator
    }

    private[BindableRope] class RopeSeq extends mutable.Seq[A] {

      override def update(index: Int, element: A): Unit = {
        Leaf.this.update(index, element)
      }

      override def length: Int = {
        Leaf.this.length
      }

      override def apply(index: Int): A = {
        Leaf.this.apply(index)
      }

      override def iterator: Iterator[A] = {
        Leaf.this.iterator
      }

    }

  }

  final class ArrayLeaf[A](data: Array[A]) extends Leaf[A] {
    override def underlyingSeq = data

    override def flatten: RopeSeq with mutable.Seq[A] = new RopeSeq
  }

  final class ArrayTree[A](data: Array[BindableRope[A]]) extends Tree[A] {

    override def underlyingSeq = data

    override def flatten: RopeSeq with Seq[A] = new RopeSeq

  }

  final class BufferLeaf[A](data: mutable.Buffer[A]) extends Leaf[A] {
    def +=:(element: A): this.type = {
      for ((subscriber, _) <- publisher) {
        subscriber.insert(this, 0, element)
      }
      element +=: data
      this
    }

    def +=(element: A): this.type = {
      for ((subscriber, _) <- publisher) {
        subscriber.insert(this, data.length, element)
      }
      data += element
      this
    }

    override def underlyingSeq = data

    override def flatten: RopeSeq with mutable.Seq[A] = new RopeSeq
  }

  final class BufferTree[A](data: mutable.Buffer[BindableRope[A]]) extends Tree[A] {

    override def underlyingSeq = data

    override def flatten: RopeSeq with Seq[A] = new RopeSeq

  }

}

*/