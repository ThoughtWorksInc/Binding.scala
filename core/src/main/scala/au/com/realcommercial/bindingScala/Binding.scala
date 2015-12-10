package au.com.realcommercial.bindingScala


import java.util.EventObject

import scala.annotation.tailrec
import scala.beans.BeanProperty
import scala.collection.{GenSeq, mutable, immutable}
import scala.collection.mutable.ArrayBuffer
import scalaz.Monad

object Binding {

  final class ChangedEvent[+Value](source: AnyRef,
                                   @BeanProperty val oldValue: Value,
                                   @BeanProperty val newValue: Value) extends EventObject(source)

  final class PatchedEvent[+Element](source: AnyRef,
                                     @BeanProperty val oldSeq: Seq[Element],
                                     @BeanProperty val from: Int,
                                     @BeanProperty val that: GenSeq[Element],
                                     @BeanProperty val replaced: Int) extends EventObject(source)

  trait ChangedListener[-Value] {
    def changed(event: ChangedEvent[Value]): Unit
  }

  trait PatchedListener[-Element] {
    def patched(event: PatchedEvent[Element]): Unit
  }

  final case class Constant[A](override val getValue: A) extends Binding[A] {
    override def removeChangedListener(listener: ChangedListener[A]): Unit = {
      // Do nothing because this Constant never changes
    }

    override def addChangedListener(listener: ChangedListener[A]): Unit = {
      // Do nothing because this Constant never changes
    }
  }


  final class Var[A](private var cache: A) extends Binding[A] {

    private val publisher = new Publisher[ChangedListener[A]]

    override final def getValue: A = {
      cache
    }

    final def setValue(a: A): Unit = {
      if (cache != a) {
        for ((listener, _) <- publisher) {
          listener.changed(new ChangedEvent(this, cache, a))
        }
        cache = a
      }
    }

    override def removeChangedListener(listener: ChangedListener[A]): Unit = {
      publisher.unsubscribe(listener)
    }

    override def addChangedListener(listener: ChangedListener[A]): Unit = {
      publisher.subscribe(listener)
    }
  }

  final class FlatMap[A, B](upstream: Binding[A], f: A => Binding[B])
    extends Binding[B] with ChangedListener[B] {

    object forwarder
      extends Publisher[ChangedListener[B]]
      with ChangedListener[A] {

      override final def changed(event: ChangedEvent[A]): Unit = {
        cache.removeChangedListener(FlatMap.this)
        val newCache = f(event.newValue)
        newCache.addChangedListener(FlatMap.this)
        if (event.oldValue != newCache.getValue) {
          for ((listener, _) <- forwarder.this) {
            listener.changed(new ChangedEvent(FlatMap.this, cache.getValue, newCache.getValue))
          }
        }
        cache = newCache
      }
    }

    override final def changed(event: ChangedEvent[B]) = {
      for ((listener, _) <- forwarder) {
        listener.changed(new ChangedEvent(FlatMap.this, event.oldValue, event.newValue))
      }
    }

    override def addChangedListener(listener: ChangedListener[B]): Unit = {
      if (forwarder.isEmpty) {
        upstream.addChangedListener(forwarder)
        cache.addChangedListener(this)
      }
      forwarder.subscribe(listener)
    }


    var cache: Binding[B] = f(upstream.getValue)

    override final def getValue: B = {
      @tailrec
      def tailrecGetValue(binding: Binding[B]): B = {
        binding match {
          case flatMap: FlatMap[_, B] => tailrecGetValue(flatMap.cache)
          case _ => binding.getValue
        }
      }
      tailrecGetValue(cache)
    }

    override def removeChangedListener(listener: ChangedListener[B]): Unit = {
      forwarder.unsubscribe(listener)
      if (forwarder.isEmpty) {
        upstream.removeChangedListener(forwarder)
        cache.removeChangedListener(this)
      }
    }

  }

  implicit object BindingInstances extends Monad[Binding] {
    override def bind[A, B](fa: Binding[A])(f: (A) => Binding[B]): Binding[B] = new FlatMap[A, B](fa, f)

    override def point[A](a: => A): Binding[A] = Constant(a)
  }

  trait BindingSeq[+A] extends Binding[Seq[A]] {

    def removePatchedListener(listener: PatchedListener[A]): Unit

    def addPatchedListener(listener: PatchedListener[A]): Unit

  }

  private case class SingleSeq[+A](element: A) extends collection.immutable.IndexedSeq[A] {

    override final def length: Int = 1

    override final def apply(idx: Int) = {
      if (idx == 0) {
        element
      } else {
        throw new IndexOutOfBoundsException
      }
    }

    override final def iterator = Iterator.single(element)

  }

  final class VarBuffer[A](initialValues: A*) extends BindingSeq[A] {
    private val publisher = new Publisher[PatchedListener[A]]

    private var cache = ArrayBuffer(initialValues: _*)

    override def getValue: mutable.Buffer[A] = new Proxy

    class Proxy extends mutable.Buffer[A] {
      override def apply(n: Int): A = {
        cache.apply(n)
      }

      override def update(n: Int, newelem: A): Unit = {
        for {
          (listener, _) <- publisher
        } yield {
          listener.patched(new PatchedEvent(VarBuffer.this, this, n, SingleSeq(newelem), 1))
        }
        cache.update(n, newelem)
      }

      override def clear(): Unit = {
        for {
          (listener, _) <- publisher
        } yield {
          listener.patched(new PatchedEvent(VarBuffer.this, this, 0, Nil, cache.length))
        }
        cache.clear()
      }

      override def length: Int = {
        cache.length
      }

      override def remove(n: Int): A = {
        for {
          (listener, _) <- publisher
        } yield {
          listener.patched(new PatchedEvent(VarBuffer.this, this, n, Nil, 1))
        }
        cache.remove(n)
      }

      override def +=:(elem: A): this.type = {
        for {
          (listener, _) <- publisher
        } yield {
          listener.patched(new PatchedEvent(VarBuffer.this, this, 0, SingleSeq(elem), 0))
        }
        cache += elem
        Proxy.this
      }

      override def +=(elem: A): this.type = {
        for {
          (listener, _) <- publisher
        } yield {
          listener.patched(new PatchedEvent(VarBuffer.this, this, cache.length, SingleSeq(elem), 0))
        }
        cache += elem
        Proxy.this
      }

      override def insertAll(n: Int, elems: Traversable[A]): Unit = {
        val seq = elems.toSeq
        for {
          (listener, _) <- publisher
        } yield {
          listener.patched(new PatchedEvent(VarBuffer.this, this, n, seq, 0))
        }
        cache.insertAll(n, elems)
      }

      override def iterator: Iterator[A] = {
        cache.iterator
      }
    }

    override def removeChangedListener(listener: ChangedListener[Seq[A]]): Unit = {
      ???
    }

    override def addChangedListener(listener: ChangedListener[Seq[A]]): Unit = {
      ???
    }

    override def removePatchedListener(listener: PatchedListener[A]): Unit = {
      publisher.unsubscribe(listener)
    }

    override def addPatchedListener(listener: PatchedListener[A]): Unit = {
      publisher.subscribe(listener)
    }

  }

  /*
  final class ProxySeq[A](underlying: Binding[immutable.Seq[A]]) extends BindingSeq[A] {

    private object forwarder extends Publisher[PatchedListener[this.type]] {

    }

    override def removePatchedListener(listener: PatchedListener[ProxySeq.this.type]): Unit = ???

    override def addPatchedListener(listener: PatchedListener[ProxySeq.this.type]): Unit = ???

    override def getValue: A = ???

    override def subscribe(listener: ValueSubscriber[,ProxySeq.this.type]): Unit = ???

    override def unsubscribe(listener: ValueSubscriber[,ProxySeq.this.type]): Unit = ???
  }
*/


  private final class ValueProxy[B](underlying: Seq[Binding[B]]) extends Seq[B] {
    override def length: Int = {
      underlying.length
    }

    override def apply(idx: Int): B = {
      underlying(idx).getValue
    }

    override def iterator: Iterator[B] = {
      underlying.iterator.map(_.getValue)
    }
  }

  final class MapSeq[A, B](upstream: BindingSeq[A], f: A => Binding[B]) extends BindingSeq[B] {

    var cache: ArrayBuffer[Binding[B]] = {
      (for {
        a <- upstream.getValue
      } yield f(a)) (collection.breakOut(ArrayBuffer.canBuildFrom))
    }

    override def getValue: Seq[B] = new ValueProxy(cache)

    private object changeForwarder extends Publisher[ChangedListener[Seq[B]]] with ChangedListener[Seq[A]] {
      override def changed(event: ChangedEvent[Seq[A]]): Unit = {
        val newCache = (for {
          a <- event.newValue
        } yield f(a)) (collection.breakOut(ArrayBuffer.canBuildFrom))
        for ((listener, _) <- this) {
          listener.changed(new ChangedEvent(MapSeq.this, new ValueProxy(cache), new ValueProxy(newCache)))
        }
        for (oldChild <- cache) {
          oldChild.removeChangedListener(patchForwarder)
        }
        for (newChild <- newCache) {
          newChild.removeChangedListener(patchForwarder)
        }
        cache = newCache
      }
    }

    private object patchForwarder extends Publisher[PatchedListener[B]] with PatchedListener[A] with ChangedListener[B] {
      override def patched(event: PatchedEvent[A]): Unit = {
        val mappedNewChildren = (for {
          child <- event.that
        } yield f(child)) (collection.breakOut(Seq.canBuildFrom))
        for ((listener, _) <- this) {
          listener.patched(new PatchedEvent(MapSeq.this, new ValueProxy(cache), event.from, new ValueProxy(mappedNewChildren), event.replaced))
        }
        for (oldChild <- cache.view(event.from, event.replaced)) {
          oldChild.removeChangedListener(this)
        }
        for (newChild <- mappedNewChildren) {
          newChild.addChangedListener(this)
        }
        cache.patch(event.from, mappedNewChildren, event.replaced)
      }

      override def changed(event: ChangedEvent[B]): Unit = {
        val index = cache.indexOf(event.getSource)
        for ((listener, _) <- this) {
          listener.patched(new PatchedEvent(MapSeq.this, new ValueProxy(cache), index, SingleSeq(event.oldValue), 1))
        }
      }
    }

    private var numberOfValueSubscriber = 0

    private def checkForUpstreamSubscription(): Unit = {
      if (patchForwarder.isEmpty && changeForwarder.isEmpty) {
        upstream.addChangedListener(changeForwarder)
        upstream.addPatchedListener(patchForwarder)
        for (child <- cache) {
          child.addChangedListener(patchForwarder)
        }
      }
    }

    private def checkForUpstreamUnsubscription(): Unit = {
      if (patchForwarder.isEmpty && changeForwarder.isEmpty) {
        upstream.removeChangedListener(changeForwarder)
        upstream.removePatchedListener(patchForwarder)
        for (child <- cache) {
          child.removeChangedListener(patchForwarder)
        }
      }
    }

    override def removePatchedListener(listener: PatchedListener[B]): Unit = {
      patchForwarder.unsubscribe(listener)
      checkForUpstreamUnsubscription()
    }

    override def addPatchedListener(listener: PatchedListener[B]): Unit = {
      checkForUpstreamSubscription()
      patchForwarder.subscribe(listener)
    }

    override def removeChangedListener(listener: ChangedListener[Seq[B]]): Unit = {
      changeForwarder.unsubscribe(listener)
      checkForUpstreamUnsubscription()
    }

    override def addChangedListener(listener: ChangedListener[Seq[B]]): Unit = {
      checkForUpstreamSubscription()
      changeForwarder.subscribe(listener)
    }

  }

  final class FlatMappedSeq[A, B](source: BindingSeq[A], f: A => BindingSeq[B]) extends BindingSeq[B] {
    override def getValue: Seq[B] = ???

    override def removePatchedListener(listener: PatchedListener[B]): Unit = ???

    override def addPatchedListener(listener: PatchedListener[B]): Unit = ???

    override def addChangedListener(listener: ChangedListener[Seq[B]]): Unit = ???

    override def removeChangedListener(listener: ChangedListener[Seq[B]]): Unit = ???
  }

}


trait Binding[+A] {

  import Binding.ChangedListener

  def getValue: A

  def removeChangedListener(listener: ChangedListener[A]): Unit

  def addChangedListener(listener: ChangedListener[A]): Unit

}

