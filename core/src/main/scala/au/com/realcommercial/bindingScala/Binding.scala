package au.com.realcommercial.bindingScala


import java.util.EventObject

import scala.annotation.tailrec
import scala.beans.BeanProperty
import scala.collection.{GenSeq, mutable, immutable}
import scala.collection.mutable.ArrayBuffer
import scalaz.Monad
import scala.language.experimental.macros

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object Binding {

  private[Binding] class Publisher[Subscriber] extends collection.mutable.HashMap[Subscriber, Int] {

    override final def default(subscriber: Subscriber) = 0

    final def subscribe(subscriber: Subscriber): Unit = {
      val oldValue = this (subscriber)
      if (oldValue < 0) {
        throw new IllegalStateException()
      }
      val newValue = oldValue + 1
      this (subscriber) = newValue
    }

    final def unsubscribe(subscriber: Subscriber): Unit = {
      val oldValue = this (subscriber)
      if (oldValue <= 0) {
        throw new IllegalStateException()
      }
      val newValue = oldValue - 1
      if (newValue == 0) {
        this -= subscriber
      } else {
        this (subscriber) = newValue
      }
    }

  }

  /**
    * @group Events
    */
  private final class ChangedEvent[+Value](source: AnyRef,
                                           @BeanProperty val oldValue: Value,
                                           @BeanProperty val newValue: Value) extends EventObject(source)

  /**
    * @group Events
    */
  private final class PatchedEvent[+Element](source: AnyRef,
                                             @BeanProperty val oldSeq: Seq[Element],
                                             @BeanProperty val from: Int,
                                             @BeanProperty val that: GenSeq[Element],
                                             @BeanProperty val replaced: Int) extends EventObject(source)

  /**
    * @group Listeners
    */
  private[Binding] trait ChangedListener[-Value] {
    private[Binding] def changed(event: ChangedEvent[Value]): Unit
  }

  /**
    * @group Listeners
    */
  private[Binding] trait PatchedListener[-Element] {
    private[Binding] def patched(event: PatchedEvent[Element]): Unit
  }

  /**
    * @group Sites
    */
  trait MultiSite[Element] {

    protected def set(newValue: Seq[Element]): Unit

    protected def patch(oldSeq: Seq[Element], from: Int, that: GenSeq[Element], replaced: Int): Unit

    private object listener extends ChangedListener[Seq[Element]] with PatchedListener[Element] {

      override private[Binding] def changed(event: ChangedEvent[Seq[Element]]): Unit = {
        set(event.newValue)
      }

      override private[Binding] def patched(event: PatchedEvent[Element]): Unit = {
        patch(event.oldSeq, event.from, event.that, event.replaced)
      }
    }

    private var binding: BindingSeq[Element] = {
      val binding = BindingSeq.Empty
      binding.addChangedListener(listener)
      binding.addPatchedListener(listener)
      binding
    }


    final def :=(newBinding: BindingSeq[Element]) = {
      newBinding.addChangedListener(listener)
      newBinding.addPatchedListener(listener)
      binding.removeChangedListener(listener)
      binding.removePatchedListener(listener)
      binding = newBinding
    }
  }

  /**
    * @group Sites
    */
  trait SingleSite[Value] {

    protected def set(value: Value): Unit

    protected def initialValue: Value

    private object listener extends ChangedListener[Value] {
      override private[Binding] def changed(event: ChangedEvent[Value]): Unit = {
        set(event.newValue)
      }
    }

    private var binding: Binding[Value] = {
      val binding = Constant(initialValue)
      binding.addChangedListener(listener)
      binding
    }

    final def :=(newBinding: Binding[Value]) = {
      newBinding.addChangedListener(listener)
      binding.removeChangedListener(listener)
      binding = newBinding
    }
  }

  private final case class Constant[A](override val get: A) extends Binding[A] {
    override private[bindingScala] def removeChangedListener(listener: ChangedListener[A]): Unit = {
      // Do nothing because this Constant never changes
    }

    override private[bindingScala] def addChangedListener(listener: ChangedListener[A]): Unit = {
      // Do nothing because this Constant never changes
    }
  }

  /**
    * @group Binding Expressions
    */
  final class Var[A](private var value: A) extends Binding[A] {

    private val publisher = new Publisher[ChangedListener[A]]

    override private[bindingScala] final def get: A = {
      value
    }

    final def :=(newValue: A): Unit = {
      if (value != newValue) {
        for ((listener, _) <- publisher) {
          listener.changed(new ChangedEvent(this, value, newValue))
        }
        value = newValue
      }
    }

    override private[bindingScala] def removeChangedListener(listener: ChangedListener[A]): Unit = {
      publisher.unsubscribe(listener)
    }

    override private[bindingScala] def addChangedListener(listener: ChangedListener[A]): Unit = {
      publisher.subscribe(listener)
    }
  }

  private final class FlatMap[A, B](upstream: Binding[A], f: A => Binding[B])
    extends Binding[B] with ChangedListener[B] {

    private object forwarder
      extends Publisher[ChangedListener[B]]
      with ChangedListener[A] {

      override final def changed(event: ChangedEvent[A]): Unit = {
        cache.removeChangedListener(FlatMap.this)
        val newCache = f(event.newValue)
        newCache.addChangedListener(FlatMap.this)
        if (event.oldValue != newCache.get) {
          for ((listener, _) <- forwarder.this) {
            listener.changed(new ChangedEvent(FlatMap.this, cache.get, newCache.get))
          }
        }
        cache = newCache
      }
    }

    override private[bindingScala] final def changed(event: ChangedEvent[B]) = {
      for ((listener, _) <- forwarder) {
        listener.changed(new ChangedEvent(FlatMap.this, event.oldValue, event.newValue))
      }
    }

    override private[bindingScala] def addChangedListener(listener: ChangedListener[B]): Unit = {
      if (forwarder.isEmpty) {
        upstream.addChangedListener(forwarder)
        cache.addChangedListener(this)
      }
      forwarder.subscribe(listener)
    }

    private var cache: Binding[B] = f(upstream.get)

    override private[bindingScala] final def get: B = {
      @tailrec
      def tailrecGetValue(binding: Binding[B]): B = {
        binding match {
          case flatMap: FlatMap[_, B] => tailrecGetValue(flatMap.cache)
          case _ => binding.get
        }
      }
      tailrecGetValue(cache)
    }

    override private[bindingScala] def removeChangedListener(listener: ChangedListener[B]): Unit = {
      forwarder.unsubscribe(listener)
      if (forwarder.isEmpty) {
        upstream.removeChangedListener(forwarder)
        cache.removeChangedListener(this)
      }
    }

  }

  /**
    * @group Type class instance
    */
  implicit object BindingInstances extends Monad[Binding] {
    override def bind[A, B](fa: Binding[A])(f: (A) => Binding[B]): Binding[B] = new FlatMap[A, B](fa, f)

    override def point[A](a: => A): Binding[A] = Constant(a)
  }

  /**
    * @group Binding Expressions
    */
  trait BindingSeq[+A] extends Binding[Seq[A]] {

    private[bindingScala] def removePatchedListener(listener: PatchedListener[A]): Unit

    private[bindingScala] def addPatchedListener(listener: PatchedListener[A]): Unit

    def map[B](f: A => B): BindingSeq[B] = macro BindingSeq.Macros.map

    def flatMap[B](f: A => BindingSeq[B]): BindingSeq[B] = macro BindingSeq.Macros.flatMap


    final def withFilter(condition: A => Boolean): BindingSeq.WithFilter[A] = {
      new BindingSeq.Runtime.WithFilterData(this, condition)
    }

  }

  /**
    * @group Binding Expressions
    */
  object BindingSeq {

    sealed trait WithFilter[+A] {

      def map[B](f: A => B): BindingSeq[B] = macro BindingSeq.Macros.mapWithFilter

      def flatMap[B](f: A => BindingSeq[B]): BindingSeq[B] = macro BindingSeq.Macros.flatMapWithFilter

      def withFilter(filter: A => Boolean): BindingSeq.WithFilter[A]
    }

    private[BindingSeq] object Macros {

      def map(c: scala.reflect.macros.blackbox.Context)(f: c.Tree): c.Tree = ???

      def flatMap(c: scala.reflect.macros.blackbox.Context)(f: c.Tree): c.Tree = ???

      def mapWithFilter(c: scala.reflect.macros.blackbox.Context)(f: c.Tree): c.Tree = ???

      def flatMapWithFilter(c: scala.reflect.macros.blackbox.Context)(f: c.Tree): c.Tree = ???

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


    private[Binding] object Empty extends BindingSeq[Nothing] {
      override private[bindingScala] def removePatchedListener(listener: PatchedListener[Nothing]): Unit = {}

      override private[bindingScala] def addPatchedListener(listener: PatchedListener[Nothing]): Unit = {}

      override private[bindingScala] def get = Nil

      override private[bindingScala] def removeChangedListener(listener: ChangedListener[Seq[Nothing]]): Unit = {}

      override private[bindingScala] def addChangedListener(listener: ChangedListener[Seq[Nothing]]): Unit = {}
    }

    final class VarBuffer[A](initialValues: A*) extends BindingSeq[A] {
      private val patchedPublisher = new Publisher[PatchedListener[A]]

      private val changedPublisher = new Publisher[ChangedListener[Seq[A]]]

      private var cache = ArrayBuffer(initialValues: _*)

      override def get: mutable.Buffer[A] = new Proxy(cache)

      def reset(newValues: A*): Unit = {
        val newCache = ArrayBuffer(newValues: _*)
        for ((listener, _) <- changedPublisher) {
          listener.changed(new ChangedEvent[Seq[A]](VarBuffer.this, new Proxy(cache), new Proxy(newCache)))
        }
        cache = newCache
      }

      class Proxy(cache: ArrayBuffer[A]) extends mutable.Buffer[A] {
        override def apply(n: Int): A = {
          cache.apply(n)
        }

        override def update(n: Int, newelem: A): Unit = {
          for {
            (listener, _) <- patchedPublisher
          } {
            listener.patched(new PatchedEvent(VarBuffer.this, this, n, SingleSeq(newelem), 1))
          }
          cache.update(n, newelem)
        }

        override def clear(): Unit = {
          for {
            (listener, _) <- patchedPublisher
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
            (listener, _) <- patchedPublisher
          } yield {
            listener.patched(new PatchedEvent(VarBuffer.this, this, n, Nil, 1))
          }
          cache.remove(n)
        }

        override def +=:(elem: A): this.type = {
          for {
            (listener, _) <- patchedPublisher
          } yield {
            listener.patched(new PatchedEvent(VarBuffer.this, this, 0, SingleSeq(elem), 0))
          }
          cache += elem
          Proxy.this
        }

        override def +=(elem: A): this.type = {
          for {
            (listener, _) <- patchedPublisher
          } yield {
            listener.patched(new PatchedEvent(VarBuffer.this, this, cache.length, SingleSeq(elem), 0))
          }
          cache += elem
          Proxy.this
        }

        override def insertAll(n: Int, elems: Traversable[A]): Unit = {
          val seq = elems.toSeq
          for {
            (listener, _) <- patchedPublisher
          } yield {
            listener.patched(new PatchedEvent(VarBuffer.this, this, n, seq, 0))
          }
          cache.insertAll(n, elems)
        }

        override def iterator: Iterator[A] = {
          cache.iterator
        }
      }

      override private[bindingScala] def removeChangedListener(listener: ChangedListener[Seq[A]]): Unit = {
        changedPublisher.unsubscribe(listener)
      }

      override private[bindingScala] def addChangedListener(listener: ChangedListener[Seq[A]]): Unit = {
        changedPublisher.subscribe(listener)
      }

      override private[bindingScala] def removePatchedListener(listener: PatchedListener[A]): Unit = {
        patchedPublisher.unsubscribe(listener)
      }

      override private[bindingScala] def addPatchedListener(listener: PatchedListener[A]): Unit = {
        patchedPublisher.subscribe(listener)
      }

    }

    /**
      * Internal helpers for implementation.
      * @note Do not directly use any types in [[Runtime]] in any user code!
      */
    object Runtime {

      final case class WithFilterData[A](bindingSeq: BindingSeq[A], condition: A => Boolean) extends WithFilter[A] {
        override final def withFilter(nextCondition: A => Boolean): WithFilterData[A] = {
          new WithFilterData(bindingSeq, { a => condition(a) && nextCondition(a) })
        }
      }

      /*
      final class ProxySeq[A](underlying: Binding[immutable.Seq[A]]) extends BindingSeq[A] {

        private object forwarder extends Publisher[PatchedListener[this.type]] {

        }

        override private[bindingScala] def removePatchedListener(listener: PatchedListener[ProxySeq.this.type]): Unit = ???

        override private[bindingScala] def addPatchedListener(listener: PatchedListener[ProxySeq.this.type]): Unit = ???

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
          underlying(idx).get
        }

        override def iterator: Iterator[B] = {
          underlying.iterator.map(_.get)
        }
      }

      /**
        * @group Binding Expressions
        */
      final class MapSeq[A, B](upstream: BindingSeq[A], f: A => Binding[B]) extends BindingSeq[B] {

        var cache: ArrayBuffer[Binding[B]] = {
          (for {
            a <- upstream.get
          } yield f(a)) (collection.breakOut(ArrayBuffer.canBuildFrom))
        }

        override def get: Seq[B] = new ValueProxy(cache)

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

        override private[bindingScala] def removePatchedListener(listener: PatchedListener[B]): Unit = {
          patchForwarder.unsubscribe(listener)
          checkForUpstreamUnsubscription()
        }

        override private[bindingScala] def addPatchedListener(listener: PatchedListener[B]): Unit = {
          checkForUpstreamSubscription()
          patchForwarder.subscribe(listener)
        }

        override private[bindingScala] def removeChangedListener(listener: ChangedListener[Seq[B]]): Unit = {
          changeForwarder.unsubscribe(listener)
          checkForUpstreamUnsubscription()
        }

        override private[bindingScala] def addChangedListener(listener: ChangedListener[Seq[B]]): Unit = {
          checkForUpstreamSubscription()
          changeForwarder.subscribe(listener)
        }

      }

      /**
        * @group Binding Expressions
        */
      final class FlatMappedSeq[A, B](source: BindingSeq[A], f: A => BindingSeq[B]) extends BindingSeq[B] {
        override private[bindingScala] def get: Seq[B] = ???

        override private[bindingScala] def removePatchedListener(listener: PatchedListener[B]): Unit = ???

        override private[bindingScala] def addPatchedListener(listener: PatchedListener[B]): Unit = ???

        override private[bindingScala] def addChangedListener(listener: ChangedListener[Seq[B]]): Unit = ???

        override private[bindingScala] def removeChangedListener(listener: ChangedListener[Seq[B]]): Unit = ???
      }

    }

  }

}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
sealed trait Binding[+A] {

  private[bindingScala] def get: A

  private[bindingScala] def removeChangedListener(listener: Binding.ChangedListener[A]): Unit

  private[bindingScala] def addChangedListener(listener: Binding.ChangedListener[A]): Unit

}

