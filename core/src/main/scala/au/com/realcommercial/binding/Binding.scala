package au.com.realcommercial.binding


import java.util.EventObject

import com.thoughtworks.each.Monadic._

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.collection.{GenSeq, mutable, immutable}
import scalaz.Monad
import scala.language.experimental.macros

/**
  * @groupname typeClasses Type class instance
  * @groupname implicits Implicits Conversions
  * @groupname internal Implementation Details
  * @groupdesc internal Implementation details for internal usage only
  * @groupname expressions Binding Expressions
  * @groupdesc expressions AST nodes of binding expressions
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object Binding {

  private[binding] final class Publisher[Subscriber] extends collection.mutable.HashMap[Subscriber, Int] {

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
  private[binding] final class ChangedEvent[+Value](source: AnyRef,
                                                    val oldValue: Value,
                                                    val newValue: Value) extends EventObject(source) {
    override def toString = raw"""ChangedEvent[source=$source oldValue=$oldValue newValue=$newValue]"""

  }

  /**
    * @group Events
    */
  private[binding] final class PatchedEvent[+Element](source: AnyRef,
                                                      val oldSeq: Seq[Element],
                                                      val from: Int,
                                                      val that: GenSeq[Element],
                                                      val replaced: Int) extends EventObject(source) {
    override def toString = raw"""PatchedEvent[source=$source oldSeq=$oldSeq from=$from that=$that replaced=$replaced]"""
  }

  /**
    * @group Listeners
    */
  private[binding] trait ChangedListener[-Value] {
    private[binding] def changed(event: ChangedEvent[Value]): Unit
  }

  /**
    * @group Listeners
    */
  private[binding] trait PatchedListener[-Element] {
    private[binding] def patched(event: PatchedEvent[Element]): Unit
  }

  /**
    * @group expressions
    */
  final case class Constant[+A](override val get: A) extends Binding[A] {
    override private[binding] def removeChangedListener(listener: ChangedListener[A]): Unit = {
      // Do nothing because this Constant never changes
    }

    override private[binding] def addChangedListener(listener: ChangedListener[A]): Unit = {
      // Do nothing because this Constant never changes
    }
  }

  /**
    * @group expressions
    */
  object Var {
    def apply[A](initialValue: A) = new Var(initialValue)
  }

  /**
    * @group expressions
    */
  final class Var[A](private var value: A) extends Binding[A] {

    private val publisher = new Publisher[ChangedListener[A]]

    override def get: A = {
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

    override private[binding] def removeChangedListener(listener: ChangedListener[A]): Unit = {
      publisher.unsubscribe(listener)
    }

    override private[binding] def addChangedListener(listener: ChangedListener[A]): Unit = {
      publisher.subscribe(listener)
    }
  }

  private final class FlatMap[A, B](upstream: Binding[A], f: A => Binding[B])
    extends Binding[B] with ChangedListener[B] {

    private val publisher = new Publisher[ChangedListener[B]]

    private val forwarder = new ChangedListener[A] {

      override final def changed(event: ChangedEvent[A]): Unit = {
        cache.removeChangedListener(FlatMap.this)
        val newCache = f(event.newValue)
        newCache.addChangedListener(FlatMap.this)
        if (cache.get != newCache.get) {
          for ((listener, _) <- publisher) {
            listener.changed(new ChangedEvent(FlatMap.this, cache.get, newCache.get))
          }
        }
        cache = newCache
      }
    }

    override private[binding] final def changed(event: ChangedEvent[B]) = {
      for ((listener, _) <- publisher) {
        listener.changed(new ChangedEvent(FlatMap.this, event.oldValue, event.newValue))
      }
    }

    override private[binding] def addChangedListener(listener: ChangedListener[B]): Unit = {
      if (publisher.isEmpty) {
        upstream.addChangedListener(forwarder)
        cache.addChangedListener(this)
      }
      publisher.subscribe(listener)
    }

    private var cache: Binding[B] = f(upstream.get)

    override private[binding] final def get: B = {
      @tailrec
      def tailrecGetValue(binding: Binding[B]): B = {
        binding match {
          case flatMap: FlatMap[_, B] => tailrecGetValue(flatMap.cache)
          case _ => binding.get
        }
      }
      tailrecGetValue(cache)
    }

    override private[binding] def removeChangedListener(listener: ChangedListener[B]): Unit = {
      publisher.unsubscribe(listener)
      if (publisher.isEmpty) {
        upstream.removeChangedListener(forwarder)
        cache.removeChangedListener(this)
      }
    }

  }

  /**
    * @group typeClasses
    */
  implicit object BindingInstances extends Monad[Binding] {
    override def bind[A, B](fa: Binding[A])(f: (A) => Binding[B]): Binding[B] = {
      fa match {
        case Constant(a) =>
          f(a)
        case _ =>
          new FlatMap[A, B](fa, f)
      }
    }

    override def point[A](a: => A): Binding[A] = Constant(a)
  }

  private[binding] object Macros {

    def map(c: scala.reflect.macros.blackbox.Context)(f: c.Tree): c.Tree = {
      import c.universe._
      val apply@Apply(TypeApply(Select(self, TermName("map")), List(b)), List(f@Function(vparams, body))) = c.macroApplication
      val monadicBody =
        q"""_root_.com.thoughtworks.each.Monadic.monadic[
          _root_.au.com.realcommercial.binding.Binding
        ].apply[$b]($body)"""
      val monadicFunction = atPos(f.pos)(Function(vparams, monadicBody))
      atPos(apply.pos)( q"""$self.mapBinding[$b]($monadicFunction)""")
    }

    def flatMap(c: scala.reflect.macros.blackbox.Context)(f: c.Tree): c.Tree = {
      import c.universe._
      val apply@Apply(TypeApply(Select(self, TermName("flatMap")), List(b)), List(f@Function(vparams, body))) = c.macroApplication
      val monadicBody =
        q"""_root_.com.thoughtworks.each.Monadic.monadic[
          _root_.au.com.realcommercial.binding.Binding
        ].apply[_root_.au.com.realcommercial.binding.Binding.BindingSeq[$b]]($body)"""
      val monadicFunction = atPos(f.pos)(Function(vparams, monadicBody))
      atPos(apply.pos)( q"""$self.flatMapBinding[$b]($monadicFunction)""")
    }

    def withFilter(c: scala.reflect.macros.blackbox.Context)(condition: c.Tree): c.Tree = {
      import c.universe._
      val apply@Apply(Select(self, TermName("withFilter")), List(f@Function(vparams, body))) = c.macroApplication
      val monadicBody =
        q"""_root_.com.thoughtworks.each.Monadic.monadic[
          _root_.au.com.realcommercial.binding.Binding
        ].apply[_root_.scala.Boolean]($body)"""
      val monadicFunction = atPos(f.pos)(Function(vparams, monadicBody))
      atPos(apply.pos)( q"""$self.withFilterBinding($monadicFunction)""")
    }

  }


  private[binding] case class SingleSeq[+A](element: A) extends collection.immutable.IndexedSeq[A] {

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


  private[binding] object Empty extends BindingSeq[Nothing] {
    override private[binding] def removePatchedListener(listener: PatchedListener[Nothing]): Unit = {}

    override private[binding] def addPatchedListener(listener: PatchedListener[Nothing]): Unit = {}

    override private[binding] def get = Nil

    override private[binding] def removeChangedListener(listener: ChangedListener[Seq[Nothing]]): Unit = {}

    override private[binding] def addChangedListener(listener: ChangedListener[Seq[Nothing]]): Unit = {}
  }

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

  private[binding] final class MapBinding[A, B](upstream: BindingSeq[A], f: A => Binding[B]) extends BindingSeq[B] {

    var cache: Vector[Binding[B]] = {
      (for {
        a <- upstream.get
      } yield f(a)) (collection.breakOut(Vector.canBuildFrom))
    }

    override private[binding] def get: Seq[B] = new ValueProxy(cache)

    private[binding] val changedPublisher = new Publisher[ChangedListener[Seq[B]]]

    private val upstreamListener = new PatchedListener[A] with ChangedListener[Seq[A]] {
      override def patched(event: PatchedEvent[A]): Unit = {
        val mappedNewChildren = (for {
          child <- event.that
        } yield f(child)) (collection.breakOut(Seq.canBuildFrom))
        for ((listener, _) <- patchedPublisher) {
          listener.patched(new PatchedEvent(MapBinding.this, new ValueProxy(cache), event.from, new ValueProxy(mappedNewChildren), event.replaced))
        }
        for (oldChild <- cache.view(event.from, event.replaced)) {
          oldChild.removeChangedListener(childListener)
        }
        for (newChild <- mappedNewChildren) {
          newChild.addChangedListener(childListener)
        }
        cache = cache.patch(event.from, mappedNewChildren, event.replaced)
      }

      override def changed(event: ChangedEvent[Seq[A]]): Unit = {
        val newCache = (for {
          a <- event.newValue
        } yield f(a)) (collection.breakOut(Vector.canBuildFrom))
        for ((listener, _) <- changedPublisher) {
          listener.changed(new ChangedEvent(MapBinding.this, new ValueProxy(cache), new ValueProxy(newCache)))
        }
        for (oldChild <- cache) {
          oldChild.removeChangedListener(childListener)
        }
        for (newChild <- newCache) {
          newChild.addChangedListener(childListener)
        }
        cache = newCache
      }
    }

    private[binding] val patchedPublisher = new Publisher[PatchedListener[B]]

    private val childListener = new ChangedListener[B] {

      override def changed(event: ChangedEvent[B]): Unit = {
        val index = cache.indexOf(event.getSource)
        for ((listener, _) <- patchedPublisher) {
          listener.patched(new PatchedEvent(MapBinding.this, new ValueProxy(cache), index, SingleSeq(event.newValue), 1))
        }
      }
    }

    private def checkForUpstreamSubscription(): Unit = {
      if (patchedPublisher.isEmpty && changedPublisher.isEmpty) {
        upstream.addChangedListener(upstreamListener)
        upstream.addPatchedListener(upstreamListener)
        for (child <- cache) {
          child.addChangedListener(childListener)
        }
      }
    }

    private def checkForUpstreamUnsubscription(): Unit = {
      if (patchedPublisher.isEmpty && changedPublisher.isEmpty) {
        upstream.removeChangedListener(upstreamListener)
        upstream.removePatchedListener(upstreamListener)
        for (child <- cache) {
          child.removeChangedListener(childListener)
        }
      }
    }

    override private[binding] def removePatchedListener(listener: PatchedListener[B]): Unit = {
      patchedPublisher.unsubscribe(listener)
      checkForUpstreamUnsubscription()
    }

    override private[binding] def addPatchedListener(listener: PatchedListener[B]): Unit = {
      checkForUpstreamSubscription()
      patchedPublisher.subscribe(listener)
    }

    override private[binding] def removeChangedListener(listener: ChangedListener[Seq[B]]): Unit = {
      changedPublisher.unsubscribe(listener)
      checkForUpstreamUnsubscription()
    }

    override private[binding] def addChangedListener(listener: ChangedListener[Seq[B]]): Unit = {
      checkForUpstreamSubscription()
      changedPublisher.subscribe(listener)
    }

  }

  private[binding] final class FlatProxy[B](underlying: Seq[BindingSeq[B]]) extends Seq[B] {

    override def length: Int = {
      underlying.view.map(_.get.length).sum
    }

    override def apply(idx: Int): B = {
      val i = underlying.iterator
      @tailrec
      def findIndex(restIndex: Int): B = {
        if (i.hasNext) {
          val subSeq = i.next().get
          val currentLength = subSeq.length
          if (currentLength > restIndex) {
            subSeq(restIndex)
          } else {
            findIndex(restIndex - currentLength)
          }
        } else {
          throw new IndexOutOfBoundsException()
        }
      }
      findIndex(idx)
    }

    override def iterator: Iterator[B] = {
      for {
        subSeq <- underlying.iterator
        element <- subSeq.get.iterator
      } yield element
    }
  }

  private[binding] final class FlatMappedSeq[A, B](upstream: BindingSeq[A], f: A => BindingSeq[B]) extends BindingSeq[B] {

    var cache: Vector[BindingSeq[B]] = {
      (for {
        a <- upstream.get
      } yield f(a)) (collection.breakOut(Vector.canBuildFrom))
    }

    @inline
    override private[binding] def get = new FlatProxy(cache)

    private[binding] val changedPublisher = new Publisher[ChangedListener[Seq[B]]]

    @inline
    private def flatIndex(upstreamBegin: Int, upstreamEnd: Int): Int = {
      cache.view(upstreamBegin, upstreamEnd).map(_.get.length).sum
    }

    private val upstreamListener = new PatchedListener[A] with ChangedListener[Seq[A]] {
      override private[binding] def patched(event: PatchedEvent[A]): Unit = {
        val mappedNewChildren = (for {
          child <- event.that
        } yield f(child)) (collection.breakOut(Seq.canBuildFrom))
        val flatNewChildren = new FlatProxy(mappedNewChildren)
        if (event.replaced != 0 || flatNewChildren.nonEmpty) {
          for ((listener, _) <- patchedPublisher) {
            val flattenFrom = flatIndex(0, event.from)
            val flattenReplaced = flatIndex(event.from, event.from + event.replaced)
            listener.patched(new PatchedEvent(FlatMappedSeq.this, get, flattenFrom, flatNewChildren, flattenReplaced))
          }
          for (oldChild <- cache.view(event.from, event.replaced)) {
            oldChild.removeChangedListener(childListener)
            oldChild.removePatchedListener(childListener)
          }
          for (newChild <- mappedNewChildren) {
            newChild.addChangedListener(childListener)
            newChild.addPatchedListener(childListener)
          }
          cache = cache.patch(event.from, mappedNewChildren, event.replaced)
        }
      }

      override private[binding] def changed(event: ChangedEvent[Seq[A]]): Unit = {
        val newCache = (for {
          a <- event.newValue
        } yield f(a)) (collection.breakOut(Vector.canBuildFrom))
        for ((listener, _) <- changedPublisher) {
          listener.changed(new ChangedEvent(FlatMappedSeq.this, get, new FlatProxy(newCache)))
        }
        for (oldChild <- cache) {
          oldChild.removeChangedListener(childListener)
          oldChild.removePatchedListener(childListener)
        }
        for (newChild <- newCache) {
          newChild.addChangedListener(childListener)
          newChild.addPatchedListener(childListener)
        }
        cache = newCache
      }
    }

    private[binding] val patchedPublisher = new Publisher[PatchedListener[B]]

    private val childListener = new PatchedListener[B] with ChangedListener[Seq[B]] {

      override private[binding] def changed(event: ChangedEvent[Seq[B]]): Unit = {
        val index = flatIndex(0, cache.indexOf(event.getSource))
        for ((listener, _) <- patchedPublisher) {
          listener.patched(new PatchedEvent(FlatMappedSeq.this, get, index, event.newValue, event.oldValue.length))
        }
      }

      override private[binding] def patched(event: PatchedEvent[B]): Unit = {
        val source = event.getSource.asInstanceOf[BindingSeq[B]]
        val index = flatIndex(0, cache.indexOf(source)) + event.from
        for ((listener, _) <- patchedPublisher) {
          listener.patched(new PatchedEvent(FlatMappedSeq.this, get, index, event.that, event.replaced))
        }
      }
    }

    private def checkForUpstreamSubscription(): Unit = {
      if (patchedPublisher.isEmpty && changedPublisher.isEmpty) {
        upstream.addChangedListener(upstreamListener)
        upstream.addPatchedListener(upstreamListener)
        for (child <- cache) {
          child.addChangedListener(childListener)
          child.addPatchedListener(childListener)
        }
      }
    }

    private def checkForUpstreamUnsubscription(): Unit = {
      if (patchedPublisher.isEmpty && changedPublisher.isEmpty) {
        upstream.removeChangedListener(upstreamListener)
        upstream.removePatchedListener(upstreamListener)
        for (child <- cache) {
          child.removeChangedListener(childListener)
          child.addPatchedListener(childListener)
        }
      }
    }

    override private[binding] def removePatchedListener(listener: PatchedListener[B]): Unit = {
      patchedPublisher.unsubscribe(listener)
      checkForUpstreamUnsubscription()
    }

    override private[binding] def addPatchedListener(listener: PatchedListener[B]): Unit = {
      checkForUpstreamSubscription()
      patchedPublisher.subscribe(listener)
    }

    override private[binding] def removeChangedListener(listener: ChangedListener[Seq[B]]): Unit = {
      changedPublisher.unsubscribe(listener)
      checkForUpstreamUnsubscription()
    }

    override private[binding] def addChangedListener(listener: ChangedListener[Seq[B]]): Unit = {
      checkForUpstreamSubscription()
      changedPublisher.subscribe(listener)
    }
  }

  /**
    * @group expressions
    */
  sealed trait BindingSeq[+A] extends Binding[Seq[A]] {

    private[binding] def removePatchedListener(listener: PatchedListener[A]): Unit

    private[binding] def addPatchedListener(listener: PatchedListener[A]): Unit

    def map[B](f: A => B): BindingSeq[B] = macro Macros.map

    def flatMap[B](f: A => BindingSeq[B]): BindingSeq[B] = macro Macros.flatMap

    @inline
    final def mapBinding[B](f: A => Binding[B]): BindingSeq[B] = new MapBinding[A, B](this, f)

    @inline
    final def flatMapBinding[B](f: A => Binding[BindingSeq[B]]): BindingSeq[B] = {
      new FlatMappedSeq[A, B](this, { a =>
        new FlatMappedSeq[BindingSeq[B], B](new MapBinding[Unit, BindingSeq[B]](Constants(()), _ => f(a)), locally)
      })
    }

    def withFilter(condition: A => Boolean): WithFilter = macro Macros.withFilter

    @inline
    final def withFilterBinding(condition: A => Binding[Boolean]): WithFilter = {
      new WithFilter(condition)
    }

    sealed class WithFilter(condition: A => Binding[Boolean]) {

      def map[B](f: A => B): BindingSeq[B] = macro Macros.map

      def flatMap[B](f: A => BindingSeq[B]): BindingSeq[B] = macro Macros.flatMap

      def withFilter(condition: A => Boolean): WithFilter = macro Macros.withFilter

      final def withFilterBinding(nextCondition: A => Binding[Boolean]): WithFilter = {
        new WithFilter({ a =>
          monadic[Binding] {
            if (condition(a).each) {
              nextCondition(a).each
            } else {
              false
            }
          }
        })
      }

      final def mapBinding[B](f: (A) => Binding[B]): BindingSeq[B] = {
        BindingSeq.this.flatMapBinding { a =>
          monadic[Binding] {
            if (condition(a).each) {
              Constants(f(a).each)
            } else {
              Empty
            }
          }
        }
      }

      final def flatMapBinding[B](f: (A) => Binding[BindingSeq[B]]): BindingSeq[B] = {
        BindingSeq.this.flatMapBinding { a =>
          monadic[Binding] {
            if (condition(a).each) {
              f(a).each
            } else {
              Empty
            }
          }
        }
      }

    }

  }


  /**
    * @group expressions
    */
  final case class Constants[+A](override val get: A*) extends BindingSeq[A] {

    override private[binding] def removePatchedListener(listener: PatchedListener[A]): Unit = {}

    override private[binding] def addPatchedListener(listener: PatchedListener[A]): Unit = {}

    override private[binding] def removeChangedListener(listener: ChangedListener[Seq[A]]): Unit = {}

    override private[binding] def addChangedListener(listener: ChangedListener[Seq[A]]): Unit = {}

  }

  /**
    * @group expressions
    */
  object Vars {

    def apply[A](initialValues: A*) = new Vars(Vector(initialValues: _*))

    def empty[A] = new Vars(Vector.empty[A])

  }

  /**
    * @group expressions
    */
  final class Vars[A] private(private var cache: Vector[A]) extends BindingSeq[A] {

    private[binding] val patchedPublisher = new Publisher[PatchedListener[A]]

    private[binding] val changedPublisher = new Publisher[ChangedListener[Seq[A]]]

    override def get: mutable.Buffer[A] = new Proxy

    def reset(newValues: A*): Unit = {
      val newCache = Vector(newValues: _*)
      for ((listener, _) <- changedPublisher) {
        listener.changed(new ChangedEvent[Seq[A]](Vars.this, cache, newCache))
      }
      cache = newCache
    }

    private[binding] final class Proxy extends mutable.Buffer[A] {
      override def apply(n: Int): A = {
        cache.apply(n)
      }

      override def update(n: Int, newelem: A): Unit = {
        for {
          (listener, _) <- patchedPublisher
        } {
          listener.patched(new PatchedEvent(Vars.this, cache, n, SingleSeq(newelem), 1))
        }
        cache = cache.updated(n, newelem)
      }

      override def clear(): Unit = {
        for {
          (listener, _) <- patchedPublisher
        } yield {
          listener.patched(new PatchedEvent(Vars.this, cache, 0, Nil, cache.length))
        }
        cache = Vector.empty
      }

      override def length: Int = {
        cache.length
      }

      override def remove(n: Int): A = {
        for {
          (listener, _) <- patchedPublisher
        } yield {
          listener.patched(new PatchedEvent(Vars.this, cache, n, Nil, 1))
        }
        val result = cache(n)
        cache = cache.patch(n, Nil, 1)
        result
      }

      override def +=:(elem: A): this.type = {
        for {
          (listener, _) <- patchedPublisher
        } yield {
          listener.patched(new PatchedEvent(Vars.this, cache, 0, SingleSeq(elem), 0))
        }
        cache = elem +: cache
        Proxy.this
      }

      override def +=(elem: A): this.type = {
        for {
          (listener, _) <- patchedPublisher
        } yield {
          listener.patched(new PatchedEvent(Vars.this, cache, cache.length, SingleSeq(elem), 0))
        }
        cache = cache :+ elem
        Proxy.this
      }

      override def insertAll(n: Int, elems: Traversable[A]): Unit = {
        val seq = elems.toSeq
        for {
          (listener, _) <- patchedPublisher
        } {
          listener.patched(new PatchedEvent(Vars.this, cache, n, seq, 0))
        }
        cache.patch(n, seq, 0)
      }

      override def iterator: Iterator[A] = {
        cache.iterator
      }
    }

    override private[binding] def removeChangedListener(listener: ChangedListener[Seq[A]]): Unit = {
      changedPublisher.unsubscribe(listener)
    }

    override private[binding] def addChangedListener(listener: ChangedListener[Seq[A]]): Unit = {
      changedPublisher.subscribe(listener)
    }

    override private[binding] def removePatchedListener(listener: PatchedListener[A]): Unit = {
      patchedPublisher.unsubscribe(listener)
    }

    override private[binding] def addPatchedListener(listener: PatchedListener[A]): Unit = {
      patchedPublisher.subscribe(listener)
    }

  }

  /**
    * @group expressions
    */
  sealed trait MountPoint extends Binding[Unit] {

    private var referenceCount = 0

    private[binding] def mount(): Unit

    private[binding] def unmount(): Unit

    override private[binding] def addChangedListener(listener: ChangedListener[Unit]): Unit = {
      if (referenceCount == 0) {
        mount()
      }
      referenceCount += 1
    }

    override private[binding] def removeChangedListener(listener: ChangedListener[Unit]): Unit = {
      referenceCount -= 1
      if (referenceCount == 0) {
        unmount()
      }
    }

    override private[binding] def get: Unit = ()

  }

  /**
    * @group expressions
    */
  abstract class MultiMountPoint[Element](upstream: BindingSeq[Element]) extends MountPoint {

    private[binding] final def mount(): Unit = {
      set(upstream.get)
      upstream.addChangedListener(upstreamListener)
      upstream.addPatchedListener(upstreamListener)
    }

    private[binding] final def unmount(): Unit = {
      upstream.removeChangedListener(upstreamListener)
      upstream.removePatchedListener(upstreamListener)
    }

    protected def set(newValue: Seq[Element]): Unit

    protected def splice(oldSeq: Seq[Element], from: Int, that: GenSeq[Element], replaced: Int): Unit

    private val upstreamListener = new ChangedListener[Seq[Element]] with PatchedListener[Element] {

      override private[binding] def changed(event: ChangedEvent[Seq[Element]]): Unit = {
        set(event.newValue)
      }

      override private[binding] def patched(event: PatchedEvent[Element]): Unit = {
        splice(event.oldSeq, event.from, event.that, event.replaced)
      }

    }

  }

  /**
    * @group expressions
    */
  abstract class SingleMountPoint[Value](upstream: Binding[Value]) extends MountPoint {

    protected def set(value: Value): Unit

    private var referenceCount = 0

    private[binding] final def mount(): Unit = {
      set(upstream.get)
      upstream.addChangedListener(upstreamListener)
    }

    private[binding] final def unmount(): Unit = {
      upstream.removeChangedListener(upstreamListener)
    }

    private val upstreamListener = new ChangedListener[Value] {
      override private[binding] def changed(event: ChangedEvent[Value]): Unit = {
        set(event.newValue)
      }
    }

  }

  /**
    * @group implicits
    */
  @inline
  implicit def eachOps[A](binding: Binding[A]): EachOps[Binding, A] = new EachOps[Binding, A](binding)

  private[binding] object DummyListener extends ChangedListener[Any] {
    override private[binding] def changed(event: ChangedEvent[Any]): Unit = {}
  }

}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
sealed trait Binding[+A] {

  private[binding] def get: A

  private[binding] def removeChangedListener(listener: Binding.ChangedListener[A]): Unit

  private[binding] def addChangedListener(listener: Binding.ChangedListener[A]): Unit

  final def watch(): Unit = {
    addChangedListener(Binding.DummyListener)
  }

  final def unwatch(): Unit = {
    removeChangedListener(Binding.DummyListener)
  }

}

