/*
The MIT License (MIT)

Copyright (c) 2016 Yang Bo & REA Group Ltd.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
 */

package com.thoughtworks.binding

import java.util.EventObject
import scala.annotation.meta.companionMethod
import scala.annotation.tailrec
import scala.collection.SeqView
import scala.collection.View
import scala.collection.mutable.Buffer
import scala.language.higherKinds
import scala.language.implicitConversions

import Binding2Or3.SeqOpsIterable

/** @groupname typeClasses
  * Type class instance
  * @groupname implicits
  * Implicits Conversions
  * @groupname expressions
  * Binding Expressions
  * @groupdesc expressions
  *   AST nodes of binding expressions
  * @author
  *   杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object Binding extends Binding2Or3.Companion {

  /** Monad instances for [[Binding]].
    *
    * @group typeClasses
    */
  @deprecated(
    "Binding.scala for Scala 3 does not provide scalaz instances any more out-of-box.",
    "Binding.scala 12.2.0"
  )
  object BindingInstances extends Binding2Or3.BindingInstances2Or3

  sealed trait Watchable[+A] {
    def watch(): Unit
    def unwatch(): Unit
  }

  private[binding] def addChangedListener[A](binding: Binding[A], listener: ChangedListener[A]) = {
    binding.addChangedListener(listener)
  }

  private[binding] def removeChangedListener[A](binding: Binding[A], listener: ChangedListener[A]) = {
    binding.removeChangedListener(listener)
  }

  import BindingJvmOrJs._

  final class ChangedEvent[+Value](source: Binding[Value], val newValue: Value) extends EventObject(source) {
    override def getSource = super.getSource.asInstanceOf[Binding[Value]]
    override def toString = raw"""ChangedEvent[source=$source newValue=$newValue]"""

  }

  final class PatchedEvent[+Element](
      source: BindingSeq[Element],
      val from: Int,
      val that: SeqOpsIterable[Element],
      val replaced: Int
  ) extends EventObject(source) {
    override def getSource = super.getSource.asInstanceOf[BindingSeq[Element]]
    override def toString = raw"""PatchedEvent[source=$source from=$from that=$that replaced=$replaced]"""
  }

  trait ChangedListener[-Value] {
    def changed(event: ChangedEvent[Value]): Unit
  }

  trait PatchedListener[-Element] {
    def patched(event: PatchedEvent[Element]): Unit
  }

  /** A data binding expression that never changes.
    *
    * @group expressions
    */
  final case class Constant[+A](override val value: A) extends Binding[A] {

    @inline
    override protected def removeChangedListener(listener: ChangedListener[A]): Unit = {
      // Do nothing because this Constant never changes
    }

    @inline
    override protected def addChangedListener(listener: ChangedListener[A]): Unit = {
      // Do nothing because this Constant never changes
    }
  }

  /** @group expressions
    */
  object Var {
    @inline
    def apply[A](initialValue: A) = new Var(initialValue)
  }

  /** Source variable of data binding expression.
    *
    * You can manually change the value:
    *
    * {{{
    * import com.thoughtworks.binding.Binding.Var
    * val bindingVar = Var("initial value")
    * bindingVar.value = "changed value"
    * }}}
    *
    * Then, any data binding expressions that depend on this [[Var]] will be changed automatically.
    *
    * @group expressions
    */
  final class Var[A](private var cache: A) extends Binding[A] {

    private val publisher = new SafeBuffer[ChangedListener[A]]

    @inline
    override def value = cache

    /** Changes the current value of this [[Var]], and reevaluates any expressions that depends on this [[Var]].
      *
      * @note
      *   This method must not be invoked inside a `@dom` method body or a `Binding { ... }` block.
      */
    def value_=(newValue: A): Unit = {
      if (cache.isInstanceOf[View[_]] || cache != newValue) {
        cache = newValue
        val event = new ChangedEvent(this, newValue)
        for (listener <- publisher) {
          listener.changed(event)
        }
      }
    }

    @inline
    override protected def removeChangedListener(listener: ChangedListener[A]): Unit = {
      publisher.-=(listener)
    }

    @inline
    override protected def addChangedListener(listener: ChangedListener[A]): Unit = {
      publisher.+=(listener)
    }
  }

  /** @group expressions
    */
  final class Map[A, B](upstream: Binding[A], f: A => B) extends Binding[B] with ChangedListener[A] {

    private val publisher = new SafeBuffer[ChangedListener[B]]

    private var cache: B = _

    private def refreshCache() = {
      cache = f(upstream.value)
    }

    @inline
    override protected def value: B = {
      cache
    }

    @inline
    override protected def addChangedListener(listener: ChangedListener[B]): Unit = {
      if (publisher.isEmpty) {
        upstream.addChangedListener(this)
        refreshCache()
      }
      publisher.+=(listener)
    }

    @inline
    override protected def removeChangedListener(listener: ChangedListener[B]): Unit = {
      publisher.-=(listener)
      if (publisher.isEmpty) {
        upstream.removeChangedListener(this)
      }
    }

    override final def changed(upstreamEvent: ChangedEvent[A]): Unit = {
      val oldCache = cache
      val newCache = f(upstreamEvent.newValue)
      cache = newCache
      if (oldCache.isInstanceOf[View[_]] || oldCache != newCache) {
        val event = new ChangedEvent(Map.this, newCache)
        for (listener <- publisher) {
          listener.changed(event)
        }
      }
    }
  }

  private val ReentryDetector = new Binding[Nothing] {
    protected def throwException(): Nothing =
      throw new IllegalStateException(
        "Must not change an upstream value in a data binding expression that depends on the same upstream value!"
      )
    def value: Nothing = throwException()

    protected def removeChangedListener(listener: ChangedListener[Nothing]): Unit = throwException()

    protected def addChangedListener(listener: ChangedListener[Nothing]): Unit = throwException()
  }

  /** @group expressions
    */
  final class FlatMap[A, B](upstream: Binding[A], f: A => Binding[B]) extends Binding[B] with ChangedListener[B] {

    private val publisher = new SafeBuffer[ChangedListener[B]]

    private val forwarder = new ChangedListener[A] {

      override final def changed(upstreamEvent: ChangedEvent[A]): Unit = {
        val oldCache = cache
        oldCache.removeChangedListener(FlatMap.this)
        val newCache = f(upstreamEvent.newValue)
        cache = newCache
        newCache.addChangedListener(FlatMap.this)
        if (oldCache.isInstanceOf[View[_]] || oldCache.value != newCache.value) {
          val event = new ChangedEvent(FlatMap.this, newCache.value)
          for (listener <- publisher) {
            listener.changed(event)
          }
        }
      }
    }

    @inline
    override def changed(upstreamEvent: ChangedEvent[B]) = {
      val event = new ChangedEvent(FlatMap.this, upstreamEvent.newValue)
      for (listener <- publisher) {
        listener.changed(event)
      }
    }

    @inline
    override def addChangedListener(listener: ChangedListener[B]): Unit = {
      if (publisher.isEmpty) {
        upstream.addChangedListener(forwarder)
        refreshCache()
        cache.addChangedListener(this)
      }
      publisher.+=(listener)
    }

    private var cache: Binding[B] = ReentryDetector

    private def refreshCache() = {
      cache = f(upstream.value)
    }

    override protected def value: B = {
      @tailrec
      @inline
      def tailrecGetValue(binding: Binding[B]): B = {
        binding match {
          case flatMap: FlatMap[_, _] => tailrecGetValue(flatMap.cache)
          case _                      => binding.value
        }
      }
      tailrecGetValue(cache)
    }

    override protected def removeChangedListener(listener: ChangedListener[B]): Unit = {
      publisher.-=(listener)
      if (publisher.isEmpty) {
        upstream.removeChangedListener(forwarder)
        cache.removeChangedListener(this)
      }
    }

  }

  private[binding] case class SingleSeq[+A](element: A) extends IndexedSeq[A] {

    @inline
    override final def length: Int = 1

    @inline
    override final def apply(idx: Int) = {
      if (idx == 0) {
        element
      } else {
        throw new IndexOutOfBoundsException
      }
    }

    @inline
    override final def iterator = Iterator.single(element)

  }

  private[binding] val Empty = new BindingSeq[Nothing] {
    @inline
    override protected def removePatchedListener(listener: PatchedListener[Nothing]): Unit = {}

    @inline
    override protected def addPatchedListener(listener: PatchedListener[Nothing]): Unit = {}

    type All[+A] = List[A]

    @inline
    override protected def value = Nil
  }

  private[Binding] abstract class ValueProxy[B] extends SeqView[B] with HasCache[Binding[B]] {

    protected def underlying = cacheData

    @inline
    override def length: Int = {
      cacheData.length
    }

    @inline
    override def apply(idx: Int): B = {
      cacheData(idx).value
    }

    @inline
    override def iterator: Iterator[B] = {
      cacheData.iterator.map(_.value)
    }
  }

  /** The companion of a data binding expression of a sequence
    *
    * @group expressions
    */
  object BindingSeq {

    /** A helper to build complicated comprehension expressions for [[BindingSeq]]
      */
    abstract class WithFilter[+A] private[BindingSeq] extends Binding2Or3.BindingSeq2Or3.WithFilter2Or3[A] {

      /** Underlying implementation of [[withFilter.
        *
        * @note
        *   Don't use this method in user code.
        */
      def withFilterBinding(nextCondition: A => Binding[Boolean]): WithFilter[A]

      /** Underlying implementation of [[map]].
        *
        * @note
        *   Don't use this method in user code.
        */
      def mapBinding[B](f: (A) => Binding[B]): BindingSeq[B]

      /** Underlying implementation of [[flatMap]].
        *
        * @note
        *   Don't use this method in user code.
        */
      @inline
      def flatMapBinding[B](f: (A) => Binding[BindingSeq[B]]): BindingSeq[B]
    }

    private[binding] final class FlatProxy[B](protected val underlying: collection.Seq[BindingSeq[B]])
        extends SeqView[B] {

      @inline
      override def length: Int = {
        underlying.view.map(_.value.length).sum
      }

      @inline
      override def apply(idx: Int): B = {
        val i = underlying.iterator
        @tailrec
        def findIndex(restIndex: Int): B = {
          if (i.hasNext) {
            val subSeq = i.next().value
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

      @inline
      override def iterator: Iterator[B] = {
        for {
          subSeq <- underlying.iterator
          element <- subSeq.value.iterator
        } yield element
      }
    }

    private[binding] def addPatchedListener[A](binding: BindingSeq[A], listener: PatchedListener[A]) = {
      binding.addPatchedListener(listener)
    }

    private[binding] def removePatchedListener[A](binding: BindingSeq[A], listener: PatchedListener[A]) = {
      binding.removePatchedListener(listener)
    }

    private[Binding] abstract class MultiMountPoint[-Element](upstream: BindingSeq[Element]) extends MountPoint {

      protected def mount(): Unit = {
        upstream.addPatchedListener(upstreamListener)
        set(upstream.value)
      }

      protected def unmount(): Unit = {
        upstream.removePatchedListener(upstreamListener)
        set(Seq.empty)
      }

      protected def set(children: Iterable[Element]): Unit

      protected def splice(from: Int, that: Iterable[Element], replaced: Int): Unit

      private val upstreamListener = new PatchedListener[Element] {

        @inline
        override def patched(upstreamEvent: PatchedEvent[Element]): Unit = {
          splice(upstreamEvent.from, upstreamEvent.that, upstreamEvent.replaced)
        }

      }

    }

    final class FlatMap[A, B](upstream: BindingSeq[A], f: A => BindingSeq[B])
        extends BindingSeq[B]
        with HasCache[BindingSeq[B]] {

      private[binding] var cacheData: Cache = _

      private def refreshCache() = {
        cacheData = toCacheData(for {
          a <- upstream.value /*.view*/
        } yield f(a))
      }

      type All[+A] = SeqView[A]

      @inline
      override protected def value = new FlatProxy(cacheData)

      @inline
      private def flatIndex(oldCache: Cache, upstreamBegin: Int, upstreamEnd: Int): Int = {
        oldCache.view.slice(upstreamBegin, upstreamEnd).map(_.value.length).sum
      }

      private val upstreamListener = new PatchedListener[A] {
        override def patched(upstreamEvent: PatchedEvent[A]): Unit = {
          val mappedNewChildren: Cache = toCacheData(for {
            child <- upstreamEvent.that /*.view*/
          } yield f(child))
          val flatNewChildren = new FlatProxy(mappedNewChildren)
          val flattenFrom = flatIndex(cacheData, 0, upstreamEvent.from)
          val flattenReplaced = flatIndex(cacheData, upstreamEvent.from, upstreamEvent.from + upstreamEvent.replaced)
          val oldChildren = spliceCache(upstreamEvent.from, mappedNewChildren, upstreamEvent.replaced)
          for (newChild <- mappedNewChildren) {
            newChild.addPatchedListener(childListener)
          }
          for (oldChild <- oldChildren) {
            oldChild.removePatchedListener(childListener)
          }
          if (upstreamEvent.replaced != 0 || flatNewChildren.nonEmpty) {
            val event = new PatchedEvent(FlatMap.this, flattenFrom, flatNewChildren, flattenReplaced)
            for (listener <- publisher) {
              listener.patched(event)
            }
          }
        }

      }

      private[binding] val publisher = new SafeBuffer[PatchedListener[B]]

      private val childListener = new PatchedListener[B] {
        override def patched(upstreamEvent: PatchedEvent[B]): Unit = {
          val source = upstreamEvent.getSource
          val index = flatIndex(cacheData, 0, indexOfCache(source)) + upstreamEvent.from
          val event = new PatchedEvent(FlatMap.this, index, upstreamEvent.that, upstreamEvent.replaced)
          for (listener <- publisher) {
            listener.patched(event)
          }
        }
      }

      @inline
      override protected def removePatchedListener(listener: PatchedListener[B]): Unit = {
        publisher.-=(listener)
        if (publisher.isEmpty) {
          upstream.removePatchedListener(upstreamListener)
          for (child <- cacheData) {
            child.removePatchedListener(childListener)
          }
        }
      }

      @inline
      override protected def addPatchedListener(listener: PatchedListener[B]): Unit = {
        if (publisher.isEmpty) {
          upstream.addPatchedListener(upstreamListener)
          refreshCache()
          for (child <- cacheData) {
            child.addPatchedListener(childListener)
          }
        }
        publisher.+=(listener)
      }
    }

    private[Binding] final class ForeachBinding[A](upstream: BindingSeq[A], f: A => Binding[Any])
        extends MountPoint
        with PatchedListener[A]
        with HasCache[Binding[Any]] {
      private[binding] var cacheData: Cache = _

      private def refreshCache() = {
        cacheData = toCacheData(for {
          a <- upstream.value /*.view*/
        } yield f(a))
      }

      protected def mount(): Unit = {
        upstream.addPatchedListener(this)
        refreshCache()
        for (child <- cacheData) {
          child.watch()
        }

      }
      protected def unmount(): Unit = {
        upstream.removePatchedListener(this)
        for (child <- cacheData) {
          child.unwatch()
        }

      }
      def patched(upstreamEvent: PatchedEvent[A]): Unit = {
        val mappedNewChildren: Cache = toCacheData(for {
          child <- upstreamEvent.that /*.view*/
        } yield f(child))
        val oldChildren = spliceCache(upstreamEvent.from, mappedNewChildren, upstreamEvent.replaced)
        for (newChild <- mappedNewChildren) {
          newChild.watch()
        }
        for (oldChild <- oldChildren) {
          oldChild.unwatch()
        }
      }
    }

    final class MapBinding[A, B](upstream: BindingSeq[A], f: A => Binding[B])
        extends BindingSeq[B]
        with HasCache[Binding[B]] {

      private[binding] var cacheData: Cache = _

      private def refreshCache() = {
        cacheData = toCacheData(for {
          a <- upstream.value /*.view*/
        } yield f(a))
      }

      type All[+A] = SeqView[A]

      override protected def value: SeqView[B] = {
        val cacheData0 = cacheData
        new ValueProxy[B] {
          var cacheData = cacheData0
        }
      }

      private val upstreamListener = new PatchedListener[A] {
        override def patched(upstreamEvent: PatchedEvent[A]): Unit = {
          val mappedNewChildren: Cache = toCacheData(for {
            child <- upstreamEvent.that /*.view*/
          } yield f(child))
          val oldChildren = spliceCache(upstreamEvent.from, mappedNewChildren, upstreamEvent.replaced)
          for (newChild <- mappedNewChildren) {
            newChild.addChangedListener(childListener)
          }
          for (oldChild <- oldChildren) {
            oldChild.removeChangedListener(childListener)
          }
          val proxy = new ValueProxy[B] {
            var cacheData = mappedNewChildren
          }
          val event =
            new PatchedEvent[B](MapBinding.this, upstreamEvent.from, proxy, upstreamEvent.replaced)
          for (listener <- publisher) {
            listener.patched(event)
          }
        }

      }

      private[binding] val publisher = new SafeBuffer[PatchedListener[B]]

      private val childListener = new ChangedListener[B] {

        override def changed(event: ChangedEvent[B]): Unit = {
          val index = indexOfCache(event.getSource)
          for (listener <- publisher) {
            listener.patched(new PatchedEvent(MapBinding.this, index, SingleSeq(event.newValue), 1))
          }
        }
      }

      override protected def removePatchedListener(listener: PatchedListener[B]): Unit = {
        publisher.-=(listener)
        if (publisher.isEmpty) {
          upstream.removePatchedListener(upstreamListener)
          for (child <- cacheData) {
            child.removeChangedListener(childListener)
          }
        }
      }

      override protected def addPatchedListener(listener: PatchedListener[B]): Unit = {
        if (publisher.isEmpty) {
          upstream.addPatchedListener(upstreamListener)
          refreshCache()
          for (child <- cacheData) {
            child.addChangedListener(childListener)
          }
        }
        publisher.+=(listener)
      }

    }

    private[binding] final class Length(bindingSeq: BindingSeq[_]) extends Binding[Int] with PatchedListener[Any] {

      private val publisher = new SafeBuffer[ChangedListener[Int]]

      @inline
      override protected def value: Int = bindingSeq.value.length

      @inline
      override protected def removeChangedListener(listener: ChangedListener[Int]): Unit = {
        publisher.-=(listener)
        if (publisher.isEmpty) {
          bindingSeq.removePatchedListener(this)
        }
      }

      @inline
      override protected def addChangedListener(listener: ChangedListener[Int]): Unit = {
        if (publisher.isEmpty) {
          bindingSeq.addPatchedListener(this)
        }
        publisher.+=(listener)
      }

      @inline
      override def patched(upstreamEvent: PatchedEvent[Any]): Unit = {
        val event = new ChangedEvent[Int](this, bindingSeq.value.length)
        for (subscriber <- publisher) {
          subscriber.changed(event)
        }
      }

    }

  }

  /** Data binding expression of a sequence
    *
    * @group expressions
    */
  trait BindingSeq[+A] extends Watchable[A] with Binding2Or3.BindingSeq2Or3[A] {

    /** Returns a new [[Binding]] expression of all elements in this [[BindingSeq]]. */
    final def all: Binding[All[A]] = new Binding[All[A]] { asBinding =>
      private val patchedListener = new PatchedListener[A] {
        @inline
        def patched(upstreamEvent: PatchedEvent[A]): Unit = {
          val event = new ChangedEvent[All[A]](asBinding, asBinding.value)
          for (listener <- publisher) {
            listener.changed(event)
          }
        }
      }
      private val publisher = new SafeBuffer[ChangedListener[All[A]]]

      @inline
      override protected def value: All[A] = BindingSeq.this.value

      @inline
      override protected def removeChangedListener(listener: ChangedListener[All[A]]): Unit = {
        publisher.-=(listener)
        if (publisher.isEmpty) {
          BindingSeq.this.removePatchedListener(patchedListener)
        }
      }

      @inline
      override protected def addChangedListener(listener: ChangedListener[All[A]]): Unit = {
        if (publisher.isEmpty) {
          BindingSeq.this.addPatchedListener(patchedListener)
        }
        publisher.+=(listener)
      }
    }

    /** Enables automatic recalculation.
      *
      * You may invoke this method more than once. Then, when you want to disable automatic recalculation, you must
      * invoke [[unwatch]] same times as the number of calls to this method.
      *
      * @note
      *   This method is recursive, which means that the dependencies of this [[BindingSeq]] will be watched as well.
      */
    @inline
    final def watch(): Unit = {
      addPatchedListener(Binding.DummyPatchedListener)
    }

    /** Disables automatic recalculation.
      *
      * @note
      *   This method is recursive, which means that the dependencies of this [[BindingSeq]] will be unwatched as well.
      */
    @inline
    final def unwatch(): Unit = {
      removePatchedListener(Binding.DummyPatchedListener)
    }

    /** The value type of [[all]] */
    type All[+A] <: SeqOpsIterable[A]

    /** Returns the current value of this [[BindingSeq]]. */
    protected def value: All[A]

    /** Returns the current value of this [[BindingSeq]].
      *
      * @note
      *   This method is used for internal testing purpose only.
      */
    private[binding] def get: All[A] = value

    protected def removePatchedListener(listener: PatchedListener[A]): Unit

    protected def addPatchedListener(listener: PatchedListener[A]): Unit

    def length: Binding[Int] = new BindingSeq.Length(this)

    def isEmpty: Binding[Boolean] = all.map(_.isEmpty)

    def nonEmpty: Binding[Boolean] = all.map(_.nonEmpty)

    /** The underlying implementation of [[foreach]].
      *
      * @note
      *   Don't use this method in user code.
      */
    @inline
    def foreachBinding[U](f: A => Binding[U]): Binding[Unit] = {
      new BindingSeq.ForeachBinding[A](this, f)
    }

    /** The underlying implementation of [[map]].
      *
      * @note
      *   Don't use this method in user code.
      */
    @inline
    final def mapBinding[B](f: A => Binding[B]): BindingSeq[B] = new BindingSeq.MapBinding[A, B](this, f)

    /** The underlying implementation of [[flatMap]].
      *
      * @note
      *   Don't use this method in user code.
      */
    @inline
    final def flatMapBinding[B](f: A => Binding[BindingSeq[B]]): BindingSeq[B] = {
      new BindingSeq.FlatMap[BindingSeq[B], B](new BindingSeq.MapBinding[A, BindingSeq[B]](this, f), locally)
    }

    /** The underlying implementation of [[withFilter]].
      *
      * @note
      *   Don't use this method in user code.
      */
    @inline
    final def withFilterBinding(condition: A => Binding[Boolean]): BindingSeq.WithFilter[A] = {
      new BindingSeq.WithFilter[A] {

        /** Underlying implementation of [[withFilter.
          *
          * @note
          *   Don't use this method in user code.
          */
        @inline
        def withFilterBinding(nextCondition: A => Binding[Boolean]): BindingSeq.WithFilter[A] = {
          BindingSeq.this.withFilterBinding { (a: A) =>
            condition(a).flatMap {
              case true =>
                nextCondition(a)
              case false =>
                Binding.Constant(false)
            }
          }
        }

        /** Underlying implementation of [[map]].
          *
          * @note
          *   Don't use this method in user code.
          */
        @inline
        def mapBinding[B](f: (A) => Binding[B]): BindingSeq[B] = {
          BindingSeq.this.flatMapBinding { (a: A) =>
            condition(a).flatMap {
              case true =>
                f(a).map(Constants(_))
              case false =>
                Constant(Empty)
            }
          }
        }

        /** Underlying implementation of [[flatMap]].
          *
          * @note
          *   Don't use this method in user code.
          */
        @inline
        def flatMapBinding[B](f: (A) => Binding[BindingSeq[B]]): BindingSeq[B] = {
          BindingSeq.this.flatMapBinding { (a: A) =>
            condition(a).flatMap {
              case true =>
                f(a)
              case false =>
                Constant(Empty)
            }
          }
        }

      }
    }

  }

  /** An data binding expression of sequence that never changes.
    *
    * @group expressions
    */
  final class Constants[+A] private[Binding] (underlying: ConstantsData[A]) extends BindingSeq[A] {
    type All[+A] = collection.Seq[A]

    @inline
    override def value: collection.Seq[A] = underlying

    @inline
    override protected def removePatchedListener(listener: PatchedListener[A]): Unit = {}

    @inline
    override protected def addPatchedListener(listener: PatchedListener[A]): Unit = {}

  }

  /** @group expressions
    */
  object Constants {

    @inline
    def apply[A](elements: A*) = new Constants(toConstantsData(elements))

    @inline
    def upapplySeq[A](constants: Constants[A]) = Some(constants.value)

    private final val Empty = Constants[Nothing]()

    @inline
    def empty[A]: Constants[A] = Empty

  }

  /** @group expressions
    */
  object Vars {

    @inline
    def apply[A](initialValues: A*) = new Vars(toCacheData(initialValues))

    @inline
    def empty[A] = new Vars(emptyCacheData[A])

  }

  /** Source sequence of data binding expression.
    *
    * @group expressions
    */
  final class Vars[A] private (private[binding] var cacheData: HasCache[A]#Cache)
      extends BindingSeq[A]
      with HasCache[A] {

    private[binding] val publisher = new SafeBuffer[PatchedListener[A]]

    type All[+A] = Buffer[_ <: A] with SeqOpsIterable[A]

    /** Returns a [[scala.collection.mutable.Buffer]] that allow you change the content of this [[Vars]].
      *
      * Whenever you change the returned data, other binding expressions that depend on this [[Vars]] will be
      * automatically changed.
      *
      * @note
      *   This method must not be invoked inside a `@dom` method body or a `Binding { ... }` block..
      */
    @inline
    override def value: Buffer[A] = new Proxy

    private[binding] final class Proxy extends Buffer[A] {

      @inline
      override def patchInPlace(from: Int, patch: IterableOnce[A], replaced: Int): this.type = {
        val result = spliceCache(from, patch, replaced)
        for (listener <- publisher) {
          listener.patched(new PatchedEvent(Vars.this, from, result, replaced))
        }
        this
      }

      @inline
      override def apply(n: Int): A = {
        getCache(n)
      }

      @inline
      override def update(n: Int, newelem: A): Unit = {
        updateCache(n, newelem)
        for (listener <- publisher) {
          listener.patched(new PatchedEvent(Vars.this, n, SingleSeq(newelem), 1))
        }
      }

      @inline
      override def clear(): Unit = {
        val oldLength = cacheLength
        clearCache()
        val event = new PatchedEvent[A](Vars.this, 0, List.empty[A], oldLength)
        for (listener <- publisher) {
          listener.patched(event)
        }
      }

      @inline
      override def length: Int = {
        cacheLength
      }

      @inline
      override def remove(n: Int): A = {
        val result = removeCache(n)
        val event = new PatchedEvent[A](Vars.this, n, List.empty[A], 1)
        for (listener <- publisher) {
          listener.patched(event)
        }
        result
      }

      @inline
      override def remove(idx: Int, count: Int): Unit = {
        removeCache(idx, count)
        val event = new PatchedEvent[A](Vars.this, idx, List.empty[A], count)
        for (listener <- publisher) {
          listener.patched(event)
        }
      }

      @inline
      override def addAll(elements: IterableOnce[A]): this.type = {
        val oldLength = cacheLength
        val seq = appendCache(elements)
        for (listener <- publisher) {
          listener.patched(new PatchedEvent(Vars.this, oldLength, seq, 0))
        }
        Proxy.this
      }

      @inline
      override def prepend(elem: A): this.type = {
        prependCache(elem)
        for (listener <- publisher) {
          listener.patched(new PatchedEvent(Vars.this, 0, SingleSeq(elem), 0))
        }
        Proxy.this
      }

      @inline
      override def addOne(elem: A): this.type = {
        val oldLength = cacheLength
        appendCache(elem)
        for (listener <- publisher) {
          listener.patched(new PatchedEvent(Vars.this, oldLength, SingleSeq(elem), 0))
        }
        Proxy.this
      }

      @inline
      override def insert(idx: Int, elem: A): Unit = {
        val seq = insertOneCache(idx, elem)
        for (listener <- publisher) {
          listener.patched(new PatchedEvent(Vars.this, idx, seq, 0))
        }

      }

      @inline
      override def insertAll(n: Int, elems: IterableOnce[A]): Unit = {
        val seq = insertCache(n, elems)
        for (listener <- publisher) {
          listener.patched(new PatchedEvent(Vars.this, n, seq, 0))
        }
      }

      @inline
      override def iterator: Iterator[A] = {
        cacheIterator
      }
    }

    @inline
    override protected def removePatchedListener(listener: PatchedListener[A]): Unit = {
      publisher.-=(listener)
    }

    @inline
    override protected def addPatchedListener(listener: PatchedListener[A]): Unit = {
      publisher.+=(listener)
    }

  }

  /** A [[BindingSeq]] that contains only one element
    *
    * @group expressions
    */
  final case class SingletonBindingSeq[A](upstream: Binding[A]) extends BindingSeq[A] {

    private val publisher = new SafeBuffer[PatchedListener[A]]

    private val changedListener = new ChangedListener[A] {

      override def changed(event: ChangedEvent[A]) = {
        val patchedEvent = new PatchedEvent[A](SingletonBindingSeq.this, 0, SingleSeq(event.newValue), 1)
        for (listener <- publisher) {
          listener.patched(patchedEvent)
        }
      }

    }

    override def length: Constant[Int] = Constant(1)

    type All[+A] = IndexedSeq[A]

    @inline
    override protected def value = SingleSeq(upstream.value)

    @inline
    override protected def removePatchedListener(listener: PatchedListener[A]): Unit = {
      publisher.-=(listener)
      if (publisher.isEmpty) {
        upstream.removeChangedListener(changedListener)
      }
    }

    @inline
    override protected def addPatchedListener(listener: PatchedListener[A]): Unit = {
      if (publisher.isEmpty) {
        upstream.addChangedListener(changedListener)
      }
      publisher.+=(listener)
    }

  }

  /** A mechanism that mounts the result of a data binding expression into DOM or other system.
    *
    * @group expressions
    */
  private[Binding] sealed trait MountPoint extends Binding[Unit] {

    private var referenceCount = 0

    protected def mount(): Unit

    protected def unmount(): Unit

    @inline
    override protected def addChangedListener(listener: ChangedListener[Unit]): Unit = {
      if (referenceCount == 0) {
        mount()
      }
      referenceCount += 1
    }

    @inline
    override protected def removeChangedListener(listener: ChangedListener[Unit]): Unit = {
      referenceCount -= 1
      if (referenceCount == 0) {
        unmount()
      }
    }

    @inline
    override protected def value: Unit = ()

  }

  /** A mechanism that mounts the result of a data binding expression of a sequence into DOM or other system.
    *
    * @group expressions
    */
  abstract class MultiMountPoint[-Element](upstream: BindingSeq[Element]) extends BindingSeq.MultiMountPoint(upstream)

  /** A mechanism that mounts the result of a data binding expression of a single value into DOM or other system.
    *
    * Use this class only if you must override [[mount]] or [[unmount]]. If you only want to override [[set]], you can
    * use `Binding[Unit] { onUpstreamChange(upstream.bind) }` instead.
    *
    * @group expressions
    */
  abstract class SingleMountPoint[-Value](upstream: Binding[Value]) extends MountPoint {

    protected def set(value: Value): Unit

    protected def mount(): Unit = {
      upstream.addChangedListener(upstreamListener)
      set(upstream.value)
    }

    protected def unmount(): Unit = {
      upstream.removeChangedListener(upstreamListener)
    }

    private val upstreamListener = new ChangedListener[Value] {
      @inline
      override def changed(event: ChangedEvent[Value]): Unit = {
        set(event.newValue)
      }
    }

  }

  private[binding] val DummyPatchedListener = new PatchedListener[Any] {
    @inline
    override def patched(event: PatchedEvent[Any]): Unit = {}
  }

  private[binding] val DummyChangedListener = new ChangedListener[Any] {
    @inline
    override def changed(event: ChangedEvent[Any]): Unit = {}
  }

  private class RxDefer[A](upstream: => Rx.Observable[A]) extends Rx.Observable[A] with ChangedListener[Option[A]] {

    def changed(upstream: ChangedEvent[Option[A]]): Unit = {
      val event = new ChangedEvent(this, upstream.newValue)
      for (listener <- publisher) {
        listener.changed(event)
      }
    }

    private var upstreamCache: Rx.Observable[A] = _

    private val publisher = new SafeBuffer[ChangedListener[Option[A]]]

    override protected def value: Option[A] = {
      if (publisher.isEmpty) {
        None
      } else {
        upstreamCache.value
      }
    }

    override protected def removeChangedListener(listener: ChangedListener[Option[A]]): Unit = {
      publisher -= listener
      if (publisher.isEmpty) {
        val upstreamLocal = upstreamCache
        upstreamCache = null
        upstreamLocal.removeChangedListener(this)
      }
    }

    override protected def addChangedListener(listener: ChangedListener[Option[A]]): Unit = {
      if (publisher.isEmpty) {
        val upstreamLocal = upstream
        upstreamLocal.addChangedListener(this)
        upstreamCache = upstreamLocal
      }
      publisher += listener
    }

  }

  private class RxMerge[A](upstream: BindingSeq[A]) extends Rx.Observable[A] with PatchedListener[A] {

    private var cache: Option[A] = None

    override def patched(upstreamEvent: PatchedEvent[A]): Unit = {
      upstreamEvent.that.headOption match {
        case None =>
          if (cache != None && upstream.get.isEmpty) {
            cache = None
            val event = new ChangedEvent[Option[A]](this, None)
            for (listener <- publisher) {
              listener.changed(event)
            }
          }
        case someNew =>
          if (cache != someNew) {
            cache = someNew
            val event = new ChangedEvent[Option[A]](this, someNew)
            for (listener <- publisher) {
              listener.changed(event)
            }
          }
      }

    }

    private val publisher = new SafeBuffer[ChangedListener[Option[A]]]
    protected def value: Option[A] = cache
    protected def addChangedListener(listener: ChangedListener[Option[A]]): Unit = {
      if (publisher.isEmpty) {
        BindingSeq.addPatchedListener(upstream, this)
        cache = upstream.get.headOption
      }
      publisher += listener
    }

    protected def removeChangedListener(listener: ChangedListener[Option[A]]): Unit = {
      publisher -= listener
      if (publisher.isEmpty) {
        BindingSeq.removePatchedListener(upstream, this)
      }
    }

  }

  private final class RxToBindingSeq[A](observable: Rx.Observable[A])
      extends BindingSeq[A]
      with ChangedListener[Option[A]]
      with HasCache[A] {

    override def changed(upstreamEvent: ChangedEvent[Option[A]]): Unit = {
      val oldLength = cacheLength
      val event = upstreamEvent.newValue match {
        case Some(newValue) =>
          appendCache(newValue)
          new PatchedEvent(this, oldLength, Seq(newValue), 0)
        case None =>
          clearCache()
          new PatchedEvent(this, 0, Nil, oldLength)
      }
      for (listener <- publisher) {
        listener.patched(event)
      }
    }

    private val publisher = new SafeBuffer[PatchedListener[A]]
    type All[+A] = SeqOpsIterable[A]

    private[binding] var cacheData: Cache = emptyCacheData

    override protected def value: All[A] = cacheData

    override protected def removePatchedListener(listener: PatchedListener[A]): Unit = {
      publisher -= listener
      if (publisher.isEmpty) {
        observable.removeChangedListener(this)
      }
    }

    override protected def addPatchedListener(listener: PatchedListener[A]): Unit = {
      if (publisher.isEmpty) {
        observable.value.foreach(appendCache)
        observable.addChangedListener(this)
      }
      publisher += listener
    }

  }

  private class RxConcat[A](var observables: LazyList[Rx.Observable[A]])
      extends Rx.Observable[A]
      with ChangedListener[Option[A]] {
    private var pending = false
    def changed(upstreamEvent: ChangedEvent[Option[A]]): Unit = {
      if (pending) {
        throw new IllegalStateException(
          "Must not trigger a changed event when the listener is just added to a Binding"
        )
      }
      val newValue = upstreamEvent.newValue match {
        case None =>
          observables match {
            case head #:: tail =>
              if (head != upstreamEvent.getSource) {
                throw new IllegalStateException(
                  "This ChangedListener should have been removed from the terminated observable."
                )
              }
              head.removeChangedListener(this)
              observables = tail
              nextLivingValue()
            case _ =>
              throw new IllegalStateException(
                "This ChangedListener should not be triggered when all observables have been terminated."
              )
          }
        case someValue =>
          someValue
      }
      val event = new ChangedEvent(RxConcat.this, newValue)
      for (listener <- publisher) {
        listener.changed(event)
      }
    }

    @tailrec
    private def nextLivingValue(): Option[A] = {
      observables match {
        case head #:: tail =>
          pending = true
          head.addChangedListener(this)
          pending = false
          head.value match {
            case None =>
              head.removeChangedListener(this)
              observables = tail
              nextLivingValue()
            case someValue =>
              someValue
          }
        case _ =>
          None
      }
    }

    private val publisher = new SafeBuffer[ChangedListener[Option[A]]]
    protected def value: Option[A] = observables.headOption.flatMap(_.value)
    protected def addChangedListener(listener: ChangedListener[Option[A]]): Unit = {
      if (publisher.isEmpty) {
        nextLivingValue()
      }
      publisher += listener
    }
    protected def removeChangedListener(listener: ChangedListener[Option[A]]): Unit = {
      publisher -= listener
      observables match {
        case head #:: _ if publisher.isEmpty =>
          head.removeChangedListener(this)
        case _ =>
      }
    }

  }

  /** Reactive operators for [[Observable]]s.
    *
    * @see
    *   [[http://reactivex.io/ ReactiveX]]
    * @note
    *   [[Rx]] operators are incomplete. Feel free to create a Pull Request if you need a certain operator.
    */
  object Rx {

    /** A [[Binding]] that can be terminated.
      *
      * Once the value turned into a [[scala.None]], this [[Observable]] would be considered as terminated, and any
      * future changes of this [[Observable]] will be ignored by any [[Rx]] operators derived from this [[Observable]],
      * even if this [[Observable]] turns into a [[scala.Some]] value again.
      *
      * @note
      *   Even though an [[Observable]] is technically a [[Binding]], an [[Observable]] created from a [[Rx]] operator
      *   does not actually indicates data-binding.
      *
      * For example, given an [[Observable]] created from [[Rx.concat]],
      * {{{
      *   import com.thoughtworks.binding.Binding._
      *   val sourceObservable0 = Var[Option[String]](Some("0"))
      *   val sourceObservable1 = Var[Option[String]](Some("1"))
      *   val sourceObservables = List(sourceObservable0, sourceObservable1)
      *   val derivedObservable = Rx.concat(sourceObservables)
      *   derivedObservable.watch()
      * }}}
      *
      * when a source value gets changed,
      *
      * {{{
      *   val originalDerivedObservableValue = derivedObservable.get
      *   sourceObservable0.value = None
      * }}}
      *
      * and the source value is changed back to the original value,
      *
      * {{{
      *   sourceObservable0.value = Some("0")
      * }}}
      *
      * then the value of the derived observable might not be the original value.
      *
      * {{{
      *   derivedObservable.get shouldNot be(originalDerivedObservableValue)
      * }}}
      *
      * In contrast, if the `concat` operator is implemented by ordinary [[Binding.bind]] macros, the derived Binding is
      * indeed a data-binding, i.e. it always perform the same calculation for the same values of source [[Binding]]s.
      *
      * {{{
      *   import com.thoughtworks.binding.Binding._
      *   val sourceBinding0 = Var[Option[String]](Some("0"))
      *   val sourceBinding1 = Var[Option[String]](Some("1"))
      *   val sourceBindings = List(sourceBinding0, sourceBinding1)
      *   def concatBinding(
      *       sourceBindings: collection.LinearSeq[Rx.Observable[String]]
      *   ): Rx.Observable[String] = {
      *     sourceBindings match {
      *       case head +: tail =>
      *         Binding {
      *           head.bind match {
      *             case None =>
      *               concatBinding(tail).bind
      *             case someValue =>
      *               someValue
      *           }
      *         }
      *       case _ =>
      *         Constant(None)
      *     }
      *   }
      *   val derivedBinding = concatBinding(sourceBindings)
      *   derivedBinding.watch()
      *   val originalDerivedBindingValue = derivedBinding.get
      *   sourceBinding0.value = None
      *   sourceBinding0.value = Some("0")
      *   derivedBinding.get should be(originalDerivedBindingValue)
      * }}}
      */
    type Observable[A] = Binding[Option[A]]

    /** Emit the emissions from two or more [[Observable]]s without interleaving them.
      *
      * @see
      *   [[http://reactivex.io/documentation/operators/concat.html ReactiveX - Concat operator]]
      *
      * @example
      *   Given a sequence of [[Observable]]s,
      *   {{{
      * import com.thoughtworks.binding.Binding._
      * val observable0 = Var[Option[String]](None)
      * val observable1 = Var[Option[String]](Some("1"))
      * val observable2 = Var[Option[String]](Some("2"))
      * val observable3 = Var[Option[String]](None)
      * val observable4 = Var[Option[String]](None)
      *
      * val observable7 = Var[Option[String]](Some("7"))
      *
      * val observable8 = Binding { observable7.bind.map { v => s"8-$v-derived" } }
      * val observable5 = Binding { observable7.bind.map { v => s"5-$v-derived" } }
      *
      * val observable6 = Var[Option[String]](None)
      * val observable9 = Var[Option[String]](Some("9"))
      * val observables = Seq(
      *   observable0,
      *   observable1,
      *   observable2,
      *   observable3,
      *   observable4,
      *   observable5,
      *   observable6,
      *   observable7,
      *   observable8,
      *   observable9,
      * )
      *   }}}
      *
      * when concatenate them together,
      *
      * {{{
      * val concatenated = Rx.concat(observables).map(identity)
      * concatenated.watch()
      * }}}
      *
      * the concatenated value should be the first [[scala.Some]] value in the sequence of observables;
      *
      * {{{
      * concatenated.get should be(Some("1"))
      * }}}
      *
      * when the current observable becomes `None`,
      * {{{
      * observable1.value = None
      * }}}
      *
      * the concatenated value should be the next [[scala.Some]] value in the sequence of observables,
      *
      * {{{
      * concatenated.get should be(Some("2"))
      * }}}
      *
      * even when the next [[scala.Some]] value is derived from another [[Binding]];
      *
      * {{{
      * observable2.value = None
      * concatenated.get should be(Some("5-7-derived"))
      * }}}
      *
      * when the value of the upstream [[Binding]] is changed to another [[scala.Some]] value,
      *
      * {{{
      * observable7.value = Some("7-running")
      * }}}
      *
      * the concatenated value should be changed accordingly;
      * {{{
      * concatenated.get should be(Some("5-7-running-derived"))
      * }}}
      *
      * when multiple observables become [[scala.None]] at once,
      *
      * {{{
      * observable7.value = None
      * }}}
      *
      * they all should be skipped when calculate the concatenated value;
      * {{{
      * concatenated.get should be(Some("9"))
      * }}}
      *
      * when the last observable in the sequence becomes [[scala.None]],
      * {{{
      * observable9.value = None
      * }}}
      *
      * the concatenated value should become [[scala.None]] permanently,
      *
      * {{{
      * concatenated.get should be(None)
      * }}}
      *
      * even when some observables in the sequence become [[scala.Some]] again.
      *
      * {{{
      * observable9.value = Some("9-after-termination")
      * concatenated.get should be(None)
      * observable7.value = Some("7-after-termination")
      * concatenated.get should be(None)
      * }}}
      */
    def concat[A](observables: IterableOnce[Observable[A]]): Observable[A] = {
      new RxConcat(LazyList.from(observables))
    }

    /** @see
      *   [[http://reactivex.io/documentation/operators/repeat.html ReactiveX - Repeat operator]]
      */
    def repeat[A](source: => Observable[A]): Observable[A] = {
      new RxConcat(LazyList.continually(source))
    }

    /** @see
      *   [[http://reactivex.io/documentation/operators/merge.html ReactiveX - Merge operator]]
      */
    def merge[A](bindingSeq: BindingSeq[A]): Observable[A] = {
      new RxMerge(bindingSeq)
    }

    /** do not create the Observable until the observer subscribes
      *
      * @see
      *   [[http://reactivex.io/documentation/operators/defer.html ReactiveX - Defer operator]]
      *
      * @note
      *   This [[defer]] is slightly different from other implementation the
      *   [[http://reactivex.io/documentation/operators/defer.html ReactiveX Defer]] operator, because this [[defer]]
      *   shares the same upstream [[Observable]] instance for all subscribes.
      *
      * @example
      *   Circular referenced [[Observable]]s can be created with the help of [[defer]]
      *   {{{
      *   import Binding._
      *   val source = Var("init")
      *   lazy val observable1: Rx.Observable[String] =
      *     Binding[Option[String]] {
      *       source.bind match {
      *         case "init" =>
      *           None
      *         case v =>
      *           observable2.bind.map(_ + "_" + v)
      *       }
      *     }
      *
      *   lazy val observable2: Rx.Observable[String] = Rx.defer(
      *     Binding[Option[String]] {
      *       Some(observable1.getClass.getSimpleName)
      *     }
      *   )
      *   }}}
      *
      * Initially, `observable1` did not subscribe `observable2` because `source` is `init`,
      * {{{
      *   observable1.watch()
      * }}}
      * therefore observable2 should be `None`,
      * {{{
      *   observable1.get should be (None)
      *   observable2.get should be (None)
      * }}}
      * when `source` changed,
      * {{{
      *   source.value = "changed"
      * }}}
      * `observable1` should subscribe `observable2`, and there should be `Some` values.
      * {{{
      *   observable1.get should be (Some("FlatMap_changed"))
      *   observable2.get should be (Some("FlatMap"))
      * }}}
      * Even though circular referenced [[Observable]]s can be created in this way, their calculation must not be
      * mutually dependent.
      */
    def defer[A](upstream: => Observable[A]): Observable[A] = {
      new RxDefer(upstream)
    }

    /** Combine multiple [[Observable]]s into one by merging their emissions.
      *
      * @see
      *   [[http://reactivex.io/documentation/operators/merge.html ReactiveX - Merge operator]]
      *
      * @example
      *   Given a sequence of [[Observable]]s,
      *   {{{
      * import com.thoughtworks.binding.Binding._
      * val observable0 = Var[Option[String]](None)
      * val observable1 = Var[Option[String]](Some("1"))
      * val observable2 = Var[Option[String]](Some("2"))
      * val observable3 = Var[Option[String]](None)
      * val observable4 = Var[Option[String]](None)
      *
      * val observable7 = Var[Option[String]](Some("7"))
      *
      * val observable8 = Binding { observable7.bind.map { v => s"8-$v-derived" } }
      * val observable5 = Binding { observable7.bind.map { v => s"5-$v-derived" } }
      *
      * val observable6 = Var[Option[String]](None)
      * val observable9 = Var[Option[String]](Some("9"))
      * val observables = Seq(
      *   observable0,
      *   observable1,
      *   observable2,
      *   observable3,
      *   observable4,
      *   observable5,
      *   observable6,
      *   observable7,
      *   observable8,
      *   observable9,
      * )
      *   }}}
      *
      * when merge them together,
      *
      * {{{
      * val merged = Rx.merge(observables).map(identity)
      * merged.watch()
      * }}}
      *
      * the merged value should be the first [[scala.Some]] value in the sequence of observables;
      *
      * {{{
      * merged.get should be(Some("1"))
      * }}}
      *
      * when the some but not all of the observable becomes `None`,
      *
      * {{{
      * observable1.value = None
      * }}}
      *
      * the merged value should be unchanged,
      *
      * {{{
      * merged.get should be(Some("1"))
      * }}}
      *
      * when any of the observable becomes `Some` value,
      *
      * {{{
      * observable2.value = Some("2-changed")
      * }}}
      *
      * the merged value should be the new value,
      *
      * {{{
      * merged.get should be(Some("2-changed"))
      * }}}
      *
      * even when a previous `None` observable becomes `Some` value,,
      *
      * {{{
      * observable3.value = Some("3-previous-None")
      * }}}
      *
      * the merged value should be the new value of the previous `None` observable,
      *
      * {{{
      * merged.get should be(Some("3-previous-None"))
      * }}}
      *
      * when multiple observables are changed at once,
      *
      * {{{
      * observable7.value = Some("7-changed")
      * }}}
      *
      * the merged value should be the value of the last derived observable
      *
      * {{{
      * merged.get should be(Some("8-7-changed-derived"))
      * }}}
      *
      * when all the observables become `None`,
      *
      * {{{
      * observable1.value = None
      * merged.get should be(Some("8-7-changed-derived"))
      * observable2.value = None
      * merged.get should be(Some("8-7-changed-derived"))
      * observable3.value = None
      * merged.get should be(Some("8-7-changed-derived"))
      * observable6.value = None
      * merged.get should be(Some("8-7-changed-derived"))
      * observable7.value = None
      * merged.get should be(Some("8-7-changed-derived"))
      * observable9.value = None
      * merged.get should be(None)
      * }}}
      */
    def merge[A](observables: IterableOnce[Observable[A]]): Observable[A] = {
      new RxMerge(
        new Constants(toConstantsData(observables)).flatMapBinding(
          _.map { option =>
            new Constants(toConstantsData(option))
          }
        )
      )
    }

    /** convert an Observable into a [[BindingSeq]].
      *
      * @see
      *   [[http://reactivex.io/documentation/operators/to.html ReactiveX - To operator]]
      *
      * @example
      *   Given a source observable,
      *   {{{
      *   import com.thoughtworks.binding.Binding._
      *   val observable = Var[Option[String]](Some("1"))
      *   }}}
      *
      * when converting it into a [[BindingSeq]],
      *
      * {{{
      *   val bindingSeq = Rx.toBindingSeq(observable)
      * }}}
      *
      * and flat-mapping to the result,
      * {{{
      *   val result = new BindingSeq.FlatMap(
      *     bindingSeq,
      *     { value: String => Constants("the value is", value) }
      *   ).all
      *   result.watch()
      * }}}
      *
      * then result should have the values corresponding to the source observable,
      * {{{
      *   result.get.toSeq should contain theSameElementsInOrderAs Seq("the value is", "1")
      * }}}
      *
      * when the source observable changes,
      * {{{
      *   observable.value = Some("2")
      * }}}
      *
      * then the corresponding new value should be appended to the result,
      * {{{
      *   result.get.toSeq should contain theSameElementsInOrderAs Seq(
      *     "the value is", "1",
      *     "the value is", "2"
      *   )
      * }}}
      *
      * when the source observable terminates,
      * {{{
      *   observable.value = None
      * }}}
      *
      * then the result should be empty
      * {{{
      *   result.get.toSeq should be(empty)
      * }}}
      */
    def toBindingSeq[A](observable: Observable[A]): BindingSeq[A] = {
      new RxToBindingSeq(observable)
    }

  }

}

/** A data binding expression that represents a value that automatically recalculates when its dependencies change.
  *
  * @example
  *   You may create a data binding expression via `Binding { ??? }` block annotation.
  *
  * {{{
  *           val bindingInt: Binding[Int] = Binding { 100 }
  * }}}
  *
  * A data binding expression may depend on other binding expressions via [[bind]] method:
  *
  * {{{
  *           val bindingString: Binding[String] = Binding { bindingInt.bind.toString }
  * }}}
  *
  * @author
  *   杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
trait Binding[+A] extends Binding.Watchable[A] with Binding2Or3[A] {

  private[binding] def get: A = value

  /** Returns the current value of this [[Binding]]
    *
    * @note
    *   This method must not be invoked inside a `@dom` method body or a `Binding { ... }` block..
    */
  protected def value: A

  protected def removeChangedListener(listener: Binding.ChangedListener[A]): Unit

  protected def addChangedListener(listener: Binding.ChangedListener[A]): Unit

  /** Enable automatic recalculation.
    *
    * You may invoke this method more than once. Then, when you want to disable automatic recalculation, you must invoke
    * [[#unwatch unwatch]] same times as the number of calls to this method.
    *
    * @note
    *   This method is recursive, which means that the dependencies of this [[Binding]] will be watched as well.
    */
  @inline
  final def watch(): Unit = {
    addChangedListener(Binding.DummyChangedListener)
  }

  /** Disable automatic recalculation.
    *
    * @note
    *   This method is recursive, which means that the dependencies of this [[Binding]] will be unwatched as well.
    */
  @inline
  final def unwatch(): Unit = {
    removeChangedListener(Binding.DummyChangedListener)
  }

  final def map[B](f: A => B): Binding[B] = {
    this match {
      case Binding.Constant(a) =>
        Binding.Constant(f(a))
      case fa =>
        new Binding.Map[A, B](fa, f)
    }
  }

  final def flatMap[B](f: A => Binding[B]): Binding[B] = {
    this match {
      case Binding.Constant(a) =>
        f(a)
      case fa =>
        new Binding.FlatMap[A, B](fa, f)
    }
  }
}
