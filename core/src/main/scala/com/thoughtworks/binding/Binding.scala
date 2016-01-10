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

import com.thoughtworks.each.Monadic._

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.collection.GenSeq
import scala.collection.Seq
import scala.collection.mutable.Buffer
import scala.util.Try
import scalaz.Monad
import scala.language.experimental.macros

/**
  * @groupname typeClasses Type class instance
  * @groupname implicits Implicits Conversions
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

  private[binding] final class ChangedEvent[+Value](source: AnyRef,
                                                    val oldValue: Value,
                                                    val newValue: Value) extends EventObject(source) {
    override def toString = raw"""ChangedEvent[source=$source oldValue=$oldValue newValue=$newValue]"""

  }

  private[binding] final class PatchedEvent[+Element](source: AnyRef,
                                                      val oldSeq: Seq[Element],
                                                      val from: Int,
                                                      val that: GenSeq[Element],
                                                      val replaced: Int) extends EventObject(source) {
    override def toString = raw"""PatchedEvent[source=$source oldSeq=$oldSeq from=$from that=$that replaced=$replaced]"""
  }

  private[binding] trait ChangedListener[-Value] {
    private[binding] def changed(event: ChangedEvent[Value]): Unit
  }

  private[binding] trait PatchedListener[-Element] {
    private[binding] def patched(event: PatchedEvent[Element]): Unit
  }

  /**
    * An data binding expression that never changes.
    * @group expressions
    */
  final case class Constant[+A](override val get: A) extends AnyVal with Binding[A] {

    @inline
    override private[binding] def removeChangedListener(listener: ChangedListener[A]): Unit = {
      // Do nothing because this Constant never changes
    }

    @inline
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
    * Source variable of data binding expression.
    *
    * You can manually change the value:
    *
    * {{{
    * val bindingVar = Var("initial value")
    * bindingVar := "changed value"
    * }}}
    *
    * Then, any data binding expressions that depend on this [[Var]] will be changed automatically.
    *
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

  /**
    * @group expressions
    */
  object FutureBinding {
    def apply[A](future: Future[A])(implicit executor: ExecutionContext) = new FutureBinding(future)
  }

  /**
    * A wrapper that wraps [[scala.concurrent.Future]] to a [[Binding]].
    * @group expressions
    * @note Because all [[Binding]] (including this [[FutureBinding]]) are not thread safe.
    *       As a result, `executor` must guarantee running sequencely.
    */
  final class FutureBinding[A](future: Future[A])(implicit executor: ExecutionContext) extends Binding[Option[Try[A]]] {

    override def get = future.value

    private val publisher = new Publisher[ChangedListener[Option[Try[A]]]]

    override private[binding] def removeChangedListener(listener: ChangedListener[Option[Try[A]]]): Unit = {
      publisher.unsubscribe(listener)
    }

    private var isHandlerRegiested: Boolean = false

    private def completeHandler(result: Try[A]): Unit = {
      for ((listener, _) <- publisher) {
        listener.changed(new ChangedEvent[Option[Try[A]]](this, None, Some(result)))
      }
    }

    override private[binding] def addChangedListener(listener: ChangedListener[Option[Try[A]]]): Unit = {
      if (!isHandlerRegiested) {
        isHandlerRegiested = true
        if (!future.isCompleted) {
          future.onComplete(completeHandler)
        }
      }
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
    * Monad instances for [[Binding]].
    *
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
          _root_.com.thoughtworks.binding.Binding
        ].apply[$b]($body)"""
      val monadicFunction = atPos(f.pos)(Function(vparams, monadicBody))
      atPos(apply.pos)( q"""$self.mapBinding[$b]($monadicFunction)""")
    }

    def flatMap(c: scala.reflect.macros.blackbox.Context)(f: c.Tree): c.Tree = {
      import c.universe._
      val apply@Apply(TypeApply(Select(self, TermName("flatMap")), List(b)), List(f@Function(vparams, body))) = c.macroApplication
      val monadicBody =
        q"""_root_.com.thoughtworks.each.Monadic.monadic[
          _root_.com.thoughtworks.binding.Binding
        ].apply[_root_.com.thoughtworks.binding.Binding.BindingSeq[$b]]($body)"""
      val monadicFunction = atPos(f.pos)(Function(vparams, monadicBody))
      atPos(apply.pos)( q"""$self.flatMapBinding[$b]($monadicFunction)""")
    }

    def withFilter(c: scala.reflect.macros.blackbox.Context)(condition: c.Tree): c.Tree = {
      import c.universe._
      val apply@Apply(Select(self, TermName("withFilter")), List(f@Function(vparams, body))) = c.macroApplication
      val monadicBody =
        q"""_root_.com.thoughtworks.each.Monadic.monadic[
          _root_.com.thoughtworks.binding.Binding
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

  private[binding] final class FlatMapBinding[A, B](upstream: BindingSeq[A], f: A => BindingSeq[B]) extends BindingSeq[B] {

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
            listener.patched(new PatchedEvent(FlatMapBinding.this, get, flattenFrom, flatNewChildren, flattenReplaced))
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
          listener.changed(new ChangedEvent(FlatMapBinding.this, get, new FlatProxy(newCache)))
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
          listener.patched(new PatchedEvent(FlatMapBinding.this, get, index, event.newValue, event.oldValue.length))
        }
      }

      override private[binding] def patched(event: PatchedEvent[B]): Unit = {
        val source = event.getSource.asInstanceOf[BindingSeq[B]]
        val index = flatIndex(0, cache.indexOf(source)) + event.from
        for ((listener, _) <- patchedPublisher) {
          listener.patched(new PatchedEvent(FlatMapBinding.this, get, index, event.that, event.replaced))
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
    * Data binding expression of a sequence
    *
    * @group expressions
    */
  sealed trait BindingSeq[+A] extends Any with Binding[Seq[A]] {

    private[binding] def removePatchedListener(listener: PatchedListener[A]): Unit

    private[binding] def addPatchedListener(listener: PatchedListener[A]): Unit

    /**
      * Returns a [[BindingSeq]] that maps each element of this [[BindingSeq]] via `f`
      * @note This method is only available in a `monadic[Binding]` block or a `@dom` method.
      */
    def map[B](f: A => B): BindingSeq[B] = macro Macros.map

    /**
      * Returns a [[BindingSeq]] that flat-maps each element of this [[BindingSeq]] via `f`
      * @note This method is only available in a `monadic[Binding]` block or a `@dom` method.
      */
    def flatMap[B](f: A => BindingSeq[B]): BindingSeq[B] = macro Macros.flatMap

    /**
      * Underlying implementation of [[#map]].
      *
      * @note Don't use this method in user code.
      */
    @inline
    final def mapBinding[B](f: A => Binding[B]): BindingSeq[B] = new MapBinding[A, B](this, f)

    /**
      * Underlying implementation of [[#flatMap]].
      *
      * @note Don't use this method in user code.
      */
    @inline
    final def flatMapBinding[B](f: A => Binding[BindingSeq[B]]): BindingSeq[B] = {
      new FlatMapBinding[A, B](this, { a =>
        new FlatMapBinding[BindingSeq[B], B](new MapBinding[Unit, BindingSeq[B]](Constants(()), _ => f(a)), locally)
      })
    }

    /**
      * Returns a view of this [[BindingSeq]] that applied a filter of `condition`
      */
    def withFilter(condition: A => Boolean): WithFilter = macro Macros.withFilter

    /**
      * Underlying implementation of [[#withFilter]].
      *
      * @note Don't use this method in user code.
      */
    @inline
    final def withFilterBinding(condition: A => Binding[Boolean]): WithFilter = {
      new WithFilter(condition)
    }

    /**
      * A helper to build complicated comprehension expressions for [[BindingSeq]]
      */
    final class WithFilter(condition: A => Binding[Boolean]) {

      /**
        * Returns a [[BindingSeq]] that maps each element of this [[BindingSeq]] via `f`
        * @note This method is only available in a `monadic[Binding]` block or a `@dom` method.
        */
      def map[B](f: A => B): BindingSeq[B] = macro Macros.map

      /**
        * Returns a [[BindingSeq]] that flat-maps each element of this [[BindingSeq]] via `f`
        * @note This method is only available in a `monadic[Binding]` block or a `@dom` method.
        */
      def flatMap[B](f: A => BindingSeq[B]): BindingSeq[B] = macro Macros.flatMap

      /**
        * Returns a view of this [[BindingSeq]] that applied a filter of `condition`
        */
      def withFilter(condition: A => Boolean): WithFilter = macro Macros.withFilter

      /**
        * Underlying implementation of [[#withFilter]].
        *
        * @note Don't use this method in user code.
        */
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

      /**
        * Underlying implementation of [[#map]].
        *
        * @note Don't use this method in user code.
        */
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

      /**
        * Underlying implementation of [[#flatMap]].
        *
        * @note Don't use this method in user code.
        */
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
    * An data binding expression of sequence that never changes.
    * @group expressions
    */
  final case class Constants[+A](override val get: A*) extends AnyVal with BindingSeq[A] {

    @inline
    override private[binding] def removePatchedListener(listener: PatchedListener[A]): Unit = {}

    @inline
    override private[binding] def addPatchedListener(listener: PatchedListener[A]): Unit = {}

    @inline
    override private[binding] def removeChangedListener(listener: ChangedListener[Seq[A]]): Unit = {}

    @inline
    override private[binding] def addChangedListener(listener: ChangedListener[Seq[A]]): Unit = {}

  }

  /**
    * @group expressions
    */
  object Vars {

    @inline
    def apply[A](initialValues: A*) = new Vars(Vector(initialValues: _*))

    @inline
    def empty[A] = new Vars(Vector.empty[A])

  }

  /**
    * Source sequence of data binding expression.
    * @group expressions
    */
  final class Vars[A] private(private var cache: Vector[A]) extends BindingSeq[A] {

    private[binding] val patchedPublisher = new Publisher[PatchedListener[A]]

    private[binding] val changedPublisher = new Publisher[ChangedListener[Seq[A]]]

    /**
      * Returns a [[scala.collection.mutable.Buffer]] that allow you change the content of this [[Vars]].
      *
      * Whenever you change the returned buffer,
      * other binding expressions that depend on this [[Vars]] will be automatically changed.
      */
    @inline
    override def get: Buffer[A] = new Proxy

    /**
      * Reset content of this [[Vars]]
      * @param newValues
      */
    def reset(newValues: A*): Unit = {
      val newCache = Vector(newValues: _*)
      for ((listener, _) <- changedPublisher) {
        listener.changed(new ChangedEvent[Seq[A]](Vars.this, cache, newCache))
      }
      cache = newCache
    }

    private[binding] final class Proxy extends Buffer[A] {
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
    * A mount point that places the result of a data binding expression into DOM or other system.
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
    * A mount point that places the result of a data binding expression of a sequence into DOM or other system.
    * @group expressions
    */
  abstract class MultiMountPoint[-Element](upstream: BindingSeq[Element]) extends MountPoint {

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
    * A mount point that places the result of a data binding expression of a single value into DOM or other system.
    * @group expressions
    */
  abstract class SingleMountPoint[-Value](upstream: Binding[Value]) extends MountPoint {

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
    * A implicit view to enable `.each` magic.
    * @group implicits
    */
  @inline
  implicit def eachOps[A](binding: Binding[A]): EachOps[Binding, A] = new EachOps[Binding, A](binding)

  private[binding] object DummyListener extends ChangedListener[Any] {
    @inline
    override private[binding] def changed(event: ChangedEvent[Any]): Unit = {}
  }

}

/**
  * A data binding expression that represent a value that automatically re-calculates when its dependencies change.
  *
  * You may compose a data binding expression via `monadic[Binding]` block,
  * or add `@dom` annotation to methods that produces a data binding expression.
  *
  * @see [[com.thoughtworks.each.Monadic]]
  *
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
trait Binding[+A] extends Any {

  private[binding] def get: A

  private[binding] def removeChangedListener(listener: Binding.ChangedListener[A]): Unit

  private[binding] def addChangedListener(listener: Binding.ChangedListener[A]): Unit

  /**
    * Enable automatically re-calculation.
    *
    * You may invoke this method more than once.
    * Then, when you want to disable automatically re-calculation,
    * you must invoke [[#unwatch]] same times as the number of calls to this method.
    *
    * @note This method is recursive, which means that the dependencies of this [[Binding]] will be watched as well.
    */
  @inline
  final def watch(): Unit = {
    addChangedListener(Binding.DummyListener)
  }

  /**
    * Disable automatically re-calculation.
    *
    * @note This method is recursive, which means that the dependencies of this [[Binding]] will be unwatched as well.
    */
  @inline
  final def unwatch(): Unit = {
    removeChangedListener(Binding.DummyListener)
  }

}

