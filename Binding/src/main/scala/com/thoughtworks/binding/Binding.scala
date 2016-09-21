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

import com.thoughtworks.sde.core.MonadicFactory._
import com.thoughtworks.enableIf
import com.thoughtworks.sde.core.MonadicFactory
import macrocompat.bundle

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.language.higherKinds
import scala.collection.GenSeq
import scala.collection.mutable.Buffer
import scala.util.Try
import scalaz.{Monad, MonadPlus}
import scala.language.experimental.macros

/**
  * @groupname typeClasses Type class instance
  * @groupname implicits Implicits Conversions
  * @groupname expressions Binding Expressions
  * @groupdesc expressions AST nodes of binding expressions
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object Binding extends MonadicFactory.WithTypeClass[Monad, Binding] {

  override val typeClass = BindingInstances

  private object Publisher {

    private[Publisher] sealed trait State

    case object Idle extends State

    case object CleanForeach extends State

    case object DirtyForeach extends State

  }

  private object Jvm {

    @enableIf(c => !c.compilerSettings.exists(_.matches("""^-Xplugin:.*scalajs-compiler_[0-9\.\-]*\.jar$""")))
    def newBuffer[A] = collection.mutable.ArrayBuffer.empty[A]

  }

  private object Js {

    @inline
    @enableIf(c => c.compilerSettings.exists(_.matches("""^-Xplugin:.*scalajs-compiler_[0-9\.\-]*\.jar$""")))
    def newBuffer[A] = new scalajs.js.Array[A]

    @enableIf(c => c.compilerSettings.exists(_.matches("""^-Xplugin:.*scalajs-compiler_[0-9\.\-]*\.jar$""")))
    @inline
    implicit final class ReduceToSizeOps[A] @inline()(array: scalajs.js.Array[A]) {
      @inline def reduceToSize(newSize: Int) = array.length = newSize
    }

  }

  import Js._
  import Jvm._

  private[binding] final class Publisher[Subscriber >: Null] {

    import Publisher._

    private val subscribers = newBuffer[Subscriber]

    @volatile
    private var state: State = Idle

    def nonEmpty = !isEmpty

    def isEmpty = subscribers.forall(_ == null)

    def foreach[U](f: Subscriber => U): Unit = {
      @tailrec
      def loop(i: Int): Unit = {
        if (i < subscribers.length) {
          val subscriber = subscribers(i)
          if (subscriber != null) {
            f(subscriber)
          }
          loop(i + 1)
        }
      }
      state match {
        case Idle =>
          state = CleanForeach
          loop(0)
          state match {
            case DirtyForeach => {
              @tailrec
              def compact(i: Int, j: Int): Unit = {
                if (i < subscribers.length) {
                  val subscriber = subscribers(i)
                  if (subscriber == null) {
                    compact(i + 1, j)
                  } else {
                    subscribers(j) = subscriber
                    compact(i + 1, j + 1)
                  }
                } else {
                  subscribers.reduceToSize(j)
                }
              }
              compact(0, 0)
              state = Idle
            }
            case CleanForeach =>
              state = Idle
            case Idle =>
              throw new IllegalStateException("Expect CleanForeach or DirtyForeach")
          }
        case CleanForeach | DirtyForeach =>
          loop(0)
      }
    }

    final def subscribe(subscriber: Subscriber): Unit = {
      subscribers += subscriber
    }

    final def unsubscribe(subscriber: Subscriber): Unit = {
      state match {
        case Idle =>
          subscribers -= subscriber
        case CleanForeach =>
          subscribers(subscribers.indexOf(subscriber)) = null
          state = DirtyForeach
        case DirtyForeach =>
          subscribers(subscribers.indexOf(subscriber)) = null
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
                                                      val that: Seq[Element],
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
    *
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

   /**
     * Changes the current value of this [[Var]], and reevaluates any expressions that depends on this [[Var]].
     *
     * @note This method must not be invoked inside a `@dom` method body.
     */
    final def :=(newValue: A): Unit = {
      if (value != newValue) {
        for (listener <- publisher) {
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
          for (listener <- publisher) {
            listener.changed(new ChangedEvent(FlatMap.this, cache.get, newCache.get))
          }
        }
        cache = newCache
      }
    }

    override private[binding] final def changed(event: ChangedEvent[B]) = {
      for (listener <- publisher) {
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

    override def ifM[B](value: Binding[Boolean], ifTrue: => Binding[B], ifFalse: => Binding[B]): Binding[B] = {
      bind(value)(if (_) ifTrue else ifFalse)
    }

    override def whileM[G[_], A](p: Binding[Boolean], body: => Binding[A])(implicit G: MonadPlus[G]): Binding[G[A]] = {
      ifM(p, bind(body)(x => map(whileM(p, body))(xs => G.plus(G.point(x), xs))), point(G.empty))
    }

    override def whileM_[A](p: Binding[Boolean], body: => Binding[A]): Binding[Unit] = {
      ifM(p, bind(body)(_ => whileM_(p, body)), point(()))
    }

    override def untilM[G[_], A](f: Binding[A], cond: => Binding[Boolean])(implicit G: MonadPlus[G]): Binding[G[A]] = {
      bind(f)(x => map(whileM(map(cond)(!_), f))(xs => G.plus(G.point(x), xs)))
    }

    override def untilM_[A](f: Binding[A], cond: => Binding[Boolean]): Binding[Unit] = {
      bind(f)(_ => whileM_(map(cond)(!_), f))
    }

  }

  @bundle
  private[Binding] class Macros(val c: scala.reflect.macros.blackbox.Context) {
    import c.universe._

    final def map(f: Tree): Tree = {
      val apply@Apply(TypeApply(Select(self, TermName("map")), List(b)), List(f@Function(vparams, body))) = c.macroApplication
      val monadicBody =
        q"""_root_.com.thoughtworks.binding.Binding.apply[$b]($body)"""
      val monadicFunction = atPos(f.pos)(Function(vparams, monadicBody))
      atPos(apply.pos)( q"""$self.mapBinding[$b]($monadicFunction)""")
    }

    final def flatMap(f: Tree): Tree = {
      val apply@Apply(TypeApply(Select(self, TermName("flatMap")), List(b)), List(f@Function(vparams, body))) = c.macroApplication
      val monadicBody =
        q"""_root_.com.thoughtworks.binding.Binding.apply[_root_.com.thoughtworks.binding.Binding.BindingSeq[$b]]($body)"""
      val monadicFunction = atPos(f.pos)(Function(vparams, monadicBody))
      atPos(apply.pos)( q"""$self.flatMapBinding[$b]($monadicFunction)""")
    }

    final def withFilter(condition: Tree): Tree = {
      val apply@Apply(Select(self, TermName("withFilter")), List(f@Function(vparams, body))) = c.macroApplication
      val monadicBody =
        q"""_root_.com.thoughtworks.binding.Binding.apply[_root_.scala.Boolean]($body)"""
      val monadicFunction = atPos(f.pos)(Function(vparams, monadicBody))
      atPos(apply.pos)( q"""$self.withFilterBinding($monadicFunction)""")
    }

    final def bind: Tree = {
      val q"$binding.$methodName" = c.macroApplication
      q"""_root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[
        _root_.com.thoughtworks.binding.Binding,
        ${TypeTree(c.macroApplication.tpe)}
      ]($binding)"""
    }

  }


  private[binding] final case class Length(bindingSeq: BindingSeq[_]) extends Binding[Int] with PatchedListener[Any] {

    private val publisher = new Publisher[ChangedListener[Int]]

    override private[binding] def get: Int = bindingSeq.get.length

    override private[binding] def removeChangedListener(listener: ChangedListener[Int]): Unit = {
      publisher.unsubscribe(listener)
      if (publisher.isEmpty) {
        bindingSeq.removePatchedListener(this)
      }
    }

    override private[binding] def addChangedListener(listener: ChangedListener[Int]): Unit = {
      if (publisher.isEmpty) {
        bindingSeq.addPatchedListener(this)
      }
      publisher.subscribe(listener)
    }

    override private[binding] final def patched(patchedEvent: PatchedEvent[Any]): Unit = {
      val oldLength = patchedEvent.oldSeq.length
      val changedEvent = new ChangedEvent[Int](this, oldLength, oldLength + patchedEvent.that.length - patchedEvent.replaced)
      for (subscriber <- publisher) {
        subscriber.changed(changedEvent)
      }
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
        a <- upstream.get.view
      } yield f(a)).toVector
    }

    override private[binding] def get: Seq[B] = new ValueProxy(cache)

    private val upstreamListener = new PatchedListener[A] {
      override def patched(event: PatchedEvent[A]): Unit = {
        val mappedNewChildren = (for {
          child <- event.that.view
        } yield f(child)).toVector
        for (listener <- publisher) {
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

    }

    private[binding] val publisher = new Publisher[PatchedListener[B]]

    private val childListener = new ChangedListener[B] {

      override def changed(event: ChangedEvent[B]): Unit = {
        val index = cache.indexOf(event.getSource)
        for (listener <- publisher) {
          listener.patched(new PatchedEvent(MapBinding.this, new ValueProxy(cache), index, SingleSeq(event.newValue), 1))
        }
      }
    }

    override private[binding] def removePatchedListener(listener: PatchedListener[B]): Unit = {
      publisher.unsubscribe(listener)
      if (publisher.isEmpty) {
        // FIXME
        upstream.removePatchedListener(upstreamListener)
        for (child <- cache) {
          child.removeChangedListener(childListener)
        }
      }
    }

    override private[binding] def addPatchedListener(listener: PatchedListener[B]): Unit = {
      if (publisher.isEmpty) {
        upstream.addPatchedListener(upstreamListener)
        for (child <- cache) {
          child.addChangedListener(childListener)
        }
      }
      publisher.subscribe(listener)
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
        a <- upstream.get.view
      } yield f(a)).toVector
    }

    @inline
    override private[binding] def get = new FlatProxy(cache)

    @inline
    private def flatIndex(upstreamBegin: Int, upstreamEnd: Int): Int = {
      cache.view(upstreamBegin, upstreamEnd).map(_.get.length).sum
    }

    private val upstreamListener = new PatchedListener[A] {
      override private[binding] def patched(event: PatchedEvent[A]): Unit = {
        val mappedNewChildren = (for {
          child <- event.that.view
        } yield f(child)).toVector
        val flatNewChildren = new FlatProxy(mappedNewChildren)
        if (event.replaced != 0 || flatNewChildren.nonEmpty) {
          val flattenFrom = flatIndex(0, event.from)
          val flattenReplaced = flatIndex(event.from, event.from + event.replaced)
          val flattenPatchedEvent = new PatchedEvent(FlatMapBinding.this, get, flattenFrom, flatNewChildren, flattenReplaced)
          for (listener <- publisher) {
            listener.patched(flattenPatchedEvent)
          }
          for (oldChild <- cache.view(event.from, event.replaced)) {
            oldChild.removePatchedListener(childListener)
          }
          for (newChild <- mappedNewChildren) {
            newChild.addPatchedListener(childListener)
          }
        }
        cache = cache.patch(event.from, mappedNewChildren, event.replaced)
      }

    }

    private[binding] val publisher = new Publisher[PatchedListener[B]]

    private val childListener = new PatchedListener[B] {
      override private[binding] def patched(event: PatchedEvent[B]): Unit = {
        val source = event.getSource.asInstanceOf[BindingSeq[B]]
        val index = flatIndex(0, cache.indexOf(source)) + event.from
        for (listener <- publisher) {
          listener.patched(new PatchedEvent(FlatMapBinding.this, get, index, event.that, event.replaced))
        }
      }
    }

    override private[binding] def removePatchedListener(listener: PatchedListener[B]): Unit = {
      publisher.unsubscribe(listener)
      if (publisher.isEmpty) {
        upstream.removePatchedListener(upstreamListener)
        for (child <- cache) {
          child.addPatchedListener(childListener)
        }
      }
    }

    override private[binding] def addPatchedListener(listener: PatchedListener[B]): Unit = {
      if (publisher.isEmpty) {
        upstream.addPatchedListener(upstreamListener)
        for (child <- cache) {
          child.addPatchedListener(childListener)
        }
      }
      publisher.subscribe(listener)
    }
  }

  /**
    * The companion of a data binding expression of a sequence
    *
    * @group expressions
    */
  object BindingSeq {

    implicit final class AsBinding[Element](upstream: BindingSeq[Element]) extends Binding[Seq[Element]] with PatchedListener[Element] {

      private val publisher = new Publisher[ChangedListener[Seq[Element]]]

      @inline
      override private[binding] def get: Seq[Element] = upstream.get

      override private[binding] def removeChangedListener(listener: ChangedListener[Seq[Element]]): Unit = {
        publisher.unsubscribe(listener)
        if (publisher.isEmpty) {
          upstream.removePatchedListener(this)
        }
      }

      override private[binding] def addChangedListener(listener: ChangedListener[Seq[Element]]): Unit = {
        if (publisher.isEmpty) {
          upstream.addPatchedListener(this)
        }
        publisher.subscribe(listener)
      }

      private[binding] def patched(event: PatchedEvent[Element]): Unit = {
        import event._
        val newSeq = oldSeq.view(0, from) ++ that ++ oldSeq.view(from + replaced, oldSeq.length)
        for (listener <- publisher) {
          listener.changed(new ChangedEvent[Seq[Element]](AsBinding.this, oldSeq, newSeq))
        }
      }

    }
  }

  /**
    * Data binding expression of a sequence
    *
    * @group expressions
    */
  sealed trait BindingSeq[+A] extends Any {

    /**
      * Enables automatically re-calculation.
      *
      * You may invoke this method more than once.
      * Then, when you want to disable automatically re-calculation,
      * you must invoke [[#unwatch]] same times as the number of calls to this method.
      *
      * @note This method is recursive, which means that the dependencies of this [[BindingSeq]] will be watched as well.
      */
    @inline
    final def watch(): Unit = {
      addPatchedListener(Binding.DummyPatchedListener)
    }

    /**
      * Disables automatically re-calculation.
      *
      * @note This method is recursive, which means that the dependencies of this [[BindingSeq]] will be unwatched as well.
      */
    @inline
    final def unwatch(): Unit = {
      removePatchedListener(Binding.DummyPatchedListener)
    }

    private[binding] def get: Seq[A]

    private[binding] def removePatchedListener(listener: PatchedListener[A]): Unit

    private[binding] def addPatchedListener(listener: PatchedListener[A]): Unit

    final def length: Binding[Int] = Length(this)

    /**
      * Returns a [[BindingSeq]] that maps each element of this [[BindingSeq]] via `f`
      *
      * @note This method is only available in a `Binding { ??? }` block or a `@dom` method.
      */
    def map[B](f: A => B): BindingSeq[B] = macro Macros.map

    /**
      * Returns a [[BindingSeq]] that flat-maps each element of this [[BindingSeq]] via `f`
      *
      * @note This method is only available in a `Binding { ??? }` block or a `@dom` method.
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
      new FlatMapBinding[BindingSeq[B], B](new MapBinding[A, BindingSeq[B]](this, f), locally)
    }

    /**
      * Returns a view of this [[BindingSeq]] that applied a filter of `condition`
      */
    def withFilter(condition: A => Boolean): BindingSeq[A]#WithFilter = macro Macros.withFilter

    /**
      * Underlying implementation of [[#withFilter]].
      *
      * @note Don't use this method in user code.
      */
    @inline
    final def withFilterBinding(condition: A => Binding[Boolean]): BindingSeq[A]#WithFilter = {
      new WithFilter(condition)
    }

    /**
      * A helper to build complicated comprehension expressions for [[BindingSeq]]
      */
    final class WithFilter(condition: A => Binding[Boolean]) {

      /**
        * Returns a [[BindingSeq]] that maps each element of this [[BindingSeq]] via `f`
        */
      def map[B](f: A => B): BindingSeq[B] = macro Macros.map

      /**
        * Returns a [[BindingSeq]] that flat-maps each element of this [[BindingSeq]] via `f`
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
          Binding {
            if (Instructions.each[Binding, Boolean](condition(a))) {
              Instructions.each[Binding, Boolean](nextCondition(a))
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
          Binding {
            if (Instructions.each[Binding, Boolean](condition(a))) {
              Constants(Instructions.each[Binding, B](f(a)))
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
          Binding {
            if (Instructions.each[Binding, Boolean](condition(a))) {
              Instructions.each[Binding, BindingSeq[B]](f(a))
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
    *
    * @group expressions
    */
  final case class Constants[+A](override val get: A*) extends AnyVal with BindingSeq[A] {

    @inline
    override private[binding] def removePatchedListener(listener: PatchedListener[A]): Unit = {}

    @inline
    override private[binding] def addPatchedListener(listener: PatchedListener[A]): Unit = {}

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
    *
    * @group expressions
    */
  final class Vars[A] private(private var cache: Vector[A]) extends BindingSeq[A] {

    private[binding] val publisher = new Publisher[PatchedListener[A]]

    /**
      * Returns a [[scala.collection.mutable.Buffer]] that allow you change the content of this [[Vars]].
      *
      * Whenever you change the returned buffer,
      * other binding expressions that depend on this [[Vars]] will be automatically changed.
      * 
      * @note This method must not be invoked inside a `@dom` method body.
      */
    @inline
    override def get: Buffer[A] = new Proxy

    private[binding] final class Proxy extends Buffer[A] {
      override def apply(n: Int): A = {
        cache.apply(n)
      }

      override def update(n: Int, newelem: A): Unit = {
        for (listener <- publisher) {
          listener.patched(new PatchedEvent(Vars.this, cache, n, SingleSeq(newelem), 1))
        }
        cache = cache.updated(n, newelem)
      }

      override def clear(): Unit = {
        for (listener <- publisher) {
          listener.patched(new PatchedEvent(Vars.this, cache, 0, Nil, cache.length))
        }
        cache = Vector.empty
      }

      override def length: Int = {
        cache.length
      }

      override def remove(n: Int): A = {
        for (listener <- publisher) {
          listener.patched(new PatchedEvent(Vars.this, cache, n, Nil, 1))
        }
        val result = cache(n)
        cache = cache.patch(n, Nil, 1)
        result
      }

      override def ++=(elements: TraversableOnce[A]): this.type = {
        val seq = elements.toVector
        for (listener <- publisher) {
          listener.patched(new PatchedEvent(Vars.this, cache, cache.length, seq, 0))
        }
        cache = seq ++ cache
        Proxy.this
      }

      override def +=:(elem: A): this.type = {
        for (listener <- publisher) {
          listener.patched(new PatchedEvent(Vars.this, cache, 0, SingleSeq(elem), 0))
        }
        cache = elem +: cache
        Proxy.this
      }

      override def +=(elem: A): this.type = {
        for (listener <- publisher) {
          listener.patched(new PatchedEvent(Vars.this, cache, cache.length, SingleSeq(elem), 0))
        }
        cache = cache :+ elem
        Proxy.this
      }

      override def insertAll(n: Int, elems: Traversable[A]): Unit = {
        val seq = elems.toSeq
        for {
          listener <- publisher
        } {
          listener.patched(new PatchedEvent(Vars.this, cache, n, seq, 0))
        }
        cache = cache.patch(n, seq, 0)
      }

      override def iterator: Iterator[A] = {
        cache.iterator
      }
    }

    override private[binding] def removePatchedListener(listener: PatchedListener[A]): Unit = {
      publisher.unsubscribe(listener)
    }

    override private[binding] def addPatchedListener(listener: PatchedListener[A]): Unit = {
      publisher.subscribe(listener)
    }

  }

  /**
    * A mechanism that mounts the result of a data binding expression into DOM or other system.
    *
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
    * A mechanism that mounts the result of a data binding expression of a sequence into DOM or other system.
    *
    * @group expressions
    */
  abstract class MultiMountPoint[-Element](upstream: BindingSeq[Element]) extends MountPoint {

    private[binding] final def mount(): Unit = {
      upstream.addPatchedListener(upstreamListener)
      set(upstream.get)
    }

    private[binding] final def unmount(): Unit = {
      upstream.removePatchedListener(upstreamListener)
      set(Seq.empty)
    }

    protected def set(children: Seq[Element]): Unit

    protected def splice(oldSeq: Seq[Element], from: Int, that: GenSeq[Element], replaced: Int): Unit

    private val upstreamListener = new PatchedListener[Element] {

      override private[binding] def patched(event: PatchedEvent[Element]): Unit = {
        splice(event.oldSeq, event.from, event.that, event.replaced)
      }

    }

  }

  /**
    * A mechanism that mounts the result of a data binding expression of a single value into DOM or other system.
    *
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

  private[binding] object DummyPatchedListener extends PatchedListener[Any] {
    @inline
    override private[binding] def patched(event: PatchedEvent[Any]): Unit = {}
  }

  private[binding] object DummyChangedListener extends ChangedListener[Any] {
    @inline
    override private[binding] def changed(event: ChangedEvent[Any]): Unit = {}
  }

}

/**
  * A data binding expression that represent a value that automatically re-calculates when its dependencies change.
  *
  * You may compose a data binding expression via `Binding { ??? }` block,
  * or add `@dom` annotation to a method that produce a data binding expression.
  *
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
trait Binding[+A] extends Any {

  @deprecated(message = "Use [[Binding#bind]] instead", since = "7.0.0")
  final def each: A = macro Binding.Macros.bind

  /**
    * Returns the current value of this [[Binding]] and mark the current `@dom` method depend on this [[Binding]].
    *
    * Each time the value changes, in the current `@dom` method,
    * all code after the current `bind` expression will be re-evaluated if the current `@dom` method is [[#watch]]ing.
    * However, code in current `@dom` method and before the current `bind` expression will not be re-evaluated.
    * This rule is not applied to DOM nodes created by XHTML literal.
    * A change related to a DOM node does not affect siblings and parents of the node.
    *
    * @note This method must be invoked inside a `@dom` method body.
    */
  final def bind: A = macro Binding.Macros.bind

  /**
    * Returns the current value of this [[Binding]]
    *
    * @note This method must not be invoked inside a `@dom` method body.
    */
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
    addChangedListener(Binding.DummyChangedListener)
  }

  /**
    * Disable automatically re-calculation.
    *
    * @note This method is recursive, which means that the dependencies of this [[Binding]] will be unwatched as well.
    */
  @inline
  final def unwatch(): Unit = {
    removeChangedListener(Binding.DummyChangedListener)
  }

}

