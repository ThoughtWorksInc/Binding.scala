package au.com.realcommercial.functionalDataBinding

import scala.annotation.tailrec
import scalaz.Monad

object Binding {

  class Publisher extends collection.mutable.HashMap[() => Unit, Int] {
    override def default(key: () => Unit) = 0

    def subscribe(key: () => Unit): Unit = {
      this (key) += 1
    }

    def unsubscribe(key: () => Unit): Unit = {
      val oldValue = this (key)
      val newValue = oldValue - 1
      if (newValue == 0) {
        this -= key
      } else {
        this (key) = newValue
      }
    }

  }

  final case class Constant[A](override val value: A) extends Binding[A] {

    override def unsubscribe(subscriber: () => Unit): Unit = {}

    override def subscribe(subscriber: () => Unit): Unit = {}

    override def unsubscribeFromUpstream(): Unit = {}

  }

  final case class BindableVariable[A](private var cache: A) extends Publisher with Binding[A] {

    override def value: A = {
      cache
    }

    def value_=(a: A): Unit = {
      if (cache != a) {
        cache = a
        for ((subscriber, _) <- this) {
          subscriber()
        }
      }
    }

    override def unsubscribeFromUpstream(): Unit = {}

  }

  final case class FlatMap[A, B](upstream: Binding[A], f: A => Binding[B]) extends Publisher with Binding[B] with (() => Unit) {

    val cacheChangeHandler = { () =>
      for ((subscriber, _) <- this) {
        subscriber()
      }
    }

    override def subscribe(subscriber: () => Unit): Unit = {
      if (super.isEmpty) {
        upstream.subscribe(this)
        cache.subscribe(cacheChangeHandler)
      }
      super.subscribe(subscriber)
    }

    override def apply(): Unit = {
      val oldValue = cache.value
      cache.unsubscribeFromUpstream()
      val newCache = f(upstream.value)
      cache = newCache
      if (oldValue != newCache.value) {
        for ((subscriber, _) <- this) {
          subscriber()
        }
      }
    }

    var cache: Binding[B] = f(upstream.value)

    @tailrec
    private def tailrecGetValue(binding: Binding[B]): B = {
      binding match {
        case flatMap: FlatMap[_, B] => tailrecGetValue(flatMap.cache)
        case _ => binding.value
      }
    }

    override final def value: B = {
      tailrecGetValue(cache)
    }

    override def unsubscribe(subscriber: () => Unit): Unit = {
      super.unsubscribe(subscriber)
      if (super.isEmpty) {
        upstream.unsubscribe(this)
        cache.unsubscribe(cacheChangeHandler)
      }
    }

    override def unsubscribeFromUpstream(): Unit = {
      upstream.unsubscribe(this)
    }
  }

  implicit object BindingInstances extends Monad[Binding] {
    override def bind[A, B](fa: Binding[A])(f: (A) => Binding[B]): Binding[B] = FlatMap[A, B](fa, f)

    override def point[A](a: => A): Binding[A] = Constant(a)
  }

}


trait Binding[A] {

  def value: A

  def unsubscribeFromUpstream(): Unit

  def unsubscribe(subscriber: () => Unit): Unit

  def subscribe(subscriber: () => Unit): Unit

}
