package au.com.realcommercial.functionalDataBinding

import scalaz.Monad

object Binding {

  final case class Constant[A](override val value: A) extends Binding[A] {

    override def unsubscribe(subscriber: () => Unit): Unit = {}

    override def subscribe(subscriber: () => Unit): Unit = {}
    
    override def unsubscribeFromUpstream(): Unit = {}

  }

  final case class BindableVariable[A](private var cache: A) extends Binding[A] {

    val subscribers = collection.mutable.HashSet.empty[() => Unit]

    override def unsubscribe(subscriber: () => Unit): Unit = {
      subscribers -= subscriber
    }

    override def subscribe(subscriber: () => Unit): Unit = {
      subscribers += subscriber
    }

    override def value: A = {
      cache
    }

    def value_=(a: A): Unit = {
      if (cache != a) {
        cache = a
        for (subscriber <- subscribers) {
          subscriber()
        }
      }
    }

    override def unsubscribeFromUpstream(): Unit = {}

  }

  final case class FlatMap[A, B](upstream: Binding[A], f: A => Binding[B]) extends Binding[B] with (() => Unit) {
    val subscribers = collection.mutable.HashSet.empty[() => Unit]

    override def subscribe(subscriber: () => Unit): Unit = {
      if (subscribers.isEmpty) {
        upstream.subscribe(this)
      }
      subscribers += subscriber
    }

    override def apply(): Unit = {
      val oldValue = cache.value
      cache.unsubscribeFromUpstream()
      val newCache = f(upstream.value)
      cache = newCache
      if (oldValue != newCache.value) {
        for (subscriber <- subscribers) {
          subscriber()
        }
      }
    }

    var cache: Binding[B] = f(upstream.value)

    override def value: B = cache.value

    override def unsubscribe(subscriber: () => Unit): Unit = {
      subscribers -= subscriber
      if (subscribers.isEmpty) {
        upstream.unsubscribe(this)
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
