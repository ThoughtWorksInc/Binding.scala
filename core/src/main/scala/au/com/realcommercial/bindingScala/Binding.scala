package au.com.realcommercial.bindingScala

import scala.annotation.tailrec
import scalaz.Monad

object Binding {

  final case class Constant[A](override val value: A) extends Binding[A] {

    override def unsubscribe(subscriber: () => Unit): Unit = {}

    override def subscribe(subscriber: () => Unit): Unit = {}

  }

  final class BindableVariable[A](private var cache: A) extends Publisher[() => Unit] with Binding[A] {

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

  }

  final class FlatMap[A, B](upstream: Binding[A], f: A => Binding[B]) extends Publisher[() => Unit] with Binding[B] with (() => Unit) {

    private val cacheChangeHandler = { () =>
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
      cache.unsubscribe(cacheChangeHandler)
      val newCache = f(upstream.value)
      newCache.subscribe(cacheChangeHandler)
      cache = newCache
      if (oldValue != newCache.value) {
        for ((subscriber, _) <- this) {
          subscriber()
        }
      }
    }

    var cache: Binding[B] = f(upstream.value)

    override final def value: B = {
      @tailrec
      def tailrecGetValue(binding: Binding[B]): B = {
        binding match {
          case flatMap: FlatMap[_, B] => tailrecGetValue(flatMap.cache)
          case _ => binding.value
        }
      }
      tailrecGetValue(cache)
    }

    override def unsubscribe(subscriber: () => Unit): Unit = {
      super.unsubscribe(subscriber)
      if (super.isEmpty) {
        upstream.unsubscribe(this)
        cache.unsubscribe(cacheChangeHandler)
      }
    }

  }

  implicit object BindingInstances extends Monad[Binding] {
    override def bind[A, B](fa: Binding[A])(f: (A) => Binding[B]): Binding[B] = new FlatMap[A, B](fa, f)

    override def point[A](a: => A): Binding[A] = Constant(a)
  }

}


trait Binding[A] {

  def value: A

  def unsubscribe(subscriber: () => Unit): Unit

  def subscribe(subscriber: () => Unit): Unit

}
