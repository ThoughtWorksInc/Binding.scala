package au.com.realcommercial.binding

import scala.annotation.tailrec
import scala.scalajs.js

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
private[binding] final class Publisher[Subscriber]
(val subscribers: js.Array[Subscriber] = new js.Array[Subscriber]()) extends AnyVal {

  @tailrec
  final def foreach[U](f: Subscriber => U, start: Int = 0): Unit = {
    val cloned = subscribers.concat()
    if (start < cloned.length) {
      f(cloned(start))
      foreach(f, start + 1)
    }
  }

  @inline
  final def isEmpty: Boolean = {
    subscribers.length == 0
  }

  @inline
  final def subscribe(subscriber: Subscriber): Unit = {
    subscribers.push(subscriber)
  }

  final def unsubscribe(subscriber: Subscriber): Unit = {
    subscribers.indexOf(subscriber) match {
      case -1 =>
        throw new IllegalArgumentException(s"No such subscripber: $subscriber")
      case i =>
        subscribers.remove(i)
    }
  }

}