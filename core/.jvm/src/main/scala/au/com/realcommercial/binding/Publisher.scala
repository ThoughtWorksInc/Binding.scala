package au.com.realcommercial.binding

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
private[binding] final class Publisher[Subscriber]
(val subscribers: ArrayBuffer[Subscriber] = ArrayBuffer.empty[Subscriber]) extends AnyVal {

  @tailrec
  final def foreach[U](f: Subscriber => U, start: Int = 0): Unit = {
    val cloned = subscribers.clone()
    if (start < cloned.length) {
      f(cloned(start))
      foreach(f, start + 1)
    }
  }

  final def isEmpty: Boolean = {
    subscribers.isEmpty
  }

  final def subscribe(subscriber: Subscriber): Unit = {
    subscribers += subscriber
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