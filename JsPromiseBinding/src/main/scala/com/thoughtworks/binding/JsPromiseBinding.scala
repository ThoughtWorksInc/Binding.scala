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

import com.thoughtworks.binding.Binding.ChangedListener

import scala.scalajs.js.JSConverters._
import scala.scalajs.js.{Thenable, UndefOr, |, Promise => JsPromise}
import Binding._

object JsPromiseBinding {

  def apply[A](promise: JsPromise[A]) = new JsPromiseBinding(promise)

}

/**
  * A wrapper that wraps a [[scala.scalajs.js.Promise]] to a [[com.thoughtworks.binding.Binding]].
  *
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class JsPromiseBinding[A](promise: JsPromise[A]) extends Binding[Option[Either[Any, A]]] {

  @volatile
  private var cache: Option[Either[Any, A]] = None

  private val publisher = new Publisher[ChangedListener[Option[Either[Any, A]]]]

  override private[binding] def get = cache

  override private[binding] def removeChangedListener(listener: ChangedListener[Option[Either[Any, A]]]): Unit = {
    publisher.unsubscribe(listener)
  }

  private var isHandlerRegistered: Boolean = false

  private def fulfilledHandler(result: A): Unit | Thenable[Unit] = {
    val newCache = Some(Right(result))
    val event = new ChangedEvent[Option[Either[Any, A]]](this, newCache)
    for (listener <- publisher) {
      listener.changed(event)
    }
    cache = newCache
  }

  private def rejectedHandler(result: Any): Unit | Thenable[Unit] = {
    val newCache = Some(Left(result))
    val event = new ChangedEvent[Option[Either[Any, A]]](this, newCache)
    for (listener <- publisher) {
      listener.changed(event)
    }
    cache = newCache
  }

  override private[binding] def addChangedListener(listener: ChangedListener[Option[Either[Any, A]]]): Unit = {
    if (!isHandlerRegistered) {
      isHandlerRegistered = true
      promise.`then`[Unit]({ result: A =>
        fulfilledHandler(result)
      }, UndefOr.any2undefOrA({ error: Any =>
        rejectedHandler(error)
      }))
    }
    publisher.subscribe(listener)
  }
}
