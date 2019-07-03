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

import scala.scalajs.js.{Thenable, UndefOr, |, Promise => JsPromise}
import Binding._

object JsPromiseBinding {

  def apply[A](promise: JsPromise[A]) = new JsPromiseBinding(promise)
  def apply[A](thenable: Thenable[A]) = new JsPromiseBinding(thenable)

}

/**
  * A wrapper that wraps a [[scala.scalajs.js.Thenable]] to a [[com.thoughtworks.binding.Binding]].
  *
  * @example This [[JsPromiseBinding]] will cache the result of the `thenable`.
  *
  *          Given a [[scala.scalajs.js.Thenable]] that will be delay executed.
  *          {{{
  *          import scala.scalajs.js
  *          var upstreamEvaluationCount1 = 0
  *          val delayedThenable = js.Promise.resolve[Unit](()).`then`[Double] { case () =>
  *            upstreamEvaluationCount1 += 1
  *            math.random
  *          }
  *          }}}
  *
  *          The execution will not be performed right now.
  *
  *          {{{
  *          val jsPromiseBinding = JsPromiseBinding(delayedThenable)
  *          upstreamEvaluationCount1 should be(0)
  *          }}}
  *
  *          When there are multiple usages of `jsPromiseBinding`, each usage should be triggered with the same value.
  *
  *          {{{
  *          var evaluationCount1 = 0
  *          var evaluationCount2 = 0
  *          var evaluationCount3 = 0
  *
  *          val usage1 = Binding {
  *            jsPromiseBinding.bind match {
  *              case Some(Right(value)) =>
  *                evaluationCount1 += 1
  *              case _ =>
  *            }
  *          }
  *
  *          val usage2 = Binding {
  *            jsPromiseBinding.bind match {
  *              case Some(Right(value)) =>
  *                evaluationCount2 += 1
  *
  *                val usage3 = Binding {
  *                  jsPromiseBinding.bind match {
  *                    case Some(Right(value)) =>
  *                      evaluationCount3 += 1
  *                    case _ =>
  *                  }
  *                }
  *
  *                val _ = usage1.bind
  *                usage3.bind
  *              case _ =>
  *            }
  *          }
  *
  *          usage2.watch()
  *
  *          upstreamEvaluationCount1 should be(0)
  *          evaluationCount1 should be(0)
  *          evaluationCount2 should be(0)
  *          evaluationCount3 should be(0)
  *          }}}
  *
  *          And each usage should be triggered once and only once.
  *
  *          {{{
  *          delayedThenable.toFuture.map { _ =>
  *            upstreamEvaluationCount1 should be(1)
  *            evaluationCount1 should be(1)
  *            evaluationCount2 should be(1)
  *            evaluationCount3 should be(1)
  *          }
  *          }}}
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class JsPromiseBinding[A](thenable: Thenable[A]) extends Binding[Option[Either[Any, A]]] {

  def this(promise: JsPromise[A]) = this(promise: Thenable[A])

  @volatile
  private var cache: Option[Either[Any, A]] = None

  private val publisher = new SafeBuffer[ChangedListener[Option[Either[Any, A]]]]

  override protected def value = cache

  override protected def removeChangedListener(listener: ChangedListener[Option[Either[Any, A]]]): Unit = {
    publisher -= listener
  }

  private var isHandlerRegistered: Boolean = false

  @inline
  private def handler(result: Either[Any, A]): Unit | Thenable[Unit] = {
    val newCache = Some(result)
    cache = newCache
    val event = new ChangedEvent[Option[Either[Any, A]]](this, newCache)
    for (listener <- publisher) {
      listener.changed(event)
    }
  }

  override protected def addChangedListener(listener: ChangedListener[Option[Either[Any, A]]]): Unit = {
    publisher += listener
    if (!isHandlerRegistered) {
      isHandlerRegistered = true
      thenable.`then`[Unit]({ result: A =>
        handler(Right(result))
      }, UndefOr.any2undefOrA({ error: Any =>
        handler(Left(error))
      }))
    }
  }
}
