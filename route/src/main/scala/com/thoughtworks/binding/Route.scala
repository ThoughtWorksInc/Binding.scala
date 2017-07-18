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

import com.thoughtworks.binding.Binding.{SingleMountPoint, Var}
import org.scalajs.dom.{HashChangeEvent, Location, Window, window}
import upickle.default._

import scala.scalajs.js

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object Route {

  trait Format[PageState] {
    def unapply(hashText: String): Option[PageState]

    def apply(state: PageState): String
  }

  object Format {

    implicit def json[PageState: Reader: Writer] = new Format[PageState] {

      def apply(state: PageState) = {
        upickle.default.write(state)
      }

      def unapply(hashText: String) = {
        (hashText: Seq[Char]) match {
          case '#' +: rest =>
            try {
              Some(upickle.default.read[PageState](rest.mkString))
            } catch {
              case _: Exception =>
                None
            }
          case _ =>
            None
        }
      }
    }

  }

  /**
    * Let `state` always reflect the [[org.scalajs.dom.raw.Location.hash hash]] of the [[org.scalajs.dom.raw.Window.location location]] of the current [[org.scalajs.dom.window window]].
    */
  final class Hash[PageState](val state: Var[PageState], window: Window = window)(implicit format: Format[PageState])
      extends SingleMountPoint(state) {
    override protected def set(value: PageState): Unit = {
      window.location.hash = format(value)
    }

    private[binding] def updateState(): Unit = {
      window.location.hash match {
        case format(newState) =>
          state.value = newState
        case _ =>
      }
    }

    private val listener: js.Function1[HashChangeEvent, Unit] = { _: HashChangeEvent =>
      updateState()
    }

    override protected def mount(): Unit = {
      super.mount()
      updateState()
      window.addEventListener("hashchange", listener)
    }

    override protected def unmount(): Unit = {
      window.removeEventListener("hashchange", listener)
      super.unmount()
    }
  }

  object Hash {
    def apply[PageState](defaultState: PageState, window: Window = window)(
        implicit format: Format[PageState]): Hash[PageState] = {
      new Hash(Var(defaultState), window)
    }
  }

  /**
    * Let `state` always reflect the [[org.scalajs.dom.raw.Location.hash hash]] of the [[org.scalajs.dom.raw.Window.location location]] of the current [[org.scalajs.dom.window window]].
    */
  @deprecated(message = "Use `Route.Hash(state).watch() instead")
  def watchHash[PageState](state: Var[PageState])(implicit format: Format[PageState]): Unit = {
    new Hash(state).watch()
  }
}
