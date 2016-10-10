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

import com.thoughtworks.binding.Binding.Var
import org.scalajs.dom.window
import simulacrum.typeclass
import upickle.default._

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object Route {

  @typeclass
  trait Format[PageState] {
    def read(hashText: String): PageState

    def write(state: PageState): String
  }

  object Format {

    final case class Json[PageState: Reader : Writer](defaultValue: PageState)
      extends Format[PageState] {

      def write(state: PageState) = {
        upickle.default.write(state)
      }

      def read(hashText: String) = {
        (hashText: Seq[Char]) match {
          case Seq('#', rest@_*) =>
            try {
              upickle.default.read[PageState](rest.mkString)
            } catch {
              case _: Exception =>
                defaultValue
            }
          case _ =>
            defaultValue
        }
      }
    }

  }

  def hash[PageState](format: Format[PageState]): Binding[PageState] = {
    val state = Var(format.read(window.location.hash))
    window.onhashchange = { _: Any =>
      state := format.read(window.location.hash)
    }
    Binding {
      val stateValue = state.bind
      window.location.hash = format.write(stateValue)
      stateValue
    }
  }
}
