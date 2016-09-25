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

package com.thoughtworks.binding.regression

import com.thoughtworks.binding.Binding
import com.thoughtworks.binding.Binding.Var
import org.scalatest.{FreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class Zhihu50863924 extends FreeSpec with Matchers {

  "Newly created Binding expression should not re-render immediately" in {

    var renderCount0 = 0
    var renderCount1 = 0

    def subComponent(value: Var[Option[String]]) = Binding {
      renderCount0 += 1
      assert(value.bind == Some("Changed"))
      renderCount1 += 1
      Right(value.bind.get)
    }

    val value: Var[Option[String]] = Var(None)

    val render = Binding {
      if (value.bind.isDefined) {
        subComponent(value).bind
      } else {
        Left("None here!")
      }
    }

    render.watch()
    assert(render.get == Left("None here!"))
    value := Some("Changed")
    assert(render.get == Right("Changed"))
    assert(renderCount0 == 1)
    assert(renderCount1 == 1)
  }
}
