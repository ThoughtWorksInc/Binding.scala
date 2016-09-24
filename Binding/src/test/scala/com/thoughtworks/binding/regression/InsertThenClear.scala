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

import com.thoughtworks.binding.Binding._
import com.thoughtworks.binding._
import org.scalatest._

import scala.collection.mutable.ArrayBuffer

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class InsertThenClear extends FreeSpec with Matchers {
  "insert then clear" in {
    val items = Vars(1 to 10: _*)

    val mapped = items.map(-_)
    mapped.watch()
    assert(mapped.get == Seq(-1, -2, -3, -4, -5, -6, -7, -8, -9, -10))

    items.get.insertAll(3, 100 to 103)
    assert(mapped.get == Seq(-1, -2, -3, -100, -101, -102, -103, -4, -5, -6, -7, -8, -9, -10))

    items.get.clear()
    assert(mapped.get == Seq.empty)
  }
}

