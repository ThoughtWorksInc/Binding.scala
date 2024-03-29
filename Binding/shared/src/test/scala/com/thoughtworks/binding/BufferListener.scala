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

import Binding.{PatchedEvent, ChangedEvent, PatchedListener, ChangedListener}
import com.thoughtworks.binding.Binding.{PatchedEvent, ChangedEvent, PatchedListener, ChangedListener}

import scala.collection.mutable.ArrayBuffer

/** @author
  *   杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class BufferListener extends ArrayBuffer[Any] {
  val listener = new ChangedListener[Seq[Any]] with PatchedListener[Any] {
    override def changed(event: ChangedEvent[Seq[Any]]): Unit = {
      BufferListener.this += event
    }

    override def patched(event: PatchedEvent[Any]): Unit = {
      BufferListener.this += event
    }
  }
}
