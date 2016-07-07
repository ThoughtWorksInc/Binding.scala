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

import Binding._
import com.thoughtworks.each.Monadic._
import scala.collection.mutable.ArrayBuffer
import utest._

import scalaz._

object BindingTest extends TestSuite {

  final class BufferListener extends ArrayBuffer[Any] {
    val listener = new ChangedListener[Any] with PatchedListener[Any] {
      override private[binding] def changed(event: ChangedEvent[Any]): Unit = {
        BufferListener.this += event
      }

      override private[binding] def patched(event: PatchedEvent[Any]): Unit = {
        BufferListener.this += event
      }
    }
  }

  def tests = TestSuite {

    * - {
      val target = Var("World")
      val hello = monadic[Binding] {
        "Hello, " + target.each + "!"
      }
      hello.watch()

      assert(hello.get == "Hello, World!")
      target := "Each"
      assert(hello.get == "Hello, Each!")
    }

    'TripleBinding {
      val input = Var(0)
      val output = monadic[Binding] {
        input.each + input.each + input.each
      }
      output.watch()
      assert(output.get == 0)
      for (i <- 0 until 10) {
        input := i
        assert(output.get == i * 3)
      }
    }

    'DataBindingShouldBeSupportedByScalaz {

      val expr3: Var[Int] = new Var(2000)

      val expr4: Binding[Int] = monadic[Binding] {
        30000
      }

      val expr2: Binding[Int] = monadic[Binding] {
        expr3.each + expr4.each
      }

      val expr1: Binding[Int] = monadic[Binding] {
        expr2.each + 100
      }


      var resultChanged = 0

      val expr1Value0 = expr1.get

      expr1.addChangedListener(new ChangedListener[Any] {
        override def changed(event: ChangedEvent[Any]): Unit = {
          resultChanged += 1
        }
      })

      assert(resultChanged == 0)
      assert(expr1.get == expr1Value0)
      assert(expr1.get == 32100)

      expr3 := 4000


      assert(resultChanged == 1)
      assert(expr1.get != expr1Value0)
      assert(expr1.get == 34100)

    }

    'CacheShouldBeUpdated {
      val source = new Var(2.0)
      val constant = new Constant(1.0)
      val result = monadic[Binding] {
        val sourceValue = source.each
        val one = sourceValue / sourceValue / constant.each
        one / sourceValue
      }
      var resultChanged = 0

      result.addChangedListener(new ChangedListener[Any] {
        override def changed(event: ChangedEvent[Any]): Unit = {
          resultChanged += 1
        }
      })
      assert(result.get == 0.5)
      assert(resultChanged == 0)
      source := 4.0
      assert(result.get == 0.25)
      assert(resultChanged == 1)
    }

    'ForYieldWithFilter {
      val prefix = new Var("ForYield")
      val source = Vars(1, 2, 3)
      val mapped = (for {
        sourceElement <- source
        if prefix.each != sourceElement.toString
        i <- Constants((0 until sourceElement): _*)
      } yield {
        raw"""${prefix.each} $i/$sourceElement"""
      }).asInstanceOf[FlatMapBinding[_, String]]
      val mappedEvents = new BufferListener
      val sourceEvents = new BufferListener
      mapped.addPatchedListener(mappedEvents.listener)
      assert(mapped.publisher.nonEmpty)
      assert(source.publisher.nonEmpty)
      source.addPatchedListener(sourceEvents.listener)
      assert(mapped.publisher.nonEmpty)
      assert(source.publisher.nonEmpty)

      assert(sourceEvents == ArrayBuffer.empty)
      source.get.clear()
      assert(sourceEvents.length == 1)
      assert(mappedEvents.length == 1)
      sourceEvents(0) match {
        case event: PatchedEvent[_] =>
          assert(event.that.isEmpty)
          assert(event.from == 0)
          assert(event.replaced == 3)
          assert(event.getSource == source)
          assert(event.oldSeq == Seq(1, 2, 3))
      }
      mappedEvents(0) match {
        case event: PatchedEvent[_] =>
          assert(event.that.isEmpty)
          assert(event.from == 0)
          assert(event.replaced == 6)
          assert(event.getSource == mapped)
          assert(event.oldSeq == Seq("ForYield 0/1", "ForYield 0/2", "ForYield 1/2", "ForYield 0/3", "ForYield 1/3", "ForYield 2/3"))
      }
      source.get.append(2, 3, 4)
      assert(sourceEvents.length == 2)
      assert(mappedEvents.length == 2)
      sourceEvents(1) match {
        case event: PatchedEvent[_] =>
          assert(event.from == 0)
          assert(event.replaced == 0)
          assert(event.oldSeq == Seq())
          assert(event.that == Seq(2, 3, 4))
          assert(event.getSource == source)
      }
      mappedEvents(1) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == mapped)
          assert(event.from == 0)
          assert(event.replaced == 0)
          assert(event.oldSeq == Seq())
          assert(event.that == Seq("ForYield 0/2", "ForYield 1/2", "ForYield 0/3", "ForYield 1/3", "ForYield 2/3", "ForYield 0/4", "ForYield 1/4", "ForYield 2/4", "ForYield 3/4"))
      }
      source.get += 0
      assert(sourceEvents.length == 3)
      assert(mappedEvents.length == 2)
      sourceEvents(2) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == source)
          assert(event.from == 3)
          assert(event.replaced == 0)
          assert(event.that == Seq(0))
          assert(event.oldSeq == Seq(2, 3, 4))
      }
      source.get += 3
      assert(sourceEvents.length == 4)
      assert(mappedEvents.length == 3)
      sourceEvents(3) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == source)
          assert(event.from == 4)
          assert(event.replaced == 0)
          assert(event.that == Seq(3))
          assert(event.oldSeq == Seq(2, 3, 4, 0))
      }
      mappedEvents(2) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == mapped)
          assert(event.from == 9)
          assert(event.replaced == 0)
          assert(event.that == Seq("ForYield 0/3", "ForYield 1/3", "ForYield 2/3"))
          assert(event.oldSeq == Seq("ForYield 0/2", "ForYield 1/2", "ForYield 0/3", "ForYield 1/3", "ForYield 2/3", "ForYield 0/4", "ForYield 1/4", "ForYield 2/4", "ForYield 3/4"))
      }
      prefix := "3"
      assert(sourceEvents.length == 4)
      assert(mapped.get == Seq("3 0/2", "3 1/2", "3 0/4", "3 1/4", "3 2/4", "3 3/4"))

      mapped.removePatchedListener(mappedEvents.listener)
      source.removePatchedListener(sourceEvents.listener)

      assert(mapped.publisher.isEmpty)
      assert(source.publisher.isEmpty)
    }

    'ForYield {
      val prefix = new Var("ForYield")
      val source = Vars(1, 2, 3)
      val mapped = (for {
        sourceElement <- source
        i <- Constants((0 until sourceElement): _*)
      } yield {
        raw"""${prefix.each} $i/$sourceElement"""
      }).asInstanceOf[FlatMapBinding[_, String]]
      val mappedEvents = new BufferListener
      val sourceEvents = new BufferListener
      mapped.addPatchedListener(mappedEvents.listener)
      assert(mapped.publisher.nonEmpty)
      assert(source.publisher.nonEmpty)
      source.addPatchedListener(sourceEvents.listener)
      assert(mapped.publisher.nonEmpty)
      assert(source.publisher.nonEmpty)

      assert(sourceEvents == ArrayBuffer.empty)
      source.get.clear()
      assert(mappedEvents.length == 1)
      assert(sourceEvents.length == 1)
      sourceEvents(0) match {
        case event: PatchedEvent[_] =>
          assert(event.that.isEmpty)
          assert(event.from == 0)
          assert(event.replaced == 3)
          assert(event.getSource == source)
          assert(event.oldSeq == Seq(1, 2, 3))
      }
      mappedEvents(0) match {
        case event: PatchedEvent[_] =>
          assert(event.that.isEmpty)
          assert(event.from == 0)
          assert(event.replaced == 6)
          assert(event.getSource == mapped)
          assert(event.oldSeq == Seq("ForYield 0/1", "ForYield 0/2", "ForYield 1/2", "ForYield 0/3", "ForYield 1/3", "ForYield 2/3"))
      }
      source.get .append(2, 3, 4)
      assert(mappedEvents.length == 2)
      assert(sourceEvents.length == 2)
      sourceEvents(1) match {
        case event: PatchedEvent[_] =>
          assert(event.from == 0)
          assert(event.replaced == 0)
          assert(event.oldSeq == Seq())
          assert(event.that == Seq(2, 3, 4))
          assert(event.getSource == source)
      }
      mappedEvents(1) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == mapped)
          assert(event.from == 0)
          assert(event.replaced == 0)
          assert(event.oldSeq == Seq())
          assert(event.that == Seq("ForYield 0/2", "ForYield 1/2", "ForYield 0/3", "ForYield 1/3", "ForYield 2/3", "ForYield 0/4", "ForYield 1/4", "ForYield 2/4", "ForYield 3/4"))
      }
      source.get += 0
      assert(sourceEvents.length == 3)
      assert(mappedEvents.length == 2)
      sourceEvents(2) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == source)
          assert(event.from == 3)
          assert(event.replaced == 0)
          assert(event.that == Seq(0))
          assert(event.oldSeq == Seq(2, 3, 4))
      }
      source.get += 3
      assert(sourceEvents.length == 4)
      assert(mappedEvents.length == 3)
      sourceEvents(3) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == source)
          assert(event.from == 4)
          assert(event.replaced == 0)
          assert(event.that == Seq(3))
          assert(event.oldSeq == Seq(2, 3, 4, 0))
      }
      mappedEvents(2) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == mapped)
          assert(event.from == 9)
          assert(event.replaced == 0)
          assert(event.that == Seq("ForYield 0/3", "ForYield 1/3", "ForYield 2/3"))
          assert(event.oldSeq == Seq("ForYield 0/2", "ForYield 1/2", "ForYield 0/3", "ForYield 1/3", "ForYield 2/3", "ForYield 0/4", "ForYield 1/4", "ForYield 2/4", "ForYield 3/4"))
      }
      prefix := "p"
      assert(sourceEvents.length == 4)
      assert(mappedEvents.length == 15)
      val expected = Seq("p 0/2", "p 1/2", "p 0/3", "p 1/3", "p 2/3", "p 0/4", "p 1/4", "p 2/4", "p 3/4", "p 0/3", "p 1/3", "p 2/3")
      for (i <- 0 until 12) {
        mappedEvents(i + 3) match {
          case event: PatchedEvent[_] =>
            assert(event.getSource == mapped)
            assert(event.replaced == 1)
            assert(event.that == Seq(expected(event.from)))
        }
      }

      mapped.removePatchedListener(mappedEvents.listener)
      source.removePatchedListener(sourceEvents.listener)

      assert(mapped.publisher.isEmpty)
      assert(source.publisher.isEmpty)
    }

    'FlatMappedVarBuffer {
      val prefix = new Var("")
      val source = Vars(1, 2, 3)
      val mapped = new FlatMapBinding(source, { sourceElement: Int =>
        new MapBinding(Constants((0 until sourceElement): _*), { i: Int =>
          monadic[Binding] {
            raw"""${prefix.each}$sourceElement"""
          }
        })
      })
      val mappedEvents = new BufferListener
      val sourceEvents = new BufferListener
      mapped.addPatchedListener(mappedEvents.listener)
      assert(mapped.publisher.nonEmpty)
      assert(source.publisher.nonEmpty)
      source.addPatchedListener(sourceEvents.listener)
      assert(mapped.publisher.nonEmpty)
      assert(source.publisher.nonEmpty)

      assert(sourceEvents == ArrayBuffer.empty)
      source.get.clear()
      assert(mappedEvents.length == 1)
      assert(sourceEvents.length == 1)
      sourceEvents(0) match {
        case event: PatchedEvent[_] =>
          assert(event.that.isEmpty)
          assert(event.from == 0)
          assert(event.replaced == 3)
          assert(event.getSource == source)
          assert(event.oldSeq == Seq(1, 2, 3))
      }
      mappedEvents(0) match {
        case event: PatchedEvent[_] =>
          assert(event.that.isEmpty)
          assert(event.from == 0)
          assert(event.replaced == 6)
          assert(event.getSource == mapped)
          assert(event.oldSeq == Seq("1", "2", "2", "3", "3", "3"))
      }
      source.get.append(2, 3, 4)
      assert(mappedEvents.length == 2)
      assert(sourceEvents.length == 2)
      sourceEvents(1) match {
        case event: PatchedEvent[_] =>
          assert(event.from == 0)
          assert(event.replaced == 0)
          assert(event.oldSeq == Seq())
          assert(event.that == Seq(2, 3, 4))
          assert(event.getSource == source)
      }
      mappedEvents(1) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == mapped)
          assert(event.from == 0)
          assert(event.replaced == 0)
          assert(event.oldSeq == Seq())
          assert(event.that == Seq("2", "2", "3", "3", "3", "4", "4", "4", "4"))
      }
      source.get += 0
      assert(sourceEvents.length == 3)
      assert(mappedEvents.length == 2)
      sourceEvents(2) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == source)
          assert(event.from == 3)
          assert(event.replaced == 0)
          assert(event.that == Seq(0))
          assert(event.oldSeq == Seq(2, 3, 4))
      }
      source.get += 3
      assert(sourceEvents.length == 4)
      assert(mappedEvents.length == 3)
      sourceEvents(3) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == source)
          assert(event.from == 4)
          assert(event.replaced == 0)
          assert(event.that == Seq(3))
          assert(event.oldSeq == Seq(2, 3, 4, 0))
      }
      mappedEvents(2) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == mapped)
          assert(event.from == 9)
          assert(event.replaced == 0)
          assert(event.that == Seq("3", "3", "3"))
          assert(event.oldSeq == Seq("2", "2", "3", "3", "3", "4", "4", "4", "4"))
      }
      prefix := "p"
      assert(sourceEvents.length == 4)
      assert(mappedEvents.length == 15)
      val expected = Seq("p2", "p2", "p3", "p3", "p3", "p4", "p4", "p4", "p4", "p3", "p3", "p3")
      for (i <- 0 until 12) {
        mappedEvents(i + 3) match {
          case event: PatchedEvent[_] =>
            assert(event.getSource == mapped)
            assert(event.replaced == 1)
            assert(event.that == Seq(expected(event.from)))
        }
      }

      mapped.removePatchedListener(mappedEvents.listener)
      source.removePatchedListener(sourceEvents.listener)

      assert(mapped.publisher.isEmpty)
      assert(source.publisher.isEmpty)
    }

    'MappedVarBuffer {
      val prefix = new Var("")
      val source = Vars(1, 2, 3)
      val mapped = new MapBinding(source, { a: Int =>
        monadic[Binding] {
          raw"""${prefix.each}${a}"""
        }
      })
      val mappedEvents = new BufferListener
      val sourceEvents = new BufferListener
      mapped.addPatchedListener(mappedEvents.listener)
      assert(mapped.publisher.nonEmpty)
      assert(source.publisher.nonEmpty)
      source.addPatchedListener(sourceEvents.listener)
      assert(mapped.publisher.nonEmpty)
      assert(source.publisher.nonEmpty)

      assert(sourceEvents == ArrayBuffer.empty)
      source.get.clear()
      assert(mappedEvents.length == 1)
      assert(sourceEvents.length == 1)
      sourceEvents(0) match {
        case event: PatchedEvent[_] =>
          assert(event.that.isEmpty)
          assert(event.from == 0)
          assert(event.replaced == 3)
          assert(event.getSource == source)
          assert(event.oldSeq == Seq(1, 2, 3))
      }
      mappedEvents(0) match {
        case event: PatchedEvent[_] =>
          assert(event.that.isEmpty)
          assert(event.from == 0)
          assert(event.replaced == 3)
          assert(event.getSource == mapped)
          assert(event.oldSeq == Seq("1", "2", "3"))
      }
      source.get.append(2, 3, 4)
      assert(mappedEvents.length == 2)
      assert(sourceEvents.length == 2)
      sourceEvents(1) match {
        case event: PatchedEvent[_] =>
          assert(event.from == 0)
          assert(event.replaced == 0)
          assert(event.oldSeq == Seq())
          assert(event.that == Seq(2, 3, 4))
          assert(event.getSource == source)
      }
      mappedEvents(1) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == mapped)
          assert(event.from == 0)
          assert(event.replaced == 0)
          assert(event.oldSeq == Seq())
          assert(event.that == Seq("2", "3", "4"))
      }
      source.get += 20
      assert(sourceEvents.length == 3)
      assert(mappedEvents.length == 3)
      sourceEvents(2) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == source)
          assert(event.from == 3)
          assert(event.replaced == 0)
          assert(event.that == Seq(20))
          assert(event.oldSeq == Seq(2, 3, 4))
      }
      mappedEvents(2) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == mapped)
          assert(event.from == 3)
          assert(event.replaced == 0)
          assert(event.that == Seq("20"))
          assert(event.oldSeq == Seq("2", "3", "4"))
      }
      300 +=: source.get
      assert(mappedEvents.length == 4)
      assert(sourceEvents.length == 4)
      sourceEvents(3) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == source)
          assert(event.from == 0)
          assert(event.replaced == 0)
          assert(event.that == Seq(300))
          val oldSeq = event.oldSeq
          assert(oldSeq == Seq(2, 3, 4, 20))
      }
      mappedEvents(3) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == mapped)
          assert(event.from == 0)
          assert(event.replaced == 0)
          assert(event.that == Seq("300"))
          assert(event.oldSeq == Seq("2", "3", "4", "20"))
      }
      prefix := "p"
      assert(sourceEvents.length == 4)
      assert(mappedEvents.length == 9)
      val expected = Seq("p300", "p2", "p3", "p4", "p20")
      for (i <- 0 until 5) {
        mappedEvents(i + 4) match {
          case event: PatchedEvent[_] =>
            assert(event.getSource == mapped)
            assert(event.replaced == 1)
            assert(event.that == Seq(expected(event.from)))
        }
      }

      mapped.removePatchedListener(mappedEvents.listener)
      source.removePatchedListener(sourceEvents.listener)

      assert(mapped.publisher.isEmpty)
      assert(source.publisher.isEmpty)
    }

    'Length {
      val source = Vars(1)
      val length = source.length
      val lengthEvents = new BufferListener
      length.addChangedListener(lengthEvents.listener)
      source.get(0) = 100
      assert(lengthEvents.length == 1)
      lengthEvents(0) match {
        case event: ChangedEvent[_] =>
          assert(event.getSource == length)
          assert(event.oldValue == 1)
          assert(event.newValue == 1)
      }

      source.get += 200
      assert(lengthEvents.length == 2)
      lengthEvents(1) match {
        case event: ChangedEvent[_] =>
          assert(event.getSource == length)
          assert(event.oldValue == 1)
          assert(event.newValue == 2)
      }

      source.get -= 100
      assert(lengthEvents.length == 3)
      lengthEvents(2) match {
        case event: ChangedEvent[_] =>
          assert(event.getSource == length)
          assert(event.oldValue == 2)
          assert(event.newValue == 1)
      }

    }

    'WithFilter {
      monadic[Binding] {
        val myVars = Vars(1, 2, 100, 3)
        val filtered = myVars.withFilter(_ < 10).map(x => x)

        assert(filtered.get == Seq(1, 2, 3))
      }
    }

  }

}
