package au.com.realcommercial.bindingScala

import au.com.realcommercial.bindingScala.Binding.Runtime._
import au.com.realcommercial.bindingScala.Binding._
import com.thoughtworks.each.Monadic._
import scala.collection.GenSeq
import scala.collection.mutable.{ArrayBuffer, Buffer}
import utest._

import scalaz._

object DataBindingTest extends TestSuite {

  final class BufferListener extends ArrayBuffer[Any] {
    val listener = new ChangedListener[Seq[Any]] with PatchedListener[Any] {
      override private[bindingScala] def changed(event: ChangedEvent[Seq[Any]]): Unit = {
        BufferListener.this += event
      }

      override private[bindingScala] def patched(event: PatchedEvent[Any]): Unit = {
        BufferListener.this += event
      }
    }
  }

  def tests = TestSuite {

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

    'MappedVarBuffer {
      val prefix = new Var("")
      val source = new VarBuffer(1, 2, 3)
      val mapped = new MappedSeq(source, { a: Int =>
        monadic[Binding] {
          raw"""${prefix.each}${a}"""
        }
      })
      val mappedEvents = new BufferListener
      val sourceEvents = new BufferListener
      mapped.addChangedListener(mappedEvents.listener)
      mapped.addPatchedListener(mappedEvents.listener)
      assert(mapped.patchedPublisher.size == 1)
      assert(mapped.changedPublisher.size == 1)
      assert(source.patchedPublisher.size > 0)
      assert(source.changedPublisher.size > 0)
      source.addChangedListener(sourceEvents.listener)
      source.addPatchedListener(sourceEvents.listener)
      assert(mapped.patchedPublisher.size == 1)
      assert(mapped.changedPublisher.size == 1)
      assert(source.patchedPublisher.size > 1)
      assert(source.changedPublisher.size > 1)

      assert(sourceEvents == ArrayBuffer.empty)
      source.reset(2, 3, 4)
      assert(mappedEvents.length == 1)
      assert(sourceEvents.length == 1)
      sourceEvents(0) match {
        case event: ChangedEvent[_] =>
          assert(event.oldValue == Seq(1, 2, 3))
          assert(event.newValue == Seq(2, 3, 4))
          assert(event.getSource == source)
      }
      mappedEvents(0) match {
        case event: ChangedEvent[_] =>
          assert(event.getSource == mapped)
          assert(event.oldValue == Seq("1", "2", "3"))
          assert(event.newValue == Seq("2", "3", "4"))
      }
      source.get += 20
      assert(sourceEvents.length == 2)
      assert(mappedEvents.length == 2)
      sourceEvents(1) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == source)
          assert(event.from == 3)
          assert(event.replaced == 0)
          assert(event.that == Seq(20))
          assert(event.oldSeq == Seq(2, 3, 4))
      }
      mappedEvents(1) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == mapped)
          assert(event.from == 3)
          assert(event.replaced == 0)
          assert(event.that == Seq("20"))
          assert(event.oldSeq == Seq("2", "3", "4"))
      }
      300 +=: source.get
      assert(mappedEvents.length == 3)
      assert(sourceEvents.length == 3)
      sourceEvents(2) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == source)
          assert(event.from == 0)
          assert(event.replaced == 0)
          assert(event.that == Seq(300))
          val oldSeq = event.oldSeq
          assert(oldSeq == Seq(2, 3, 4, 20))
      }
      mappedEvents(2) match {
        case event: PatchedEvent[_] =>
          assert(event.getSource == mapped)
          assert(event.from == 0)
          assert(event.replaced == 0)
          assert(event.that == Seq("300"))
          assert(event.oldSeq == Seq("2", "3", "4", "20"))
      }
      prefix := "p"
      assert(sourceEvents.length == 3)
      println(mappedEvents)
      assert(mappedEvents.length == 8)
      val expected = Seq("p300", "p2", "p3", "p4", "p20")
      for (i <- 0 until 5) {
        mappedEvents(i + 3) match {
          case event: PatchedEvent[_] =>
            assert(event.getSource == mapped)
            assert(event.replaced == 1)
            assert(event.that == Seq(expected(event.from)))
        }
      }

      mapped.removeChangedListener(mappedEvents.listener)
      mapped.removePatchedListener(mappedEvents.listener)
      source.removeChangedListener(sourceEvents.listener)
      source.removePatchedListener(sourceEvents.listener)

      assert(mapped.patchedPublisher.isEmpty)
      assert(mapped.changedPublisher.isEmpty)
      assert(source.patchedPublisher.isEmpty)
      assert(source.changedPublisher.isEmpty)
    }

  }

}
