package au.com.realcommercial.bindingScala

import au.com.realcommercial.bindingScala.Binding._
import com.thoughtworks.each.Monadic._
import scala.collection.mutable.{ArrayBuffer, Buffer}
import utest._

import scalaz._

object DataBindingTest extends TestSuite {
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

      expr3.:=(4000)


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
      source.:=(4.0)
      assert(result.get == 0.25)
      assert(resultChanged == 1)
    }

    'VarBuffer {

      val source = new VarBuffer(1, 2, 3)
      val events = ArrayBuffer.empty[ChangedEvent[Seq[Int]]]
      source.addChangedListener(new ChangedListener[Seq[Int]] {
        override def changed(event: ChangedEvent[Seq[Int]]): Unit = {
          events += event
        }
      })
      assert(events == ArrayBuffer.empty)
      source := Seq(2, 3, 4)
      assert(events.length == 1)
      assert(events(0).getSource == source)
      assert(events(0).oldValue == Seq(1, 2, 3))
      assert(events(0).newValue == Seq(2, 3, 4))

    }

  }

}
