package au.com.realcommercial.bindingScala

import au.com.realcommercial.bindingScala.Binding.{Constant, Var}
import com.thoughtworks.each.Monadic._
import scala.collection.mutable.Buffer
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

      val expr1Value0 = expr1.value

      expr1.subscribe { () =>
        resultChanged += 1
      }

      assert(resultChanged == 0)
      assert(expr1.value == expr1Value0)
      assert(expr1.value == 32100)

      expr3.value = 4000


      assert(resultChanged == 1)
      assert(expr1.value != expr1Value0)
      assert(expr1.value == 34100)

    }

    'CacheShouldBeUpdated {
      val source = new Var(2.0)
      val constant = new Constant(1.0)
      val result = monadic[Binding] {
        val sourceValue = source.each
        val one =  sourceValue / sourceValue / constant.each
        one / sourceValue
      }
      var resultChanged = 0

      result.subscribe { () =>
        resultChanged += 1
      }
      assert(result.value == 0.5)
      assert(resultChanged == 0)
      source.value = 4.0
      assert(result.value == 0.25)
      assert(resultChanged == 1)
    }

  }

}
