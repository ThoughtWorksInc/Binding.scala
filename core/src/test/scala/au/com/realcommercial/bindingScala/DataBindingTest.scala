package au.com.realcommercial.bindingScala

import au.com.realcommercial.bindingScala.Binding.BindableVariable
import com.thoughtworks.each.Monadic._
import scala.collection.mutable.Buffer
import utest._

import scalaz._

object DataBindingTest extends TestSuite {
  def tests = TestSuite {

    'DataBindingShouldBeSupportedByScalaz {

      var reset3Option: Option[Int => Unit] = None

      val expr3: BindableVariable[Int] = new BindableVariable(2000)

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
  }

}
