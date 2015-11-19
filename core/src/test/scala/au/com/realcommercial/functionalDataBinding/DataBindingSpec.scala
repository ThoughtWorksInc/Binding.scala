package au.com.realcommercial.functionalDataBinding

import com.thoughtworks.each.Monadic._
import scala.collection.mutable.Buffer
import utest._

import scalaz._

object DataBindingTest extends TestSuite {
  def tests = TestSuite {

    'DataBindingShouldBeSupportedByScalaz {

      var reset3Option: Option[Int => Unit] = None

      val expr3: Binding[Int] = Cont { (reset3: Int => Unit) =>
        reset3(2000)
        reset3Option = Some(reset3)
      }

      val expr4: Binding[Int] = monadic[Binding] {
        30000
      }

      val expr2: Binding[Int] = monadic[Binding] {
        expr3.each + expr4.each
      }

      val expr1: Binding[Int] = monadic[Binding] {
        expr2.each + 100
      }

      val results = Buffer.empty[Int]
      assert(results == Buffer.empty)

      expr1 { newValue: Int =>
        results += newValue
      }

      assert(results == Buffer(32100))

      val Some(reset3) = reset3Option
      reset3(4000)

      assert(results == Buffer(32100, 34100))

    }
  }

}
