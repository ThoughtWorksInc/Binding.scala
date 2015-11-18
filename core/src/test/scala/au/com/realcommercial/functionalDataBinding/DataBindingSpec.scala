package au.com.realcommercial.functionalDataBinding

import com.thoughtworks.each.Monadic._
import scala.collection.mutable.Buffer
import utest._

import scalaz._

object DataBindingTest extends TestSuite {
  def tests = TestSuite {

    'DataBindingShouldBeSupportedByScalaz {

      var reset3Option: Option[Int => Unit] = None

      val var3: Binding[Int] = Cont { (callback: Int => Unit) =>
        callback(2000)
        reset3Option = Some(callback)
      }

      val var4: Binding[Int] = monadic[Binding] {
        30000
      }

      val var2: Binding[Int] = monadic[Binding] {
        var3.each + var4.each
      }

      val var1: Binding[Int] = monadic[Binding] {
       var2.each + 100
      }

      val results = Buffer.empty[Int]
      assert(results == Buffer.empty)

      var1 { newValue =>
        results += newValue
      }

      assert(results == Buffer(32100))

      val Some(reset3) = reset3Option
      reset3(4000)

      assert(results == Buffer(32100, 34100))

    }
  }

}
