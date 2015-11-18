package au.com.realcommercial.functionalDataBinding

import com.thoughtworks.each.Monadic._
import org.junit.runner.RunWith
import scala.collection.mutable.Buffer
import org.specs2.runner.JUnitRunner

import scalaz._

@RunWith(classOf[JUnitRunner])
class DataBindingSpec extends org.specs2.mutable.Specification {

  "xxxx" >> {
    "xxx" >> {

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

      var1 { newValue =>
        results += newValue
      }

      val Some(reset3) = reset3Option
      reset3(4000)

      results must equalTo(Buffer(32100, 34100))

    }
  }

}
