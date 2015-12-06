package au.com.realcommercial.bindingScala

import au.com.realcommercial.bindingScala.BindableRope._
import com.thoughtworks.each.Monadic._
import utest._

import scala.collection.mutable.ArrayBuffer


object ComprehensionTest extends TestSuite {
  def tests = TestSuite {

    'TestMap {
      val rope = new Single(5)
      assert(rope.flatten == Seq(5))
      val mappedRope = monadic[Binding] {
        rope.map(_.toString)
      }
      assert(mappedRope.value.flatten == Seq("5"))
    }

  }
}
