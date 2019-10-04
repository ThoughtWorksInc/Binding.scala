package com.thoughtworks.binding
package regression
import org.scalatest.{FreeSpec, Matchers}
import Binding._
import scala.collection.mutable

final class Stackoverflow58206168 extends FreeSpec with Matchers {
  // See https://stackoverflow.com/questions//binding-scala-vars-bind-seems-to-not-work-correctly
  "Binding.scala: Vars.bind seems to not work correctly" in {
    val events = mutable.Buffer.empty[List[Int]]
    val test: Vars[Int] = Vars(1, 2, 3, 4)

    Binding(test.all.bind match { case newd => events += newd.toList }).watch()
    test.value.append(1111)
    assert(events == mutable.Buffer(List(1, 2, 3, 4), List(1, 2, 3, 4, 1111)))
  }

}
