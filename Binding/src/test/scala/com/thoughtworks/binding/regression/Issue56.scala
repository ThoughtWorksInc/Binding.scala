package com.thoughtworks.binding.regression

import com.thoughtworks.binding.Binding
import com.thoughtworks.binding.Binding.{Var, Vars}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Test for https://github.com/ThoughtWorksInc/Binding.scala/issues/56
  * @author 杨博 (Yang Bo)
  */
final class Issue56 extends AnyFreeSpec with Matchers {

  "test" in {
    var dataSource = Var[Int](100)
    val isEnabled = Var[Boolean](false)

    val mappedData = Binding {
      dataSource.bind + 1
    }

    val result = Binding {
      if (isEnabled.bind) {
        mappedData.bind
      } else {
        0
      }
    }

    result.watch()
    dataSource.value = 300
    isEnabled.value = true
    result.get should be(301)
  }

}
