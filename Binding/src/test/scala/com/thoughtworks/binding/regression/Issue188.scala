package com.thoughtworks.binding.regression

import com.thoughtworks.binding.Binding
import com.thoughtworks.binding.Binding.{Var, Vars}
import org.scalatest.{FreeSpec, Matchers}

final class Issue188 extends FreeSpec with Matchers {
  "non-regression test for https://github.com/ThoughtWorksInc/Binding.scala/issues/188" in {

    val objectCache: Var[Int] = Var(1)

    val objectValue: Binding[Int] = Binding {
      val cache = objectCache.bind

      if (cache == 1) {
        objectCache.value = 2
        2
      } else {
        cache
      }
    }

    an[IllegalStateException] should be thrownBy objectValue.watch()

  }
}
