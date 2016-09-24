package com.thoughtworks.binding.regression

import com.thoughtworks.binding.Binding._
import com.thoughtworks.binding._
import org.scalatest._

import scala.collection.mutable.ArrayBuffer

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class FlatMapRemove extends FreeSpec with Matchers {
  "removed source of a flatMap" in {

    val data = Vars.empty[Either[String, String]]

    val left = for {
      s <- data
      if s.isLeft
    } yield s

    val events = ArrayBuffer.empty[String]
    val autoPrint = Binding {
      if (left.length.bind > 0) {
        events += "has left"
      } else {
        events += "does not has left"
      }
    }
    assert(events.forall(_ == "does not has left"))
    autoPrint.watch()
    assert(events.forall(_ == "does not has left"))
    data.get += Right("1")
    assert(events.forall(_ == "does not has left"))
    data.get += Right("2")
    assert(events.forall(_ == "does not has left"))
    data.get += Right("3")
    assert(events.forall(_ == "does not has left"))
    data.get(1) = Left("left 2")
    assert(events.last == "has left")
    data.get --= Seq(Left("left 2"))
    assert(events.last == "does not has left")
  }
}
