package com.thoughtworks.binding.regression

import com.thoughtworks.binding.Binding._
import com.thoughtworks.binding._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** @author
  *   杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class FlatMapRemove extends AnyFreeSpec with Matchers {
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
    every(events) should be("does not has left")
    autoPrint.watch()
    every(events) should be("does not has left")
    data.value += Right("1")
    every(events) should be("does not has left")
    data.value += Right("2")
    every(events) should be("does not has left")
    data.value += Right("3")
    every(events) should be("does not has left")
    data.value(1) = Left("left 2")
    events.last should be("has left")
    data.value --= Seq(Left("left 2"))
    events.last should be("does not has left")
  }
}
