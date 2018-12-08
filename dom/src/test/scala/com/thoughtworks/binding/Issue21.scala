package com.thoughtworks.binding

import org.scalatest.{FreeSpec, Matchers}
import org.scalajs.dom._

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
class Issue21 extends FreeSpec with Matchers {
  "dashed-id should compile" in {
    @dom def invalidId: Binding[html.Div] = <div id="dashed-id" class={ s"${`dashed-id`.tagName}-1" }></div>
    invalidId.watch()
    invalidId.get.className should be("DIV-1")
  }
}
