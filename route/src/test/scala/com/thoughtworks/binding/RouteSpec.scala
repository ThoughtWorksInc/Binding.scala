package com.thoughtworks.binding

import com.thoughtworks.binding.Binding.Var
import org.scalatest.{FreeSpec, Matchers}
import org.scalajs.dom.window

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class RouteSpec extends FreeSpec with Matchers {
  "route" in {

    val myState = Var[Option[Int]](None)
    Route.watchHash(myState)
    window.location.hash should be("#[]")

  }
}
