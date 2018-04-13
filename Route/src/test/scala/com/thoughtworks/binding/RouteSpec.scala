package com.thoughtworks.binding

import com.thoughtworks.binding.Binding.Var
import org.scalatest.{FreeSpec, Matchers}
import org.scalajs.dom.window

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class RouteSpec extends FreeSpec with Matchers {
  "route" in {
    val route = Route.Hash[Option[Int]](None)
    route.watch()
    window.location.hash should be("#[]")
    route.state.value = Some(1)
    window.location.hash should be("#[1]")
    window.location.hash = "#[4]"
    route.updateState()
    route.state.value should be(Some(4))
    window.location.hash = "#[]"
    route.updateState()
    route.state.value should be(None)
  }
}
