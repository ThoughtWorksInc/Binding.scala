package com.thoughtworks.binding

import java.util.TimeZone

import org.scalatest.{FreeSpec, Inside, Matchers, _}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class CustomTagSpec extends FreeSpec with Matchers with Inside {
  "Random" in {
    import java.util._
    @fxml val random = <Random seed={1}/>
    random.watch()
    random.get should be(a[Random])
    random.get.nextInt should be(-1155869325)
  }
}
