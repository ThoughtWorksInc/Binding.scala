package com.thoughtworks.binding

import org.scalatest.{FreeSpec, Inside, Matchers, _}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class NotJavaFXSpec extends FreeSpec with Matchers with Inside {
  "Random" in {
    import java.util._
    @fxml val random = <Random seed={1}/>
    random.watch()
    random.value should be(a[Random])
    random.value.nextInt should be(-1155869325)
  }
}
