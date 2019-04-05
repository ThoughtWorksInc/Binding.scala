package com.thoughtworks.binding

import org.scalatest.{FreeSpec, Inside, Matchers, _}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class DateSpec extends FreeSpec with Matchers with Inside {
  "Date" in {
    @fxml val date = {
      <?import scala.scalajs.js.Date?>
      <Date date={2}>
        <milliseconds>{42}</milliseconds>
      </Date>
    }
    date.watch()
    date.value.getDate should be(2)
    date.value.getMilliseconds should be(42)
  }
}
