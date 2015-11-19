package au.com.realcommercial.functionalDataBinding.dom

import org.scalajs.jquery.jQuery
import utest._

object TutorialTest extends TestSuite {

  TutorialApp.setupUI()

  def tests = TestSuite {
    'HelloWorld {
      assert(jQuery("p:contains('Hello Worldlalala')").length == 1)
    }
  }
}
