package au.com.realcommercial.bindingScala.dom

import au.com.realcommercial.bindingScala.Binding
import au.com.realcommercial.bindingScala.Binding.BindableVariable
import utest._
import utest.framework.TestSuite
import DomLiteral.bindingDom
import com.thoughtworks.each.Monadic._

object DomLiteralTest extends TestSuite {

  @bindingDom
  override def tests = TestSuite {
    'TestLiteral {
      val title = new BindableVariable("Old title")
      val html = monadic[Binding]{
<html>
  <body title={title.each}>
    <button>Hello, World!</button>
    {
      "lowercase".toUpperCase
    }
    {
      (<br/><div>{title.each}</div><hr/>)
    }
  </body>
</html>
      }

      var changeCount = 0
      html.subscribe { () =>
        changeCount += 1
      }
      assert(title.values.sum == 2)

      val oldHtmlElement = html.value
      val oldOuterHTML = html.value.outerHTML
      assert(oldOuterHTML ==
        """<html>
  <body title="Old title">
    <button>Hello, World!</button>
    LOWERCASE
    <br/><div>Old title</div><hr/>
  </body>
</html>""")

      title.value = "New title"
      val newOuterHTML = html.value.outerHTML
      assert(newOuterHTML ==
        """<html>
  <body title="New title">
    <button>Hello, World!</button>
    LOWERCASE
    <br/><div>New title</div><hr/>
  </body>
</html>""")

      assert(oldHtmlElement eq html.value)
      assert(changeCount == 0)

    }
  }

}
