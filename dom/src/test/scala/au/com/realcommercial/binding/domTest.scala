package au.com.realcommercial.binding

import au.com.realcommercial.binding.Binding.{Vars, Var, Constant}
import org.scalajs.dom.html.{Div, BR}
import utest._

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object domTest extends TestSuite {

  @dom private def privateMonadicBr: Binding[BR] = <br/>

  override def tests = TestSuite {

    'DomMethodShouldBeMonadic {
      @dom def monadicMethod = 1
      assert(monadicMethod == Constant(1))
    }

    'PrivateEmptyElement {
      assert(privateMonadicBr.get.outerHTML == "<br/>")
    }

    'EmptyElement {
      @dom val monadicBr: Binding[BR] = <br/>
      assert(monadicBr.get.outerHTML == "<br/>")
    }

    'TextElement {
      @dom var monadicDiv: Binding[Div] = <div>text</div>
      monadicDiv.watch()
      assert(monadicDiv.get.outerHTML == "<div>text</div>")
    }

    'TextInterpolationElement {
      @dom val monadicDiv: Binding[Div] = <div>{"text"}</div>
      monadicDiv.watch()
      assert(monadicDiv.get.outerHTML == "<div>text</div>")
    }

    'NestedElement {
      @dom val monadicDiv: Binding[Div] = <div> <span> text </span> </div>
      monadicDiv.watch()
      assert(monadicDiv.get.outerHTML == "<div> <span> text </span> </div>")
    }

    'ChangedElementText {
      val v0 = Var("original text")
      @dom val monadicDiv: Binding[Div] = <div> <span> {v0.each} </span> </div>
      monadicDiv.watch()
      assert(monadicDiv.get.outerHTML == "<div> <span> original text </span> </div>")
      v0 := "changed"
      assert(monadicDiv.get.outerHTML == "<div> <span> changed </span> </div>")
    }

    'ForYield {
      val v0 = Vars("original text 0","original text 1")
      @dom val monadicDiv: Binding[Div] = <div> <span> { for (s <- v0) yield <b>{s}</b> } </span> </div>
      monadicDiv.watch()
      val div = monadicDiv.get

      assert(monadicDiv.get.outerHTML == "<div> <span> <b>original text 0</b><b>original text 1</b> </span> </div>")

      v0.get.prepend("prepended")
      assert(div eq monadicDiv.get)
      assert(monadicDiv.get.outerHTML == "<div> <span> <b>prepended</b><b>original text 0</b><b>original text 1</b> </span> </div>")

      v0.get.remove(1)
      assert(div eq monadicDiv.get)
      assert(monadicDiv.get.outerHTML == "<div> <span> <b>prepended</b><b>original text 1</b> </span> </div>")
    }

    'Attribute {
      val id = Var("oldId")
      @dom val hr = <hr id={id.each}/>
      hr.watch()
      assert(hr.get.outerHTML == """<hr id="oldId"/>""")
      id := "newId"
      assert(hr.get.outerHTML == """<hr id="newId"/>""")
    }

  }

}
