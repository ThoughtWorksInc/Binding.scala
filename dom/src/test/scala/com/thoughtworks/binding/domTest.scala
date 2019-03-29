/*
The MIT License (MIT)

Copyright (c) 2016 Yang Bo & REA Group Ltd.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

package com.thoughtworks.binding

import Binding.{Constant, Var, Vars}
import org.scalajs.dom.document
import org.scalajs.dom.html._
import org.scalajs.dom.raw.Event
import org.scalatest._

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class domTest extends FreeSpec with Matchers {

  @dom private def privateMonadicBr: Binding[BR] = <br/>

  "DomMethodShouldBeMonadic" in {
    @dom def monadicMethod = 1
    assert(monadicMethod == Constant(1))
  }

  "PrivateEmptyElement" in {
    assert(privateMonadicBr.value.outerHTML == "<br/>")
  }

  "EmptyElement" in {
    @dom val monadicBr: Binding[BR] = <br/>
    assert(monadicBr.value.outerHTML == "<br/>")
  }

  "TextElement" in {
    @dom val monadicDiv: Binding[Div] = <div>text</div>
    monadicDiv.watch()
    assert(monadicDiv.value.outerHTML == "<div>text</div>")
  }

  "TextInterpolationElement" in {
    @dom val monadicDiv: Binding[Div] = <div>{"text"}</div>
    monadicDiv.watch()
    assert(monadicDiv.value.outerHTML == "<div>text</div>")
  }

  "NestedElement" in {
    @dom val monadicDiv: Binding[Div] = <div> <span> text </span> </div>
    monadicDiv.watch()
    assert(monadicDiv.value.outerHTML == "<div> <span> text </span> </div>")
  }

  "ChangedElementText" in {
    val v0 = Var("original text")
    @dom val monadicDiv: Binding[Div] = <div> <span> {v0.bind} </span> </div>
    monadicDiv.watch()
    assert(monadicDiv.value.outerHTML == "<div> <span> original text </span> </div>")
    v0.value = "changed"
    assert(monadicDiv.value.outerHTML == "<div> <span> changed </span> </div>")
  }

  "ForYield" in {
    val v0 = Vars("original text 0","original text 1")
    @dom val monadicDiv: Binding[Div] = <div> <span> { for (s <- v0) yield <b>{s}</b> } </span> </div>
    monadicDiv.watch()
    val div = monadicDiv.value

    assert(monadicDiv.value.outerHTML == "<div> <span> <b>original text 0</b><b>original text 1</b> </span> </div>")

    v0.value.prepend("prepended")
    assert(div eq monadicDiv.value)
    assert(monadicDiv.value.outerHTML == "<div> <span> <b>prepended</b><b>original text 0</b><b>original text 1</b> </span> </div>")

    v0.value.remove(1)
    assert(div eq monadicDiv.value)
    assert(monadicDiv.value.outerHTML == "<div> <span> <b>prepended</b><b>original text 1</b> </span> </div>")
  }

  "Attribute" in {
    val id = Var("oldId")
    @dom val hr = <hr id={id.bind}/>
    hr.watch()
    assert(hr.value.outerHTML == """<hr id="oldId"/>""")
    id.value = "newId"
    assert(hr.value.outerHTML == """<hr id="newId"/>""")
  }

  "ForYieldIf" in {

    final case class User(firstName: Var[String], lastName: Var[String], age: Var[Int])

    val filterPattern = Var("")

    val users = Vars(
      User(Var("Steve"), Var("Jobs"), Var(10)),
      User(Var("Tim"), Var("Cook"), Var(12)),
      User(Var("Jeff"), Var("Lauren"), Var(13))
    )

    @dom
    def shouldShow(user: User): Binding[Boolean] = {
      val pattern = filterPattern.bind
      if (pattern == "") {
        true
      } else if (user.firstName.bind.toLowerCase.contains(pattern)) {
        true
      } else if (user.lastName.bind.toLowerCase.contains(pattern)) {
        true
      } else {
        false
      }
    }

    @dom
    def tbodyBinding = {
      <tbody>{
        for {
          user <- users
          if shouldShow(user).bind
        } yield <tr><td>{user.firstName.bind}</td><td>{user.lastName.bind}</td><td>{user.age.bind.toString}</td></tr>
      }</tbody>
    }

    @dom
    val tableBinding = {
      <table title="My Tooltip" className="my-table"><thead><tr><td>First Name</td><td>Second Name</td><td>Age</td></tr></thead>{tbodyBinding.bind}</table>
    }
    tableBinding.watch()
    assert(tableBinding.value.outerHTML == """<table class="my-table" title="My Tooltip"><thead><tr><td>First Name</td><td>Second Name</td><td>Age</td></tr></thead><tbody><tr><td>Steve</td><td>Jobs</td><td>10</td></tr><tr><td>Tim</td><td>Cook</td><td>12</td></tr><tr><td>Jeff</td><td>Lauren</td><td>13</td></tr></tbody></table>""")
    filterPattern.value = "o"
    assert(tableBinding.value.outerHTML == """<table class="my-table" title="My Tooltip"><thead><tr><td>First Name</td><td>Second Name</td><td>Age</td></tr></thead><tbody><tr><td>Steve</td><td>Jobs</td><td>10</td></tr><tr><td>Tim</td><td>Cook</td><td>12</td></tr></tbody></table>""")
  }

  "NodeSeq" in {
    @dom def nodeSeq = {
      <hr/><table id="myId"> </table><br/>
    }
    val div = document.createElement("div")
    dom.render(div, nodeSeq)
    assert(div.outerHTML == """<div><hr/><table id="myId"> </table><br/></div>""")
  }

  "DoubleInsertion" in {

    intercept[IllegalStateException] {
      @dom
      val child = <hr/>
      @dom
      val parent = <p><span>{child.bind}</span><span>{child.bind}</span></p>
      val div = document.createElement("div")
      dom.render(div, parent)
    }

  }

  "reference by id from nested content" in {
    @dom def innerDiv = {
      <div id="my-div"><p id="myParagraph">The tagName of current Div is { `my-div`.tagName }. The tagName of current Paragraph is { myParagraph.tagName }. </p></div>
    }
    val div = document.createElement("div")
    dom.render(div, innerDiv)
    val outerHTML = div.outerHTML
    assert(outerHTML == """<div><div id="my-div"><p id="myParagraph">The tagName of current Div is DIV. The tagName of current Paragraph is P. </p></div></div>""")
  }

  "reference by id from content" in {
    @dom def innerDiv = {
        <p id="currentElement">The tagName of current element is { currentElement.tagName }.</p>
    }
    val div = document.createElement("div")
    dom.render(div, innerDiv)
    val outerHTML = div.outerHTML
    assert(outerHTML == """<div><p id="currentElement">The tagName of current element is P.</p></div>""")
  }

  "reference by id from attributes" in {
    @dom def input = {
        <br id="myBr" class={myBr.tagName}/>
    }
    val div = document.createElement("div")
    dom.render(div, input)
    val outerHTML = div.outerHTML
    assert(outerHTML == """<div><br class="BR" id="myBr"/></div>""")
  }

  "StyleVisibility" in {
    @dom def hr = <hr style:borderLeft="123px"/>
    val div = document.createElement("div")
    dom.render(div, hr)
    assert(div.firstChild.asInstanceOf[HR].style.borderLeft == "123px")
  }

  "ComplexProperty" in {
    implicit class MyHr(hr: HR) {
      object a {
        object b {
          def c = hr.style.borderLeft
          def c_=(value: String) = {
            hr.style.borderLeft = "123px"
          }
        }
      }
    }
    @dom def hr = <hr a:b:c="hidden"/>
    val div = document.createElement("div")
    dom.render(div, hr)
    assert(div.firstChild.asInstanceOf[HR].style.borderLeft == "123px")
  }

  "WithFilter" in {
    @dom def domMethod() = {
      val myVars = Vars(1, 2, 100, 3)
      val filtered = for {
        myVar <- myVars
        if myVar < 10
      } yield myVar
      filtered.watch()
      assert(filtered.value == Seq(1, 2, 3))
    }
    domMethod()
  }

  "TextStyle" in {
    @dom def hr = <hr style="borderLeft: 123px"/>
    val div = document.createElement("div")
    dom.render(div, hr)
    assert(div.firstChild.asInstanceOf[HR].style.borderLeft == "123px")
  }

  "Class" in {
    @dom def hr = <hr class="myClass"/>
    val div = document.createElement("div")
    dom.render(div, hr)
    assert(div.firstChild.asInstanceOf[HR].className == "myClass")
  }

  "Comment" in {
    @dom def comment = <div><!--my comment--></div>
    val div = document.createElement("div")
    dom.render(div, comment)
    assert(div.innerHTML == "<div><!--my comment--></div>")
  }

  "Escape" in {
    @dom def comment = <div>&#32;</div>
    val div = document.createElement("div")
    dom.render(div, comment)
    assert(div.innerHTML == "<div> </div>")
  }

  "Entity" in {
    @dom def comment = <div>&amp;&lt;&copy;&lambda;</div>
    val div = document.createElement("div")
    dom.render(div, comment)
    assert(div.innerHTML == "<div>&amp;&lt;©λ</div>")
  }

  "CustomAttribute" in {
    @dom def hr = <hr data:custom-key="value"/>
    val div = document.createElement("div")
    dom.render(div, hr)
    assert(div.firstChild.asInstanceOf[HR].getAttribute("custom-key") == "value")
  }

  "id in BindingSeq" in {
    val v = Var("Initial value")
    @dom val input = <input id="foo" onclick={ _: Event => v.value = s"${foo.tagName} and ${bar.innerHTML}"} value={ v.bind }/><div> <hr class="h"/> <div><label id="bar">Label Text</label></div></div>
    val div = document.createElement("div")
    dom.render(div, input)
    assert(v.value == "Initial value")
    assert(input.value.value(0).asInstanceOf[Input].value == "Initial value")
    div.firstChild.asInstanceOf[Input].onclick(null)
    assert(v.value == "INPUT and Label Text")
    assert(input.value.value(0).asInstanceOf[Input].value == "INPUT and Label Text")
  }

  "id in Binding" in {
    val v = Var("Initial value")
    @dom val input = {
      <input id="foo" onclick={ _: Event => v.value = s"${foo.tagName}"}/>
      foo.value = v.bind
      foo
    }
    val div = document.createElement("div")
    dom.render(div, input)
    assert(v.value == "Initial value")
    assert(input.value.value == "Initial value")
    div.firstChild.asInstanceOf[Input].onclick(null)
    assert(v.value == "INPUT")
    assert(input.value.value == "INPUT")
  }

  "Seq in DOM" in {
    @dom def myUl = {
      <ul>{Seq(<li>data1</li>, <li>data2</li>)}</ul>
    }

    val div = document.createElement("div")

    dom.render(div, myUl)
    div.firstChild.childNodes.length should be(2)
  }

  "select " in {
    @dom def mySelect = <br class=""/>
  }

  "Option" in {
    val warning = Var(true)
    @dom val text = if(warning.bind) Some(<b>warning</b>) else None

    @dom val div = <div>{text.bind}</div>
    div.watch()

    assert(div.value.outerHTML == "<div><b>warning</b></div>")
    warning.value = false
    assert(div.value.outerHTML == "<div/>")
  }

  "OptionMonadicExpression" in {
    import scalaz.std.option._
    val firstName = Var[scala.Option[String]](Some("firstName"))
    @dom val text = firstName.bind.map(text => <span>{text}</span>)

    @dom val div = <div>{text.bind}</div>
    div.watch()

    assert(div.value.outerHTML == "<div><span>firstName</span></div>")
    firstName.value = None
    assert(div.value.outerHTML == "<div/>")
  }

  "OptionalAttribute" in {
    val id = Var[scala.Option[String]](Some("oldId"))
    @dom val hr = <hr option:id={id.bind}/>
    hr.watch()
    assert(hr.value.outerHTML == """<hr id="oldId"/>""")
    id.value = Some("newId")
    assert(hr.value.outerHTML == """<hr id="newId"/>""")
    id.value = None
    assert(hr.value.outerHTML == "<hr/>")
    id.value = Some("createdId")
    assert(hr.value.outerHTML == """<hr id="createdId"/>""")
  }


  "reference by local-id from nested content" in {
    @dom def innerDiv = {
      <div id="html-my-id" local-id="my-div"><p local-id="myParagraph" id="htmlMyParagraph">The tagName of current Div is { `my-div`.tagName }. The tagName of current Paragraph is { myParagraph.tagName }. </p></div>
    }
    val div = document.createElement("div")
    dom.render(div, innerDiv)
    val outerHTML = div.outerHTML
    assert(outerHTML == """<div><div id="html-my-id"><p id="htmlMyParagraph">The tagName of current Div is DIV. The tagName of current Paragraph is P. </p></div></div>""")
  }

  "reference by local-id from content" in {
    @dom def innerDiv = {
      <p local-id="currentElement">The tagName of current element is { currentElement.tagName }.</p>
    }
    val div = document.createElement("div")
    dom.render(div, innerDiv)
    val outerHTML = div.outerHTML
    assert(outerHTML == """<div><p>The tagName of current element is P.</p></div>""")
  }

  "reference by local-id from attributes" in {
    @dom def input = {
        <br local-id="myBr" class={myBr.tagName}/>
    }
    val div = document.createElement("div")
    dom.render(div, input)
    val outerHTML = div.outerHTML
    assert(outerHTML == """<div><br class="BR"/></div>""")
  }

  "local-id takes precedence over the id attribute" in {
    @dom def input = {
        <br id="html-id" local-id="myBr" class={myBr.tagName}/>
    }
    val div = document.createElement("div")
    dom.render(div, input)
    val outerHTML = div.outerHTML
    assert(outerHTML == """<div><br class="BR" id="html-id"/></div>""")
  }

  "local-id in BindingSeq" in {
    val v = Var("Initial value")
    @dom val input = <input local-id="foo" onclick={ _: Event => v.value = s"${foo.tagName} and ${bar.innerHTML}"} value={ v.bind }/><div> <hr class="h"/> <div><label local-id="bar">Label Text</label></div></div>
    val div = document.createElement("div")
    dom.render(div, input)
    assert(v.value == "Initial value")
    assert(input.value.value(0).asInstanceOf[Input].value == "Initial value")
    div.firstChild.asInstanceOf[Input].onclick(null)
    assert(v.value == "INPUT and Label Text")
    assert(input.value.value(0).asInstanceOf[Input].value == "INPUT and Label Text")
  }

  "local-id in Binding" in {
    val v = Var("Initial value")
    @dom val input = {
      <input local-id="foo" onclick={ _: Event => v.value = s"${foo.tagName}"}/>
      foo.value = v.bind
      foo
    }
    val div = document.createElement("div")
    dom.render(div, input)
    assert(v.value == "Initial value")
    assert(input.value.value == "Initial value")
    div.firstChild.asInstanceOf[Input].onclick(null)
    assert(v.value == "INPUT")
    assert(input.value.value == "INPUT")
  }

  "dashed-local-id should compile" in {
    @dom def div = <div local-id="dashed-id" class={ s"${`dashed-id`.tagName}-1" }></div>
    div.watch()
    assert(div.value.className == "DIV-1")
  }

  "CustomTag" in {
    @dom def tag = {
      <data:custom-tag local-id="customTag" id="custom" data:custom-key="value"><data id="123">{customTag.tagName}</data></data:custom-tag>
    }
    val div = document.createElement("div")
    dom.render(div, tag)

    assert(div.outerHTML == """<div><custom-tag custom-key="value" id="custom"><data id="123">CUSTOM-TAG</data></custom-tag></div>""")
  }

}

