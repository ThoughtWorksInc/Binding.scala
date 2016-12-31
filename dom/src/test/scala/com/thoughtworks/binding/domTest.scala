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

import Binding.{BindingSeq, Constant, Var, Vars}
import org.scalajs.dom.document
import org.scalajs.dom.html._
import org.scalajs.dom.raw.{Comment, Event, Node}
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
    assert(privateMonadicBr.get.outerHTML == "<br/>")
  }

  "EmptyElement" in {
    @dom val monadicBr: Binding[BR] = <br/>
    assert(monadicBr.get.outerHTML == "<br/>")
  }

  "TextElement" in {
    @dom val monadicDiv: Binding[Div] = <div>text</div>
    monadicDiv.watch()
    assert(monadicDiv.get.outerHTML == "<div>text</div>")
  }

  "TextInterpolationElement" in {
    @dom val monadicDiv: Binding[Div] = <div>{"text"}</div>
    monadicDiv.watch()
    assert(monadicDiv.get.outerHTML == "<div>text</div>")
  }

  "NestedElement" in {
    @dom val monadicDiv: Binding[Div] = <div> <span> text </span> </div>
    monadicDiv.watch()
    assert(monadicDiv.get.outerHTML == "<div> <span> text </span> </div>")
  }

  "ChangedElementText" in {
    val v0 = Var("original text")
    @dom val monadicDiv: Binding[Div] = <div> <span> {v0.bind} </span> </div>
    monadicDiv.watch()
    assert(monadicDiv.get.outerHTML == "<div> <span> original text </span> </div>")
    v0 := "changed"
    assert(monadicDiv.get.outerHTML == "<div> <span> changed </span> </div>")
  }

  "ForYield" in {
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

  "Attribute" in {
    val id = Var("oldId")
    @dom val hr = <hr id={id.bind}/>
    hr.watch()
    assert(hr.get.outerHTML == """<hr id="oldId"/>""")
    id := "newId"
    assert(hr.get.outerHTML == """<hr id="newId"/>""")
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
    assert(tableBinding.get.outerHTML == """<table class="my-table" title="My Tooltip"><thead><tr><td>First Name</td><td>Second Name</td><td>Age</td></tr></thead><tbody><tr><td>Steve</td><td>Jobs</td><td>10</td></tr><tr><td>Tim</td><td>Cook</td><td>12</td></tr><tr><td>Jeff</td><td>Lauren</td><td>13</td></tr></tbody></table>""")
    filterPattern := "o"
    assert(tableBinding.get.outerHTML == """<table class="my-table" title="My Tooltip"><thead><tr><td>First Name</td><td>Second Name</td><td>Age</td></tr></thead><tbody><tr><td>Steve</td><td>Jobs</td><td>10</td></tr><tr><td>Tim</td><td>Cook</td><td>12</td></tr></tbody></table>""")
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

  "NestedContentCurrentTarget" in {
    @dom def innerDiv = {
      // FIXME: Nested element of same node type does not work, e.g. <div><div>{ dom.currentTarget[Div] }</div></div>
      <div><p>The tagName of current Div is { dom.currentTarget[Div].tagName }. The tagName of current Paragraph is { dom.currentTarget[Paragraph].tagName }. </p></div>
    }
    val div = document.createElement("div")
    dom.render(div, innerDiv)
    val outerHTML = div.outerHTML
    assert(outerHTML == """<div><div><p>The tagName of current Div is DIV. The tagName of current Paragraph is P. </p></div></div>""")
  }

  "ContentCurrentTarget" in {
    @dom def innerDiv = {
        <p>The tagName of current element is { dom.currentTarget.tagName }.</p>
    }
    val div = document.createElement("div")
    dom.render(div, innerDiv)
    val outerHTML = div.outerHTML
    assert(outerHTML == """<div><p>The tagName of current element is P.</p></div>""")
  }

  "AttributeCurrentTarget" in {
    @dom def input = {
        <br id={dom.currentTarget.tagName}/>
    }
    val div = document.createElement("div")
    dom.render(div, input)
    val outerHTML = div.outerHTML
    assert(outerHTML == """<div><br id="BR"/></div>""")
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
      assert(filtered.get == Seq(1, 2, 3))
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
    @dom val input = <input id="foo" onclick={ _: Event => v := s"${foo.tagName} and ${bar.innerHTML}"} value={ v.bind }/><div> <hr class="h"/> <div><label id="bar">Label Text</label></div></div>
    val div = document.createElement("div")
    dom.render(div, input)
    assert(v.get == "Initial value")
    assert(input.get.get(0).asInstanceOf[Input].value == "Initial value")
    div.firstChild.asInstanceOf[Input].onclick(null)
    assert(v.get == "INPUT and Label Text")
    assert(input.get.get(0).asInstanceOf[Input].value == "INPUT and Label Text")
  }

  "id in Binding" in {
    val v = Var("Initial value")
    @dom val input = {
      <input id="foo" onclick={ _: Event => v := s"${foo.tagName}"}/>
      foo.value = v.bind
      foo
    }
    val div = document.createElement("div")
    dom.render(div, input)
    assert(v.get == "Initial value")
    assert(input.get.value == "Initial value")
    div.firstChild.asInstanceOf[Input].onclick(null)
    assert(v.get == "INPUT")
    assert(input.get.value == "INPUT")
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
    import scala.collection.immutable
    @dom def mySelect = <br class=""/>
  }

}

