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

import Binding.{BindingSeq, Vars, Var, Constant}
import org.scalajs.dom.document
import org.scalajs.dom.html.{Div, BR}
import org.scalajs.dom.raw.{Node, Event}
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

    'ForYieldIf {

      final case class User(firstName: Var[String], lastName: Var[String], age: Var[Int])

      val filterPattern = Var("")

      val users = Vars(
        User(Var("Steve"), Var("Jobs"), Var(10)),
        User(Var("Tim"), Var("Cook"), Var(12)),
        User(Var("Jeff"), Var("Lauren"), Var(13))
      )

      @dom
      def shouldShow(user: User): Binding[Boolean] = {
        val pattern = filterPattern.each
        if (pattern == "") {
          true
        } else if (user.firstName.each.toLowerCase.contains(pattern)) {
          true
        } else if (user.lastName.each.toLowerCase.contains(pattern)) {
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
            if shouldShow(user).each
          } yield <tr><td>{user.firstName.each}</td><td>{user.lastName.each}</td><td>{user.age.each.toString}</td></tr>
        }</tbody>
      }

      @dom
      val tableBinding = {
        <table title="My Tooltip" className="my-table"><thead><tr><td>First Name</td><td>Second Name</td><td>Age</td></tr></thead>{tbodyBinding.each}</table>
      }
      tableBinding.watch()
      assert(tableBinding.get.outerHTML == """<table class="my-table" title="My Tooltip"><thead><tr><td>First Name</td><td>Second Name</td><td>Age</td></tr></thead><tbody><tr><td>Steve</td><td>Jobs</td><td>10</td></tr><tr><td>Tim</td><td>Cook</td><td>12</td></tr><tr><td>Jeff</td><td>Lauren</td><td>13</td></tr></tbody></table>""")
      filterPattern := "o"
      assert(tableBinding.get.outerHTML == """<table class="my-table" title="My Tooltip"><thead><tr><td>First Name</td><td>Second Name</td><td>Age</td></tr></thead><tbody><tr><td>Steve</td><td>Jobs</td><td>10</td></tr><tr><td>Tim</td><td>Cook</td><td>12</td></tr></tbody></table>""")
    }

    'NodeSeq {
      @dom def nodeSeq = {
        <hr/><table id="myId"> </table><br/>
      }
      val div = document.createElement("div")
      dom.render(div, nodeSeq)
      assert(div.outerHTML == """<div><hr/><table id="myId"> </table><br/></div>""")
    }

    'DoubleInsertion {

      intercept[IllegalStateException] {
        @dom
        val child = <hr/>
        @dom
        val parent = <p><span>{child.each}</span><span>{child.each}</span></p>
        val div = document.createElement("div")
        dom.render(div, parent)
      }

    }

    'AttributeCurrentTarget {
      @dom def input = {
          <br id={dom.currentTarget.tagName}/>
      }
      val div = document.createElement("div")
      dom.render(div, input)
      val outerHTML = div.outerHTML
      assert(outerHTML == """<div><br id="BR"/></div>""")

    }

  }

}
