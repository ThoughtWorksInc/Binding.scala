package com.thoughtworks.binding

import com.thoughtworks.binding.Binding.{BindingSeq, Constant}
import com.thoughtworks.binding.dom.Runtime.TagsAndTags2
import org.scalajs.dom.Node
import org.scalatest.{FreeSpec, Matchers}
import org.scalajs.dom.html.Div

/**
  * @author Leonid Turnaev &lt;lmars@mail.ru&gt;
  */
class DomComponentTest extends FreeSpec with Matchers {

  "without children" - {
    implicit final class UserTags(x: TagsAndTags2.type) {
      @dom
      def dialog(): Binding[Div] = <div class="dialog"/>
    }

    "can be self closed tag" in {
      @dom val html = <div><dialog/></div>
      html.watch()

      assert(html.get.outerHTML == """<div><div class="dialog"/></div>""")
    }

    "can be empty tag" in {
      @dom val html = <div><dialog></dialog></div>
      html.watch()

      assert(html.get.outerHTML == """<div><div class="dialog"/></div>""")
    }

    "should not compile with children" in {
      assertDoesNotCompile("@dom val html = <div><dialog><div/></dialog></div>")
    }

    "also should not compile with child text node" in {
      assertDoesNotCompile("@dom val html = <div><dialog>    </dialog></div>")
    }
  }

  "with children" - {
    implicit final class UserTags(x: TagsAndTags2.type) {
      @dom
      def dialog(children: BindingSeq[Node]): Binding[Div] = <div class="dialog">{children}</div>
    }

    "can have single child" in {
      @dom val html = <div><dialog><div/></dialog></div>
      html.watch()

      assert(html.get.outerHTML == """<div><div class="dialog"><div/></div></div>""")
    }

    "can have more than one child" in {
      @dom val html = <div><dialog><div/><button><i>OK</i></button></dialog></div>
      html.watch()

      assert(html.get.outerHTML == """<div><div class="dialog"><div/><button><i>OK</i></button></div></div>""")
    }

    "can have child text node" in {
      @dom val html = <div><dialog>    </dialog></div>
      html.watch()

      assert(html.get.outerHTML == """<div><div class="dialog">    </div></div>""")
    }

    "should not compile as self closed tag" in {
      assertDoesNotCompile("@dom val html = <div><dialog/></div>")
    }

    "should not compile as empty tag" in {
      assertDoesNotCompile("@dom val html = <div><dialog></dialog></div>")
    }
  }

  "dom component" - {
    implicit final class UserTags(x: TagsAndTags2.type) {
      @dom
      def dialog(): Binding[Div] = <div class="dialog"/>

      @dom
      def dialog(id: Binding[String]): Binding[Div] = <div class="dialog" id={id.bind}/>

      @dom
      def dialog(children: BindingSeq[Node], id: Binding[String] = Constant("dialog")): Binding[Div] = <div class="rich-dialog" id={id.bind}>{children}</div>
    }

    "can be used as child" in {
      @dom val html = <div><dialog/></div>
      html.watch()

      assert(html.get.outerHTML == """<div><div class="dialog"/></div>""")
    }

    "can be used standalone" in {
      @dom val html = <dialog/>
      html.watch()

      assert(html.get.outerHTML == """<div class="dialog"/>""")
    }

    "can be used in sequence" in {
      @dom val components = <dialog/><dialog/>
      @dom val html = <div>{components.bind}</div>
      html.watch()

      assert(html.get.outerHTML == """<div><div class="dialog"/><div class="dialog"/></div>""")
    }

    "should support attributes" in {
      @dom val html = <dialog id="123"/>
      html.watch()

      assert(html.get.outerHTML == """<div id="123" class="dialog"/>""")
    }

    "should support overloading" in {
      @dom val html = <dialog id="123"><div/></dialog>
      html.watch()

      assert(html.get.outerHTML == """<div id="123" class="rich-dialog"><div/></div>""")
    }

    "should support defaults" in {
      @dom val html = <dialog><div/></dialog>
      html.watch()

      assert(html.get.outerHTML == """<div id="dialog" class="rich-dialog"><div/></div>""")
    }
  }
}