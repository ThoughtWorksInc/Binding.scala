package com.thoughtworks.binding
package html
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import com.thoughtworks.dsl.keywords.Await
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.macros.Reset.Default.`*`
import Binding.BindingSeq
import org.scalajs.dom.Node
import scala.concurrent.Future
import org.scalajs.dom.Element
import org.scalajs.dom.document

// TODO: Move to html.scala once sbt-example supports extensions
final class htmlSpec extends AsyncFreeSpec with Matchers {

  "html literal of multiple root nodes should be a BindingSeq[Node]" in {
    def myId = "my-id"
    val nodes =
      html"""<p>1&nbsp;${"foo"}2${"bar"}3</p><div id=$myId class="my-class">${"baz"}</div>"""
    nodes should be(a[BindingSeq[Node]])
    *[Future] {
      (
        for snapshot <- !Await(nodes.snapshots.toLazyList)
        yield {
          for node <- snapshot.toList
          yield node.asInstanceOf[Element].outerHTML
        }.mkString
      ) should be(
        LazyList(
          "",
          "<p>1&nbsp;foo2bar3</p>",
          "<p>1&nbsp;foo2bar3</p><div class=\"my-class\" id=\"my-id\">baz</div>"
        )
      )
    }
  }

  "bug in todoapp" ignore {
    val children = html"<section>${"text"}</section><footer></footer>"
    *[Future] {
      (
        for snapshot <- !Await(children.snapshots.toLazyList)
        yield {
          for node <- snapshot.toList
          yield node.asInstanceOf[Element].outerHTML
        }.mkString
      ) should be(
        LazyList(
          "", "<footer></footer>", "<section>text</section><footer></footer>"
        )
      )
    }
  }
}
