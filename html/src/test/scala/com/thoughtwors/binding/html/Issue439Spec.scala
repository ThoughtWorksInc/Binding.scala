package com.thoughtworks.binding
package html
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import com.thoughtworks.dsl.keywords.Await
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.macros.Reset.Default.`*`
import Binding.BindingSeq
import bindable.BindableSeq
import org.scalajs.dom.Node
import scala.concurrent.Future
import org.scalajs.dom.Element
import org.scalajs.dom.document

final class Issue439Spec extends AsyncFreeSpec with Matchers {

  "The content of element 1 is an argument; element 2 is empty" in {
    val children = html"<section>${"argument"}</section><footer></footer>"
    *[Future] {
      (
        for snapshot <- !Await(children.snapshots.toLazyList)
        yield {
          for node <- snapshot.toList
          yield node.asInstanceOf[Element].outerHTML
        }.mkString
      ) should be(
        LazyList(
          "",
          "<section>argument</section>",
          "<section>argument</section><footer></footer>"
        )
      )
    }
  }

  "The content of element 1 is an argument and some more text; element 2 is empty" in {
    val children = html"<section>${"argument"} and some more text</section><footer></footer>"
    *[Future] {
      (
        for snapshot <- !Await(children.snapshots.toLazyList)
        yield {
          for node <- snapshot.toList
          yield node.asInstanceOf[Element].outerHTML
        }.mkString
      ) should be(
        LazyList(
          "",
          "<section>argument and some more text</section>",
          "<section>argument and some more text</section><footer></footer>"
        )
      )
    }
  }

}
