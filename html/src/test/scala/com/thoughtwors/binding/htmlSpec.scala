package com.thoughtworks.binding
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import scalaz.std.scalaFuture.given
import com.thoughtworks.dsl.Dsl
import Binding.BindingSeq
import org.scalajs.dom.Node

// TODO: Move to html.scala once sbt-example supports extensions
final class htmlSpec extends AsyncFreeSpec with Matchers {

  "html literal of multiple root nodes should be a BindingSeq[Node]" ignore {
    def myId = "my-id"
    val nodes = html"<p>1&nbsp;${"foo"}2${"bar"}3</p><div id=$myId class=\"my-class\">${"baz"}</div>"
    nodes should be(a[BindingSeq[Node]])
  }
}
