package com.thoughtworks.binding
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.ExecutionContext.Implicits.given
import scalaz.std.scalaFuture.given
import com.thoughtworks.dsl.Dsl
import Binding.BindingSeq
import org.scalajs.dom.Node

// TODO: Move to html.scala once sbt-example supports extensions
final class htmlSpec extends AnyFreeSpec with Matchers {

  "html literal of multiple root nodes should be a BindingSeq[Node]" in {
    def myId = "xx"
    val nodes = html"<p>1&nbsp;${"foo"}2${"bar"}3</p><div id=$myId class=\"my-class\">${"baz"}</div>"
    nodes should be(a[BindingSeq[Node]])
  }
}
