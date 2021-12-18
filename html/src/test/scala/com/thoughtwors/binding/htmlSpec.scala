package com.thoughtworks.binding
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

// TODO: Move to html.scala once sbt-example supports extensions
final class htmlSpec extends AnyFreeSpec with Matchers {
  def test = html"<p>1&nbsp;${"foo"}2${"bar"}3</p><div id=$a name=\"my-name\">${"baz"}</div>"
}
