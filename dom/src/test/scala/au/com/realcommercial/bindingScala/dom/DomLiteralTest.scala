package au.com.realcommercial.bindingScala.dom

import au.com.realcommercial.bindingScala.BindableRope.Subscriber
import au.com.realcommercial.bindingScala.{BindableRope, Binding}
import au.com.realcommercial.bindingScala.Binding.Var
import com.thoughtworks.each.Monadic
import org.scalajs.dom.{Node, Event, document}
import utest._
import utest.framework.TestSuite
import DomLiteral.bindingDom
import com.thoughtworks.each.Monadic._
import org.scalajs.dom.raw.Text

import scala.collection.mutable.ArrayBuffer

object DomLiteralTest extends TestSuite {

  sealed trait Record[A]

  final case class Insert[A](target: BindableRope[A], index: Int, newChildren: A*) extends Record[A]

  final case class Remove[A](target: BindableRope[A], index: Int) extends Record[A]

  final case class Update[A](target: BindableRope[A], index: Int, newChild: A) extends Record[A]

  final case class Splice[A](target: BindableRope[A], index: Int, numberOfOldChildren: Int, newChildren: A*) extends Record[A]

  final case class Recorder[A](records: ArrayBuffer[Record[A]] = ArrayBuffer.empty[Record[A]]) extends Subscriber[A] {

    override def insert(target: BindableRope[A], index: Int, newChildren: A*): Unit = {
      records += Insert(target, index, newChildren: _*)
    }

    override def update(target: BindableRope[A], index: Int, newChild: A): Unit = {
      records += Update(target, index, newChild)
    }

    override def remove(target: BindableRope[A], index: Int): Unit = {
      records += Remove(target, index)
    }

    override def splice(target: BindableRope[A], index: Int, numberOfOldChildren: Int, newChildren: A*): Unit = {
      records += Splice(target, index, numberOfOldChildren, newChildren: _*)

    }
  }

  @bindingDom
  override def tests = TestSuite {

    'TestSimpleText {

      val title = new Var("Old text");
      val bufferVariable = monadic[Binding] {
        val buffer = DomLiteral.DomRope(new Array[Node](1));
        monadic[Binding].apply[Unit](
          buffer.update(0, document.createTextNode(title.each))
        ).each
        buffer
      }

      bufferVariable.subscribe { () =>
        assert(false) // Must not trigger!
      }
      assert(bufferVariable.value.flatten(0).textContent == "Old text");
      title.value = "New text";
      assert(bufferVariable.value.flatten(0).textContent == "New text");
    }

    'TestText {

      val title = new Var("Old text");

      val bufferVariable = monadic[Binding] {
        val buffer$macro$2 = DomLiteral.DomRope(Array.fill[DomLiteral.DomRope](1)(BindableRope.Empty));
        monadic[Binding].apply[Unit](
          buffer$macro$2.update(0, DomLiteral.DomRope(title.each))
        ).each
        buffer$macro$2

      }
      bufferVariable.subscribe { () =>
        assert(false) // Must not trigger!
      }


      val recorder = new Recorder[Node]


      bufferVariable.value.subscribe(recorder)

      //      assert(recorder.records == Seq())
      assert(bufferVariable.value.flatten(0).textContent == "Old text");
      title.value = "New text";
      assert(bufferVariable.value.flatten(0).textContent == "New text");
      //      assert(recorder.records.length == 2)
      //      assert(recorder.records(0).asInstanceOf[Remove[Node]].index == 0)
      //      assert(recorder.records(1).asInstanceOf[Insert[Node]].index == 0)
      //      assert(recorder.records(1).asInstanceOf[Insert[Node]].newChildren.length == 1)
      //      assert(recorder.records(1).asInstanceOf[Insert[Node]].newChildren(0).textContent == "New text")
    }

    'TestMount {
      val recorder = new Recorder[Node]
      val recorder1 = new Recorder[Node]

      var numBuffersCreated = 0
      val title = new Var("Old text");
      val html = monadic[Binding] {
        val element = DomLiteral.TagsAndTags2.p().render
        val buffer = DomLiteral.DomRope(Array.fill[DomLiteral.DomRope](1)(BindableRope.Empty))
        buffer.subscribe(recorder)
        numBuffersCreated += 1
        monadic[Binding].apply[Unit](
          buffer.update(0, DomLiteral.DomRope(title.each))
        ).each
        buffer.subscribe(recorder1)
        DomLiteral.mount(element, buffer)
        element
      }
      html.subscribe { () =>
        assert(false) // Must not trigger!
      }

      assert(numBuffersCreated == 1)
      //      assert(recorder.records.length == 1)
      //      assert(recorder.records(0).asInstanceOf[Insert[Node]].index == 0)
      //      assert(recorder.records(0).asInstanceOf[Insert[Node]].newChildren.length == 1)
      //      assert(recorder.records(0).asInstanceOf[Insert[Node]].newChildren(0).textContent == "Old text")
      assert(html.value.outerHTML == "<p>Old text</p>")
      title.value = "New text"
      assert(numBuffersCreated == 1)
      //      assert(recorder.records.length == 3)
      //      assert(recorder.records(0).asInstanceOf[Insert[Node]].target == recorder.records(1).asInstanceOf[Remove[Node]].target)
      //      assert(recorder.records(0).asInstanceOf[Insert[Node]].target == recorder.records(2).asInstanceOf[Insert[Node]].target)
      //      assert(recorder.records(1).asInstanceOf[Splice[Node]].index == 0)
      //      assert(recorder.records(1).asInstanceOf[Splice[Node]].numberOfOldChildren == 1)
      //      assert(recorder.records(1).asInstanceOf[Splice[Node]].newChildren.length == 1)
      //      assert(recorder.records(1).asInstanceOf[Splice[Node]].newChildren(0).textContent == "New text")
      assert(html.value.outerHTML == "<p>New text</p>")
    }

    'TestLiteral {
      val title = new Var("Old title")
      val html = monadic[Binding] {
<html>
  <body title={title.each}>
    <button onclick={event: Event => println("Clicked")}>Hello, World!</button>
    {"lowercase".toUpperCase}
    {(<br/><div>{title.each}</div><hr/>)}
  </body>
</html>
      }

      var changeCount = 0
      html.subscribe { () =>
        changeCount += 1
      }
      assert(title.values.sum == 2)

      val oldHtmlElement = html.value
      val oldOuterHTML = html.value.outerHTML
      assert(
        oldOuterHTML == """<html>
  <body title="Old title">
    <button>Hello, World!</button>
    LOWERCASE
    <br/><div>Old title</div><hr/>
  </body>
</html>""")

      title.value = "New title"
      val newOuterHTML = html.value.outerHTML
      assert(
        newOuterHTML == """<html>
  <body title="New title">
    <button>Hello, World!</button>
    LOWERCASE
    <br/><div>New title</div><hr/>
  </body>
</html>""")

      assert(oldHtmlElement eq html.value)
      assert(changeCount == 0)

    }
  }

}
