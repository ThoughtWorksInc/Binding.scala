package au.com.realcommercial.functionalDataBinding
package dom

import com.thoughtworks.each.Monadic._
import org.scalajs.dom.Element
import utest._

import scala.collection.mutable.ArrayBuffer
import scalaz._

object DomNodeSeqTest extends TestSuite {

  override def tests = TestSuite {
    'TestDom {
      var resetParagraph2Option: Option[String => Unit] = None
      val p2: Binding[String] = Cont { (callback: String => Unit) =>
        callback("Paragraph2")
        resetParagraph2Option = Some(callback)
      }

      def page: Binding[Element] = monadic[Binding] {
        createElement(
          "div",
          DomAttributeMap(),
          MutableSeq[DomNodeSeq].mutableSequence[Binding, Any](
            monadic[Binding](createElement(
              "p",
              DomAttributeMap("onclick"->"xxx()"),
              DomNodeSeq("Paragraph1")
            )),
            p2,
            monadic[Binding](createElement(
              "p",
              DomAttributeMap(),
              DomNodeSeq("Paragraph3")
            ))
          ).each
        )
      }

      /*
      <div>
       <p>Paragraph1</p>
       {paragraph2}
       <p>Paragraph3</p>
     </div>
      */

      val results = ArrayBuffer.empty[Element]
      assert(results == ArrayBuffer.empty)


      page { result =>
        results += result
      }

      assert(results.length == 1)
      assert(results(0).outerHTML == "<div><p>Paragraph1</p>Paragraph2<p>Paragraph3</p></div>")

      val Some(resetParagraph2) = resetParagraph2Option
      resetParagraph2("Changed")
      assert(results.length == 2)
      assert(results(0) == results(1))
      assert(results(0).outerHTML == "<div><p>Paragraph1</p>Changed<p>Paragraph3</p></div>")
      assert(results(1).outerHTML == "<div><p>Paragraph1</p>Changed<p>Paragraph3</p></div>")

    }
  }

}
