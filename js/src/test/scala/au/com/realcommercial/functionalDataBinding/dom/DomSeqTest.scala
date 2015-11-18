package au.com.realcommercial.functionalDataBinding
package dom

import com.thoughtworks.each.Monadic._
import utest._

import scala.collection.mutable.ArrayBuffer
import scalaz._

object DomSeqTest extends TestSuite {

  def tests = TestSuite {
    'TestDom {
      val paragraph1 = monadic[Binding] { "Paragraph1" }

      var resetParagraph2Option: Option[String => Unit] = None

      val paragraph2 = Cont { (callback: String => Unit) =>
        callback("Paragraph2")
        resetParagraph2Option = Some(callback)
      }

      val paragraph3 = monadic[Binding] { "Paragraph3" }

      def page: Binding[ArrayBuffer[String]] = MutableSeq[ArrayBuffer].mutableSequence[Binding, String](
      paragraph1,
      paragraph2,
      paragraph3
    )

      /*
       <p>{paragraph1}</p>
       <p>{paragraph2}</p>
       <p>{paragraph3}</p>
       */

      val results = ArrayBuffer.empty[ArrayBuffer[String]]
      assert(results == ArrayBuffer.empty)


      page { result =>
        results += result
      }

      assert(results == ArrayBuffer(ArrayBuffer("Paragraph1", "Paragraph2", "Paragraph3")))

      val Some(resetParagraph2) = resetParagraph2Option
      resetParagraph2("Changed")

      assert(results == ArrayBuffer(ArrayBuffer("Paragraph1", "Changed", "Paragraph3"), ArrayBuffer("Paragraph1", "Changed", "Paragraph3")))

    }
  }

}
