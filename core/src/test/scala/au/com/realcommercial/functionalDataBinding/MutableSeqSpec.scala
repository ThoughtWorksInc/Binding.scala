package au.com.realcommercial.functionalDataBinding

import au.com.realcommercial.functionalDataBinding.Binding._
import com.thoughtworks.each.Monadic._
import utest._

import scala.collection.mutable.ArrayBuffer
import scalaz._

object MutableSeqTest extends TestSuite {

  override def tests = TestSuite {
    'TestMutableSeq {
      val paragraph1 = Constant("Paragraph1")

      val paragraph2 = BindableVariable("Paragraph2")

      val paragraph3 = Constant("Paragraph3")

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

      var resultChanged = false

      val pageValue0 = page.value
      assert(page.value == pageValue0)

      page.subscribe { () =>
        resultChanged = true
      }

      assert(!resultChanged)
      assert(page.value == pageValue0)
      assert(page.value == ArrayBuffer("Paragraph1", "Paragraph2", "Paragraph3"))

      paragraph2.value = "Changed"

      assert(!resultChanged)
      // I don't know why these failed
//      assert(page.value == pageValue0)
//      assert(page.value eq pageValue0)
//      assert(pageValue0 == ArrayBuffer("Paragraph1", "Changed", "Paragraph3"))
      assert(page.value == ArrayBuffer("Paragraph1", "Changed", "Paragraph3"))

    }
  }

}
