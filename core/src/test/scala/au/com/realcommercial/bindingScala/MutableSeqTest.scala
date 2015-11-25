package au.com.realcommercial.bindingScala

import au.com.realcommercial.bindingScala.Binding._
import com.thoughtworks.each.Monadic._
import utest._

import scala.collection.mutable.ArrayBuffer
import scalaz._

object MutableSeqTest extends TestSuite {

  override def tests = TestSuite {
    'TestMutableSeq {
      val paragraph1 = Constant("Paragraph1")

      val paragraph2 = new BindableVariable("Paragraph2")

      val paragraph3 = Constant("Paragraph3")

      val page: Binding[ArrayBuffer[String]] = MutableSeq[ArrayBuffer].mutableSequence[Binding, String](
        paragraph1,
        paragraph2,
        paragraph3
      )

      /*
       <p>{paragraph1}</p>
       <p>{paragraph2}</p>
       <p>{paragraph3}</p>
       */

      var resultChanged = 0

      val pageValue0 = page.value
      assert(page.value eq pageValue0)
      assert(paragraph2.values.sum == 0)

      val handler = { () =>
        resultChanged += 1
      }

      page.subscribe(handler)

      assert(paragraph2.values.sum == 1)
      assert(resultChanged == 0)
      assert(page.value eq pageValue0)
      assert(page.value == ArrayBuffer("Paragraph1", "Paragraph2", "Paragraph3"))

      paragraph2.value = "Changed"

      assert(paragraph2.values.sum == 1)
      assert(resultChanged == 0)
      assert(page.value eq pageValue0)
      assert(pageValue0 == ArrayBuffer("Paragraph1", "Changed", "Paragraph3"))
      assert(page.value == ArrayBuffer("Paragraph1", "Changed", "Paragraph3"))

      paragraph2.value = "Changed, again!"

      assert(paragraph2.values.sum == 1)
      assert(resultChanged == 0)
      assert(page.value eq pageValue0)
      assert(pageValue0 == ArrayBuffer("Paragraph1", "Changed, again!", "Paragraph3"))
      assert(page.value == ArrayBuffer("Paragraph1", "Changed, again!", "Paragraph3"))

      page.unsubscribe(handler)
      assert(paragraph2.values.sum == 0)

    }
  }

}
