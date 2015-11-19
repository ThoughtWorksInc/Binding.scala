package au.com.realcommercial.functionalDataBinding
package demo

import com.thoughtworks.each.Monadic._
import org.scalajs.dom._
import org.scalajs.dom.html.Input

import scala.scalajs.js.annotation.JSExport
import scalaz.Cont

@JSExport
object Demo1 {

  var reset3Option: Option[Int => Unit] = None

  @JSExport
  def setRandomExpr3() = {
    val Some(reset3) = reset3Option
    reset3(util.Random.nextInt(10))
  }

  @JSExport
  def main(): Unit = {
    val demo1Content = document.getElementById("demo1-content")

    val expr3: Binding[Int] = Cont { (reset3: Int => Unit) =>
      reset3(2000)
      reset3Option = Some(reset3)
    }

    val expr4: Binding[Int] = monadic[Binding] {
      30000
    }

    val expr2: Binding[Int] = monadic[Binding] {
      expr3.each + expr4.each
    }

    val expr1: Binding[Int] = monadic[Binding] {
      expr2.each + 100
    }


    expr1 { newValue: Int =>
      demo1Content.innerHTML = newValue.toString
      demo1Content.appendChild(new Input())
    }
  }

}
