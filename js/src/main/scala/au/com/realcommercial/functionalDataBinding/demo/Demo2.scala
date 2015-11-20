package au.com.realcommercial.functionalDataBinding
package demo

import au.com.realcommercial.functionalDataBinding.dom._
import org.scalajs.dom.Element
import org.scalajs.dom.Event
import org.scalajs.dom.document
import org.scalajs.dom.html.Input
import com.thoughtworks.each.Monadic._
import scalaz.std.list._

import scala.collection.mutable
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._

@JSExport
object Demo2 {

  @JSExport
  def add(firstName: String, lastName: String, age: String): Unit = {
    users.value :+= User(BindableVariable(firstName), BindableVariable(lastName), BindableVariable(age.toInt))
  }

  @JSExport
  def filter(pattern: String): Unit = {
    filterPattern.value = pattern
  }

  final case class User(firstName: BindableVariable[String], lastName: BindableVariable[String], age: BindableVariable[Int])

  val filterPattern = BindableVariable("")

  val users = BindableVariable(List(
    User(BindableVariable("Steve"), BindableVariable("Jobs"), BindableVariable(10)),
    User(BindableVariable("Tim"), BindableVariable("Cook"), BindableVariable(12)),
    User(BindableVariable("Jeff"), BindableVariable("Lauren"), BindableVariable(13))
  ))

  private def shouldShow(pattern: String, user: User): Binding[Boolean] = monadic[Binding] {
    if (pattern == "") {
      true
    } else if (user.firstName.binding.each.toLowerCase.contains(pattern)) {
      true
    } else if (user.lastName.binding.each.toLowerCase.contains(pattern)) {
      true
    } else {
      false
    }
  }

  private def bindUsersTable: Binding[Element] = monadic[Binding] {
    val pattern = filterPattern.binding.each
    table(
      thead(
        tr(
          td("First Name"),
          td("Second Name"),
          td("Age")
        )
      ),
      tbody(
        (for {
          user <- users.binding.each.monadicLoop
          if shouldShow(pattern, user).each
        } yield tr(
          td(user.firstName.binding.each),
          td(user.lastName.binding.each),
          td(user.age.binding.each)
        )).underlying: _*
      )
    ).render
  }

  @JSExport
  def main(): Unit = {
    val parent = document.getElementById("users-table-parent")
    val usersTable = bindUsersTable
    usersTable { element =>
      if (parent.childElementCount == 0) {
        parent.appendChild(element)
      } else if (parent.firstChild != element) {
        parent.replaceChild(element, parent.firstChild)
      }
    }
  }
}
