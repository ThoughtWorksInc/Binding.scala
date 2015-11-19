package au.com.realcommercial.functionalDataBinding
package demo

import au.com.realcommercial.functionalDataBinding.dom._
import org.scalajs.dom._
import com.thoughtworks.each.Monadic._
import org.scalajs.dom.html.Input

import scala.collection.mutable
import scala.scalajs.js.annotation.JSExport

@JSExport
object Demo2 {

  @JSExport
  def add(firstName: String, lastName: String, age: Int): Unit = {
    users.value :+= User(BindableVariable(firstName), BindableVariable(lastName), BindableVariable(age))
  }

  @JSExport
  def filter(pattern: String): Unit = {
    users.value = users.value.filter(_.firstName.value.toLowerCase().contains(pattern.toLowerCase()))
  }

  /*
  page { result: Element =>
    results += result
  }
  */


  final case class User(firstName: BindableVariable[String], lastName: BindableVariable[String], age: BindableVariable[Int])

  val users = BindableVariable(List(
    User(BindableVariable("Steve"), BindableVariable("Jobs"), BindableVariable(10)),
    User(BindableVariable("Tim"), BindableVariable("Cook"), BindableVariable(12)),
    User(BindableVariable("Jeff"), BindableVariable("Lauren"), BindableVariable(13))
  ))
  /*
      <table id="users-table">
      <thead>
      <tr>
          <td>First Name</td>
          <td>Second Name</td>
          <td>Age</td>
      </tr>
      </thead>
      <tbody id="users-table-body">
      <tr>
          <td>Steve</td>
          <td>Jobs</td>
          <td>10</td>
      </tr>
      <tr>
          <td>Tim</td>
          <td>Cook</td>
          <td>12</td>
      </tr>
      <tr>
          <td>Jeff</td>
          <td>Lauren</td>
          <td>13</td>
      </tr>
      </tbody>
  </table>
   */

  private def bindUsersTable: Binding[Element] = monadic[Binding] {
    createElement(
      "table",
      DomAttributeMap(),
      MutableSeq[DomNodeSeq].mutableSequence[Binding, Any](
        monadic[Binding](
          createElement(
            "thead",
            DomAttributeMap(),
            DomNodeSeq(
              createElement(
                "tr",
                DomAttributeMap(),
                DomNodeSeq(
                  createElement(
                    "td",
                    DomAttributeMap(),
                    DomNodeSeq("First Name")
                  ),
                  createElement(
                    "td",
                    DomAttributeMap(),
                    DomNodeSeq("Second Name")
                  ),
                  createElement(
                    "td",
                    DomAttributeMap(),
                    DomNodeSeq("Age")
                  )
                )
              )
            )
          )
        ),
        monadic[Binding](
          createElement(
            "tbody",
            DomAttributeMap(),
            MutableSeq[DomNodeSeq].mutableSequence[Binding, Element](
              (for (user <- users.binding.each) yield {
                monadic[Binding] {
                  createElement(
                    "tr",
                    DomAttributeMap(),
                    MutableSeq[DomNodeSeq].mutableSequence[Binding, Any](
                      monadic[Binding] {
                        createElement(
                          "td",
                          DomAttributeMap(),
                          MutableSeq[DomNodeSeq].mutableSequence[Binding, Any](monadic[Binding] {
                            user.firstName.binding.each
                          }).each
                        )
                      },
                      monadic[Binding] {
                        createElement(
                          "td",
                          DomAttributeMap(),
                          MutableSeq[DomNodeSeq].mutableSequence[Binding, Any](monadic[Binding] {
                            val input = createElement(
                              "input",
                              DomAttributeMap(),
                              MutableSeq[DomNodeSeq].mutableSequence[Binding, Any](monadic[Binding] {
                                user.lastName.binding.each
                              }).each
                            )
                            input.asInstanceOf[Input].onchange = { _: Event =>
                              user.lastName.value = input.asInstanceOf[Input].value
                            }
                            input
                          }).each
                        )
                      },
                      monadic[Binding] {
                        createElement(
                          "td",
                          DomAttributeMap(),
                          MutableSeq[DomNodeSeq].mutableSequence[Binding, Any](monadic[Binding] {
                            user.age.binding.each
                          }).each
                        )
                      }
                    ).each
                  )
                }
              }): _*
            ).each
          )
        )
      ).each
    )
  }

  @JSExport
  def main(): Unit = {
    val parent = document.getElementById("users-table-parent")
    val usersTable = bindUsersTable
    usersTable { element =>
      if (parent.childElementCount == 0) {
        parent.appendChild(element)
      } else if (parent.firstChild != element) {
        parent.replaceChild(parent.firstChild, element)
      }
    }
  }

}
