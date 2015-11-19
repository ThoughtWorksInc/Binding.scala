package com.tw

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.jquery.jQuery
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object TutorialApp extends JSApp {

  def main(): Unit = {
    appendPar(document.body, "Hello World")
    jQuery("body").append("<p>[message]</p>")
    jQuery(setupUI)
  }


  def appendPar(targetNode: dom.Node, text: String): Unit = {
    val parNode = document.createElement("p")
    val textNode = document.createTextNode(text)
    parNode.appendChild(textNode)
    targetNode.appendChild(parNode)
  }

  @JSExport
  def addClickedMessage(): Unit = {
    appendPar(document.body, "You clicked the button!")
  }

  def setupUI(): Unit = {
    jQuery("#click-me-button").click(addClickedMessage _)
    jQuery("body").append("<p>Hello Worldlalala</p>")
  }
}