package com.thoughtworks.binding
import org.scalatest.{FreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo)
  */
class Issue118 extends FreeSpec with Matchers {
  "XHTML boolean attributes should compile" in {
    @dom val elementWithBooleanAttributes = <textarea hidden="hidden" draggable="true" readOnly="readOnly"></textarea>
    elementWithBooleanAttributes.watch()
    elementWithBooleanAttributes.get.getAttribute("draggable") should be("true")
    elementWithBooleanAttributes.get.getAttribute("hidden") should be("hidden")
    elementWithBooleanAttributes.get.getAttribute("readOnly") should be("readOnly")
  }
}
