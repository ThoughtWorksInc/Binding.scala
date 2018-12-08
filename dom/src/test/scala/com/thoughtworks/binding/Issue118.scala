package com.thoughtworks.binding
import org.scalatest.{FreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo)
  */
class Issue118 extends FreeSpec with Matchers {
  "XHTML boolean attributes should compile" in {
    @dom val elementWithBooleanAttributes = <textarea hidden="hidden" draggable="true" readOnly="readOnly"></textarea>
    elementWithBooleanAttributes.watch()
    elementWithBooleanAttributes.value.getAttribute("draggable") should be("true")
    elementWithBooleanAttributes.value.getAttribute("hidden") should be("hidden")
    elementWithBooleanAttributes.value.getAttribute("readOnly") should be("readOnly")
  }
}
