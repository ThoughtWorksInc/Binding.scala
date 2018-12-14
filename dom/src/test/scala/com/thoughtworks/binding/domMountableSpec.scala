package com.thoughtworks.binding
import com.thoughtworks.binding.Binding.{BindingSeq, MultiMountPoint}
import com.thoughtworks.binding.dom.Mountable
import com.thoughtworks.binding.dom.Runtime.TagsAndTags2
import org.scalajs.dom.html.HR
import org.scalatest.{FreeSpec, Inside, Matchers}

import scala.collection.GenSeq
import scala.scalajs.js

/**
  * @author 杨博 (Yang Bo)
  */
final class domMountableSpec extends FreeSpec with Matchers with Inside {
  "Mountable JavaScript WrappedArray" in {
    implicit def mountableJsArray[Element, Child](implicit ev: BindingSeq[Child] <:< BindingSeq[Element])
      : Mountable[js.WrappedArray[Element], BindingSeq[Child]] =
      new Mountable[js.WrappedArray[Element], BindingSeq[Child]] {
        def mount(parent: js.WrappedArray[Element], children: BindingSeq[Child]): Binding[Unit] =
          new MultiMountPoint[Element](children) {
            protected def set(children: Seq[Element]): Unit = {
              parent.clear()
              parent ++= children
            }
            protected def splice(from: Int, that: GenSeq[Element], replaced: Int): Unit = {
              parent.splice(from + 1, replaced, that.seq: _*)
            }
          }
      }

    implicit final class ArrayTagOps(tagsAndTags2: TagsAndTags2.type) {
      object array {
        def render = js.WrappedArray.empty[Any]
      }
    }

    @dom
    val a = <array><hr/><array/><array><array/><array/></array></array>

    a.watch()

    inside(a.get) {
      case js.WrappedArray(hr: HR, js.WrappedArray(), js.WrappedArray(js.WrappedArray(), js.WrappedArray())) =>
        hr.tagName.toLowerCase should be("hr")
    }

  }
}
