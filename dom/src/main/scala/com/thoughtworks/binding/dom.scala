/*
The MIT License (MIT)

Copyright (c) 2016 Yang Bo & REA Group Ltd.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
 */

package com.thoughtworks.binding

import Binding.{BindingSeq, Constants, MultiMountPoint, SingleMountPoint, SingletonBindingSeq}
import dom.Runtime.{NodeMountPoint, NodeSeqMountPoint}
import com.thoughtworks.Extractor._
import com.thoughtworks.binding.XmlExtractor.{PrefixedName, QName, UnprefixedName}
import com.thoughtworks.sde.core.Preprocessor
import macrocompat.bundle
import org.scalajs.dom.raw._

import scala.annotation.{StaticAnnotation, compileTimeOnly, implicitNotFound, tailrec}
import scala.collection.GenSeq
import scala.reflect.macros.whitebox
import scala.language.experimental.macros
import scalatags.JsDom
import scalatags.jsdom
import org.scalajs.dom.document
import scalatags.JsDom.TypedTag
import scalatags.generic.Namespace

import scala.collection.immutable.Queue
import scala.reflect.NameTransformer

/**
  * Enable XML DOM literal for Binding.scala
  *
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
@compileTimeOnly("enable macro paradise to expand macro annotations")
// TODO: @dom will be deprecated once @html is implemented
// @deprecated(message = "Use `@html` instead", since = "11.0.0")
class dom extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro dom.Macros.macroTransform
}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
// TODO: @dom will be deprecated once @html is implemented
// @deprecated(message = "Use `@html` instead", since = "11.0.0")
object dom {

  private[dom] sealed trait LowPriorityMountable2 {
    @inline
    implicit final def mountableBindingBindingSeq[Parent, Child](
        implicit mountableBindingSeq: Mountable[Parent, BindingSeq[Child]]): Mountable[Parent, Child] =
      new Mountable[Parent, Child] {
        def mount(parent: Parent, child: Child): Binding[Unit] = {
          mountableBindingSeq.mount(parent, Constants(child))
        }
      }
  }
  private[dom] sealed trait LowPriorityMountable1 {
    @inline
    implicit final def mountableBindingBindingSeq[Parent, Child](
        implicit mountableBindingSeq: Mountable[Parent, BindingSeq[Child]])
      : Mountable[Parent, Binding[BindingSeq[Child]]] =
      new Mountable[Parent, Binding[BindingSeq[Child]]] {
        def mount(parent: Parent, children: Binding[BindingSeq[Child]]): Binding[Unit] = {
          mountableBindingSeq.mount(parent, Constants(children).flatMapBinding(identity))
        }
      }
  }

  private[dom] sealed trait LowPriorityMountable0 extends LowPriorityMountable1 {
    @inline
    implicit final def mountableBinding[Parent, Child](
        implicit mountableBindingSeq: Mountable[Parent, BindingSeq[Child]]): Mountable[Parent, Binding[Child]] =
      new Mountable[Parent, Binding[Child]] {
        def mount(parent: Parent, child: Binding[Child]): Binding[Unit] = {
          mountableBindingSeq.mount(parent, SingletonBindingSeq(child))
        }
      }
  }

  @implicitNotFound("Don't know how to mount ${Children} into ${Parent}.")
  trait Mountable[-Parent, -Children] {
    def mount(parent: Parent, children: Children): Binding[Unit]
  }

  object Mountable extends LowPriorityMountable0 {
    @inline
    implicit final def mountableBindingSeqNode[Parent <: Node, Child <: Node]: Mountable[Parent, BindingSeq[Child]] =
      new Mountable[Parent, BindingSeq[Child]] {
        def mount(parent: Parent, children: BindingSeq[Child]): NodeSeqMountPoint = {
          new NodeSeqMountPoint(parent, children)
        }
      }

    @inline
    implicit final def mountableBindingBindingSeqNode[Parent <: Node, Child <: Node]
      : Mountable[Parent, Binding[BindingSeq[Child]]] =
      new Mountable[Parent, Binding[BindingSeq[Child]]] {
        def mount(parent: Parent, children: Binding[BindingSeq[Child]]): NodeSeqMountPoint = {
          new NodeSeqMountPoint(parent, children, ())
        }
      }

    @inline
    implicit final def mountableBindingNode[Parent <: Node, Child <: Node]: Mountable[Parent, Binding[Child]] =
      new Mountable[Parent, Binding[Child]] {
        def mount(parent: Parent, child: Binding[Child]): NodeMountPoint = {
          new NodeMountPoint(parent, child)
        }
      }
  }
  private[dom] sealed trait LowPriorityRuntime1 {
    @inline
    final def domBindingSeq[A](x: A) = Constants(x)
  }

  private[dom] sealed trait LowPriorityRuntime extends LowPriorityRuntime1 {

    @inline
    final def domBindingSeq[A](x: Binding[A])(implicit dummy: DummyImplicit): SingletonBindingSeq[A] = {
      SingletonBindingSeq(x)
    }

    @inline
    final def domBindingSeq[A](x: Option[A])(implicit dummy: DummyImplicit): Constants[A] = {
      Constants(x.toSeq: _*)
    }

    @inline
    final def domBindingSeq[A](x: BindingSeq[A])(implicit dummy: DummyImplicit): BindingSeq[A] = x

    @inline
    final def notEqual[A, B](left: A, right: B, dummy: Unit = ()): Boolean = left != right

  }

  @inline
  @tailrec
  private def removeAll(parent: Node): Unit = {
    val firstChild = parent.firstChild
    if (firstChild != null) {
      parent.removeChild(firstChild)
      removeAll(parent)
    }
  }

  /**
    * Internal helpers for `@dom` annotation
    *
    * @note Do not use methods and classes in this object.
    */
  object Runtime extends LowPriorityRuntime {

    @inline
    def mount(parent: Node, children: Binding[Seq[Node]])(implicit dummy0: DummyImplicit,
                                                          dummy1: DummyImplicit,
                                                          dummy2: DummyImplicit) = {
      new NodeSeqMountPoint(parent, Constants(children).flatMap { s =>
        Constants(s.bind: _*)
      })
    }

    @inline
    def mount(parent: Node, childOption: Binding[Option[Node]])(implicit dummy0: DummyImplicit,
                                                                dummy1: DummyImplicit) = {
      new NodeSeqMountPoint(parent, Constants(childOption).flatMap { o =>
        Constants(o.bind.toSeq: _*)
      })
    }

    @inline
    def mount(parent: Node, text: Binding[String])(implicit dummy: DummyImplicit): NodeMountPoint = {
      new NodeMountPoint(parent, Binding { document.createTextNode(text.bind) })
    }

    @inline
    def mount(parent: Node, childrenBinding: BindingSeq[Node]): NodeSeqMountPoint = {
      new NodeSeqMountPoint(parent, childrenBinding)
    }

    @inline
    def mount(parent: Node, childBinding: Binding[BindingSeq[Node]], dummy: Unit = ()): NodeSeqMountPoint = {
      new NodeSeqMountPoint(parent, childBinding, dummy)
    }

    @inline
    def mount(parent: Node, childBinding: Binding[Node]): NodeMountPoint = {
      new NodeMountPoint(parent, childBinding)
    }

    @inline
    def mount[Parent, Children](parent: Parent, children: Children)(
        implicit mountable: Mountable[Parent, Children]): Binding[Unit] = {
      mountable.mount(parent, children)
    }

    final class NodeMountPoint private[dom] (parent: Node, childBinding: Binding[Node])
        extends SingleMountPoint[Node](childBinding) {
      protected def set(child: Node): Unit = {
        removeAll(parent)
        checkedAppendChild(parent, child)
      }
    }

    final class NodeSeqMountPoint(parent: Node, childrenBinding: BindingSeq[Node])
        extends MultiMountPoint[Node](childrenBinding) {

      @inline
      def this(parent: Node, childBinding: Binding[BindingSeq[Node]], dummy: Unit = ()) = {
        this(parent, Constants(childBinding).flatMapBinding(identity))
      }

      @inline
      @deprecated("Use [[NodeMountPoint]] instead", "11.4.0")
      def this(parent: Node, childBinding: Binding[Node]) = {
        this(parent, SingletonBindingSeq(childBinding))
      }

      override protected def set(children: Seq[Node]): Unit = {
        removeAll(parent)
        for (child <- children) {
          checkedAppendChild(parent, child)
        }
      }

      override protected def splice(from: Int, that: GenSeq[Node], replaced: Int): Unit = {
        @inline
        @tailrec
        def removeChildren(child: Node, n: Int): Node = {
          if (n == 0) {
            child
          } else {
            val nextSibling = child.nextSibling
            parent.removeChild(child)
            removeChildren(nextSibling, n - 1)
          }
        }

        val child = removeChildren(parent.childNodes(from), replaced)
        if (child == null) {
          for (newChild <- that) {
            checkedAppendChild(parent, newChild)
          }
        } else {
          for (newChild <- that) {
            if (newChild.parentNode != null) {
              throw new IllegalStateException(raw"""Cannot insert a ${newChild.nodeName} element twice!""")
            }
            parent.insertBefore(newChild, child)
          }
        }
      }

    }

    object TagsAndTags2 extends JsDom.Cap with jsdom.Tags with jsdom.Tags2 {

      import scala.language.dynamics

      final class DynamicDataTag private[TagsAndTags2] ()
          extends TypedTag[HTMLElement]("data", Nil, false, Namespace.htmlNamespaceConfig)
          with Dynamic {
        final def selectDynamic(tagName: String): ConcreteHtmlTag[Element] = {
          TagsAndTags2.tag(tagName)
        }
      }

      override lazy val data = new DynamicDataTag()

    }

    @inline
    def domBindingSeq(bindingSeq: BindingSeq[Node]) = bindingSeq

    @inline
    def domBindingSeq(seq: Seq[Node]) = Constants(seq: _*)

    @inline
    def domBindingSeq(node: Node) = Constants(node)

    @inline
    def domBindingSeq(text: String) = Constants(document.createTextNode(text))

    @inline
    def domBindingSeq(optionNode: Option[Node]) = Constants(optionNode.toSeq: _*)

    @inline
    def notEqual[A](left: A, right: A) = left != right
  }

  private def checkedAppendChild(parent: Node, child: Node): Unit = {
    if (child.parentNode != null) {
      throw new IllegalStateException(raw"""Cannot insert ${child.nodeName} twice!""")
    }
    parent.appendChild(child)
  }

  /**
    * This object contains implicit views imported automatically for @dom methods.
    */
  object AutoImports {

    implicit final class DataOps @inline()(node: Element) {

      import scala.language.dynamics

      object data extends Dynamic {

        final def selectDynamic(attributeName: String): String = {
          node.getAttribute(attributeName)
        }

        final def updateDynamic(attributeName: String)(attributeValue: String): Unit = {
          node.setAttribute(attributeName, attributeValue)
        }

      }

    }

    implicit final class OptionOps @inline()(node: Element) {

      import scala.language.dynamics

      object option extends Dynamic {

        final def selectDynamic(attributeName: String): Option[String] = {
          if (node.hasAttribute(attributeName)) {
            Some(node.getAttribute(attributeName))
          } else {
            None
          }
        }

        final def updateDynamic(attributeName: String)(attributeValue: Option[String]): Unit = {
          attributeValue.fold(node.removeAttribute(attributeName))(node.setAttribute(attributeName, _))
        }

      }

    }

    final class StyleOps @inline @deprecated("Use [[org.scalajs.dom.raw.HTMLElement.style]] instead", "11.2.0")(
        node: HTMLElement) {
      @deprecated("Use [[org.scalajs.dom.raw.HTMLElement.style]] instead", "11.2.0")
      @inline def style = node.style.cssText

      @deprecated("Use [[org.scalajs.dom.raw.HTMLElement.style]] instead", "11.2.0")
      @inline def style_=(value: String) = node.style.cssText = value
    }

    @deprecated("Use [[org.scalajs.dom.raw.HTMLElement.style]] instead", "11.2.0")
    def StyleOps(node: HTMLElement) = new StyleOps(node)

    implicit final class ClassOps @inline()(node: HTMLElement) {
      @inline def `class` = node.className

      @inline def class_=(value: String) = node.className = value
    }

    implicit final class ForOps @inline()(node: HTMLLabelElement) {
      @inline def `for` = node.htmlFor

      @inline def for_=(value: String) = node.htmlFor = value
    }

    /**
      * @param node HTMLDialogElement or HTMLDetailElement
      */
    implicit final class OpenOps @inline private[dom] (node: Element { var open: Boolean }) {
      @inline def open = node.getAttribute("open")
      @inline def open_=(value: String) = node.setAttribute("open", value)
    }

    implicit final class MultipleOps @inline()(node: Element { var multiple: Boolean }) {
      @inline def multiple = node.getAttribute("multiple")
      @inline def multiple_=(value: String) = node.setAttribute("multiple", value)
    }

    implicit final class FormNoValidateOps @inline()(node: Element { var formNoValidate: Boolean }) {
      @inline def formNoValidate = node.getAttribute("formNoValidate")
      @inline def formNoValidate_=(value: String) = node.setAttribute("formNoValidate", value)
    }

    implicit final class NoValidateOps @inline()(node: HTMLFormElement) {
      @inline def noValidate = node.getAttribute("noValidate")
      @inline def noValidate_=(value: String) = node.setAttribute("noValidate", value)
    }

    implicit final class ControlsOps @inline()(node: HTMLMediaElement) {
      @inline def controls = node.getAttribute("controls")
      @inline def controls_=(value: String) = node.setAttribute("controls", value)
    }

    implicit final class LoopOps @inline()(node: HTMLMediaElement) {
      @inline def loop = node.getAttribute("loop")
      @inline def loop_=(value: String) = node.setAttribute("loop", value)
    }

    implicit final class SelectedOps @inline()(node: Element { var selected: Boolean }) {
      @inline def selected = node.getAttribute("selected")
      @inline def selected_=(value: String) = node.setAttribute("selected", value)
    }

    implicit final class MutedOps @inline()(node: HTMLMediaElement) {
      @inline def muted = node.getAttribute("muted")
      @inline def muted_=(value: String) = node.setAttribute("muted", value)
    }

    implicit final class SpellcheckOps @inline()(node: HTMLElement) {
      @inline def spellcheck = node.getAttribute("spellcheck")
      @inline def spellcheck_=(value: String) = node.setAttribute("spellcheck", value)
    }

    implicit final class DraggableOps @inline()(node: HTMLElement) {
      @inline def draggable = node.getAttribute("draggable")
      @inline def draggable_=(value: String) = node.setAttribute("draggable", value)
    }

    implicit final class AutoplayOps @inline()(node: HTMLMediaElement) {
      @inline def autoplay = node.getAttribute("autoplay")
      @inline def autoplay_=(value: String) = node.setAttribute("autoplay", value)
    }

    implicit final class RequiredOps @inline()(node: Element { var required: Boolean }) {
      @inline def required = node.getAttribute("required")
      @inline def required_=(value: String) = node.setAttribute("required", value)
    }

    implicit final class AutofocusOps @inline()(node: Element { var autofocus: Boolean }) {
      @inline def autofocus = node.getAttribute("autofocus")
      @inline def autofocus_=(value: String) = node.setAttribute("autofocus", value)
    }

    implicit final class CheckedOps @inline()(node: Element { var checked: Boolean }) {
      @inline def checked = node.getAttribute("checked")
      @inline def checked_=(value: String) = node.setAttribute("checked", value)
    }

    implicit final class DisabledOps @inline()(node: Element { var disabled: Boolean }) {
      @inline def disabled = node.getAttribute("disabled")
      @inline def disabled_=(value: String) = node.setAttribute("disabled", value)
    }

    implicit final class ReadOnlyOps @inline()(node: Element { var readOnly: Boolean }) {
      @inline def readOnly = node.getAttribute("readOnly")
      @inline def readOnly_=(value: String) = node.setAttribute("readOnly", value)
    }
    implicit final class DefaultOps @inline()(node: HTMLTrackElement) {
      @inline def default = node.getAttribute("default")
      @inline def default_=(value: String) = node.setAttribute("default", value)
    }

    implicit final class PlaysInlineOps @inline()(node: HTMLVideoElement) {
      @inline def playsInline = node.getAttribute("playsInline")
      @inline def playsInline_=(value: String) = node.setAttribute("playsInline", value)
    }

    implicit final class TypeMustMatchOps @inline()(node: HTMLObjectElement) {
      @inline def typeMustMatch = node.getAttribute("typeMustMatch")
      @inline def typeMustMatch_=(value: String) = node.setAttribute("typeMustMatch", value)
    }

    implicit final class TranslateOps @inline()(node: HTMLElement) {
      @inline def translate = node.getAttribute("translate")
      @inline def translate_=(value: String) = node.setAttribute("translate", value)
    }

    implicit final class HiddenOps @inline()(node: HTMLElement) {
      @inline def hidden = node.getAttribute("hidden")
      @inline def hidden_=(value: String) = node.setAttribute("hidden", value)
    }

    implicit final class ReversedOps @inline()(node: HTMLOListElement) {
      @inline def reversed = node.getAttribute("reversed")
      @inline def reversed_=(value: String) = node.setAttribute("reversed", value)
    }

    implicit final class IsMapOps @inline()(node: HTMLImageElement) {
      @inline def isMap = node.getAttribute("isMap")
      @inline def isMap_=(value: String) = node.setAttribute("isMap", value)
    }

    implicit final class AllowFullscreenOps @inline()(node: HTMLIFrameElement) {
      @inline def allowFullscreen = node.getAttribute("allowFullscreen")
      @inline def allowFullscreen_=(value: String) = node.setAttribute("allowFullscreen", value)
    }

    implicit final class AllowPaymentRequestOps @inline()(node: HTMLIFrameElement) {
      @inline def allowPaymentRequest = node.getAttribute("allowPaymentRequest")
      @inline def allowPaymentRequest_=(value: String) = node.setAttribute("allowPaymentRequest", value)
    }

    implicit final class AllowUserMediaOps @inline()(node: HTMLIFrameElement) {
      @inline def allowUserMedia = node.getAttribute("allowUserMedia")
      @inline def allowUserMedia_=(value: String) = node.setAttribute("allowUserMedia", value)
    }
    implicit final class NoShadeOps @inline()(node: HTMLHRElement) {
      @inline def noShade = node.getAttribute("noShade")
      @inline def noShade_=(value: String) = node.setAttribute("noShade", value)
    }
    implicit final class NoWrapOps @inline()(node: HTMLTableCellElement) {
      @inline def noWrap = node.getAttribute("noWrap")
      @inline def noWrap_=(value: String) = node.setAttribute("noWrap", value)
    }
    implicit final class DeclareOps @inline()(node: HTMLObjectElement) {
      @inline def declare = node.getAttribute("declare")
      @inline def declare_=(value: String) = node.setAttribute("declare", value)
    }

    implicit final class TrueSpeedOps @inline()(node: HTMLMarqueeElement) {
      @inline def trueSpeed = node.getAttribute("trueSpeed")
      @inline def trueSpeed_=(value: String) = node.setAttribute("trueSpeed", value)
    }

    implicit final class NoResizeOps @inline()(node: HTMLFrameElement) {
      @inline def noResize = node.getAttribute("noResize")
      @inline def noResize_=(value: String) = node.setAttribute("noResize", value)
    }

    implicit final class NoHrefOps @inline()(node: HTMLAreaElement) {
      @inline def noHref = node.getAttribute("noHref")
      @inline def noHref_=(value: String) = node.setAttribute("noHref", value)
    }

    implicit final class CompactOps @inline()(node: Element { var compact: Boolean }) {
      @inline def compact = node.getAttribute("compact")
      @inline def compact_=(value: String) = node.setAttribute("compact", value)
    }
    implicit final class AsyncOps @inline()(node: HTMLScriptElement) {
      @inline def async = node.getAttribute("async")
      @inline def async_=(value: String) = node.setAttribute("async", value)
    }

    implicit final class DeferOps @inline()(node: HTMLScriptElement) {
      @inline def defer = node.getAttribute("defer")
      @inline def defer_=(value: String) = node.setAttribute("defer", value)
    }

    implicit final class NoModuleOps @inline()(node: HTMLScriptElement) {
      @inline def noModule = node.getAttribute("noModule")
      @inline def noModule_=(value: String) = node.setAttribute("noModule", value)
    }

    @inline def workaroundUnusedImport() = ()
  }

  /**
    * Render a binding node into `parent`
    */
  @inline
  def render(parent: Node, child: Binding[Node]): Unit = {
    Runtime.mount(parent, child).watch()
  }

  /**
    * Render a binding sequence of node into `parent`
    */
  @inline
  def render(parent: Node, children: BindingSeq[Node]): Unit = {
    Runtime.mount(parent, children).watch()
  }

  /**
    * Render a binding sequence of node into `parent`
    *
    * @usecase def render(parent: Node, children: Binding[BindingSeq[Node]]): Unit = ???
    **/
  @inline
  def render(parent: Node, children: Binding[BindingSeq[Node]], dummy: Unit = ()): Unit = {
    Runtime.mount(parent, children).watch()
  }

  @bundle
  private[dom] final class Macros(context: whitebox.Context) extends Preprocessor(context) with XmlExtractor {

    import c.universe._

    def macroTransform(annottees: Tree*): Tree = {
      val transformer = new ComprehensionTransformer {

        private def transformXml(tree: Tree): (Queue[ValDef], Tree) = {
          tree match {
            case transformedWithValDefs.extract(queue, tree) =>
              (queue, tree)
            case transformed.extract(transformedTree) =>
              Queue.empty -> transformedTree
            case _ =>
              Queue.empty -> super.transform(tree)
          }
        }

        private def nodeSeq(children: Seq[Tree]): (Queue[ValDef], Tree) = {
          children match {
            case Seq() =>
              Queue.empty -> q"""_root_.com.thoughtworks.binding.Binding.Constants.empty"""
            case Seq(child) =>
              val (valDefs, transformedChild) = transformXml(child)
              valDefs -> atPos(child.pos) {
                q"""_root_.com.thoughtworks.binding.Binding.apply($transformedChild)"""
              }
            case _ =>
              val transformedPairs = (for {
                child <- children
              } yield {
                val (valDefs, transformedChild) = transformXml(child)
                valDefs -> atPos(child.pos) {
                  q"""
                    _root_.com.thoughtworks.binding.Binding.apply {
                      _root_.com.thoughtworks.binding.dom.Runtime.domBindingSeq($transformedChild)
                    }
                  """
                }
              })(collection.breakOut(Queue.canBuildFrom))
              val (valDefs, transformedChildren) = transformedPairs.unzip
              valDefs.flatten -> q"""_root_.com.thoughtworks.binding.Binding.Constants(..$transformedChildren).flatMapBinding(_root_.scala.Predef.locally _)"""
          }
        }

        private def transformedWithValDefs: PartialFunction[Tree, (Queue[ValDef], Tree)] = {
          case tree @ NodeBuffer(children) =>
            nodeSeq(children)
          case tree @ Elem(tag, attributes, _, children) =>
            val idOption = findTextAttribute("local-id", attributes).orElse(findTextAttribute("id", attributes))
            val elementName = idOption match {
              case None     => TermName(c.freshName("htmlElement"))
              case Some(id) => TermName(NameTransformer.encode(id))
            }

            val attributeMountPoints = for {
              (key, value) <- attributes if {
                key match {
                  case UnprefixedName("local-id") => false
                  case _                          => true
                }
              }
            } yield {
              val attributeAccess = propertyAccess(key, q"$elementName")

              atPos(value.pos) {
                value match {
                  case EmptyAttribute() =>
                    q"""$attributeAccess = "" """
                  case Text(textLiteral) =>
                    q"$attributeAccess = $textLiteral"
                  case _ =>
                    val assignName = TermName(c.freshName("assignAttribute"))
                    val newValueName = TermName(c.freshName("newValue"))
                    q"""
                      _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[
                        _root_.com.thoughtworks.binding.Binding,
                        _root_.scala.Unit
                      ](
                        _root_.com.thoughtworks.binding.Binding.apply[_root_.scala.Unit]({
                          val $newValueName = ${transform(value)}
                          @_root_.scala.inline def $assignName() = {
                            if (_root_.com.thoughtworks.binding.dom.Runtime.notEqual($attributeAccess, $newValueName)) {
                              $attributeAccess = $newValueName
                            }
                          }
                          $assignName()
                        })
                      )
                    """
                }
              }
            }
            val (valDefs, transformedChild) = children match {
              case Seq() =>
                Queue.empty -> Nil
              case _ =>
                val (valDefs, transformedBuffer) = nodeSeq(children)
                valDefs -> List(atPos(tree.pos) {
                  q"""
                  _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[
                    _root_.com.thoughtworks.binding.Binding,
                    _root_.scala.Unit
                  ](
                    _root_.com.thoughtworks.binding.dom.Runtime.mount(
                      $elementName,
                      $transformedBuffer
                    )
                  )
                  """
                })
            }

            val tagAccess = propertyAccess(tag, q"_root_.com.thoughtworks.binding.dom.Runtime.TagsAndTags2")

            val elementDef = q"val $elementName = $tagAccess.render"
            idOption match {
              case None =>
                valDefs -> q"""
                  $elementDef
                  ..$transformedChild
                  ..$attributeMountPoints
                  $elementName
                """
              case Some(id) =>
                (valDefs.enqueue(elementDef)) -> q"""
                  ..$transformedChild
                  ..$attributeMountPoints
                  $elementName
                """
            }
        }

        private def findTextAttribute(unprefixedName: String,
                                      attributes: Seq[(XmlExtractor.QName, Tree)]): Option[String] = {
          attributes.collectFirst { case (UnprefixedName(`unprefixedName`), Text(text)) => text }
        }

        private def propertyAccess(xmlName: QName, objectAccess: RefTree): Select = {
          xmlName match {
            case UnprefixedName(localPart) =>
              q"$objectAccess.${TermName(NameTransformer.encode(localPart))}"
            case PrefixedName(prefix, localPart) =>
              localPart.split(':').foldLeft(q"$objectAccess.${TermName(NameTransformer.encode(prefix))}") {
                (prefixExpr, segmentName) =>
                  q"$prefixExpr.${TermName(NameTransformer.encode(segmentName))}"
              }
          }
        }

        private def transformed: PartialFunction[Tree, Tree] = {
          case Block(stats, expr) =>
            super.transform(Block(stats.flatMap {
              case transformedWithValDefs.extract((valDefs, transformedTree)) =>
                valDefs.enqueue(transformedTree)
              case stat =>
                Seq(stat)
            }, expr))
          case tree @ EntityRef(HtmlEntityName(unescapedCharacter)) =>
            atPos(tree.pos) {
              q"""$unescapedCharacter"""
            }
          case tree @ Comment(value) =>
            atPos(tree.pos) {
              q"""_root_.org.scalajs.dom.document.createComment($value)"""
            }
          case tree @ Text(value) =>
            atPos(tree.pos) {
              q"$value"
            }
        }

        override def transform(tree: Tree): Tree = {
          tree match {
            case transformedWithValDefs.extract((valDefs, transformedTree)) =>
              q"""
                ..$valDefs
                $transformedTree
              """
            case transformed.extract(transformedTree) =>
              transformedTree
            case _ =>
              super.transform(tree)
          }
        }
      }

      import transformer.transform
      //      def transform(tree: Tree): Tree = {
      //        val output = transformer.transform(tree)
      //        c.info(c.enclosingPosition, show(output), true)
      //        output
      //      }

      def autoImportAndTransform(body: Tree) = {
        q"""_root_.com.thoughtworks.binding.Binding.apply {
          import _root_.com.thoughtworks.binding.dom.AutoImports.{
            != => _,
            ## => _,
            == => _,
            eq => _,
            equals => _,
            getClass => _,
            hashCode => _,
            ne => _,
            notify => _,
            notifyAll => _,
            synchronized => _,
            toString => _,
            wait => _,
            _
          }
          workaroundUnusedImport()
          ${transform(body)}
        }"""
      }
      replaceDefBody(annottees, autoImportAndTransform)
    }

  }

}
