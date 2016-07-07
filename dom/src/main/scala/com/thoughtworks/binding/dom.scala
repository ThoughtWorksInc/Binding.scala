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

import Binding.{BindingSeq, Constants, MultiMountPoint, SingleMountPoint}
import dom.Runtime.NodeSeqMountPoint
import com.thoughtworks.binding.Binding.BindingSeq
import org.apache.commons.lang3.text.translate.EntityArrays
import org.scalajs.dom.raw._

import scala.annotation.{StaticAnnotation, compileTimeOnly, tailrec}
import scala.collection.GenSeq
import scala.reflect.macros.whitebox
import scala.language.experimental.macros
import scalatags.JsDom
import scalatags.jsdom
import org.scalajs.dom.document

/**
  * Enable XML DOM literal for Binding.scala
  *
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class dom extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro dom.Macros.macroTransform
}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object dom {

  /**
    * Internal helpers for `@dom` annotation
    *
    * @note Do not use methods and classes in this object.
    */
  object Runtime {

    final class CurrentTargetReference[A](val value: A) extends AnyVal

    final class AttributeMountPoint[-Value](valueBinding: Binding[Value])(setter: Value => Unit)
      extends SingleMountPoint[Value](valueBinding) {
      override protected def set(value: Value): Unit = {
        setter(value)
      }
    }

    final class TextMountPoint(parent: Text, childBinding: Binding[String])
      extends SingleMountPoint[String](childBinding) {
      override protected def set(value: String): Unit = {
        parent.textContent = value
      }
    }

    final class NodeSeqMountPoint(parent: Node, childrenBinding: BindingSeq[Node])
      extends MultiMountPoint[Node](childrenBinding) {

      def this(parent: Node, childBinding: Binding[BindingSeq[Node]], dummy: Unit = ()) = {
        this(parent, Constants(()).flatMapBinding { _ => childBinding })
      }

      def this(parent: Node, childBinding: Binding[Node]) = {
        this(parent, Constants(()).mapBinding { _ => childBinding })
      }

      @tailrec
      private def removeAll(): Unit = {
        val firstChild = parent.firstChild
        if (firstChild != null) {
          parent.removeChild(firstChild)
          removeAll()
        }
      }

      override protected def set(children: Seq[Node]): Unit = {
        removeAll()
        for (child <- children) {
          if (child.parentNode != null) {
            throw new IllegalStateException(raw"""Cannot insert ${child.nodeName} twice!""")
          }
          parent.appendChild(child)
        }
      }

      override protected def splice(oldSeq: Seq[Node], from: Int, that: GenSeq[Node], replaced: Int): Unit = {
        val i = oldSeq.iterator.drop(from)
        for (_ <- 0 until replaced) {
          if (i.hasNext) {
            parent.removeChild(i.next())
          } else {
            throw new IllegalArgumentException
          }
        }
        if (i.hasNext) {
          val refChild = i.next()
          for (newChild <- that) {
            if (newChild.parentNode != null) {
              throw new IllegalStateException(raw"""Cannot insert a ${newChild.nodeName} element twice!""")
            }
            parent.insertBefore(newChild, refChild)
          }
        } else {
          for (newChild <- that) {
            if (newChild.parentNode != null) {
              throw new IllegalStateException(raw"""Cannot insert a ${newChild.nodeName} element twice!""")
            }
            parent.appendChild(newChild)
          }
        }
      }

    }

    object TagsAndTags2 extends JsDom.Cap with jsdom.Tags with jsdom.Tags2

    def domBindingSeq(bindingSeq: BindingSeq[Node]) = bindingSeq

    def domBindingSeq(seq: Seq[Node]) = Constants(seq: _*)

    def domBindingSeq(node: Node) = Constants(node)

    def domBindingSeq(text: String) = Constants(document.createTextNode(text))

  }


  /**
    * This object contains implicit views imported automatically for @dom methods.
    */
  object AutoImports {

    implicit final class DataOps @inline() (node: Element) {

      import scala.language.dynamics

      @inline object data extends Dynamic {

        @inline final def selectDynamic(attributeName: String): String = {
          node.getAttribute(attributeName)
        }

        @inline final def updateDynamic(attributeName: String)(attributeValue: String): Unit = {
          node.setAttribute(attributeName, attributeValue)
        }

      }

    }

    implicit final class StyleOps @inline() (node: HTMLElement) {
      @inline def style = node.style.cssText

      @inline def style_=(value: String) = node.style.cssText = value
    }

    implicit final class ClassOps @inline() (node: HTMLElement) {
      @inline def `class` = node.className

      @inline def class_=(value: String) = node.className = value
    }

    implicit final class ForOps @inline() (node: HTMLLabelElement) {
      @inline def `for` = node.htmlFor

      @inline def for_=(value: String) = node.htmlFor = value
    }

  }

  /**
    * Render a binding node into `parent`
    */
  @inline
  def render(parent: Node, child: Binding[Node]): Unit = {
    new NodeSeqMountPoint(parent, child).watch()
  }

  /**
    * Render a binding sequence of node into `parent`
    */
  @inline
  def render(parent: Node, children: BindingSeq[Node]): Unit = {
    new NodeSeqMountPoint(parent, children).watch()
  }

  /**
    * Render a binding sequence of node into `parent`
    * 
    * @usecase def render(parent: Node, children: Binding[BindingSeq[Node]]): Unit = ???
    */
  @inline
  def render(parent: Node, children: Binding[BindingSeq[Node]], dummy: Unit = ()): Unit = {
    new NodeSeqMountPoint(parent, children).watch()
  }

  /**
    * Returns the current element. This method must be called in attribute value expressions.
    *
    * @example {{{<br id={ "This BR element's tagName is:" + dom.currentTarget.tagName } />}}}
    */
  def currentTarget[A](implicit implicitCurrentTarget: Runtime.CurrentTargetReference[A]): A = {
    implicitCurrentTarget.value
  }

  private[binding] object Macros {

    private val EntityRefRegex = "&(.*);".r

    private val EntityRefMap = (for {
      Array(character, EntityRefRegex(reference)) <- EntityArrays.BASIC_ESCAPE.view ++ EntityArrays.ISO8859_1_ESCAPE ++ EntityArrays.HTML40_EXTENDED_ESCAPE
    } yield reference -> character).toMap

    def macroTransform(c: whitebox.Context)(annottees: c.Tree*): c.Tree = {
      import c.universe._
      val transformer = new Transformer {

        private def extractChildren: PartialFunction[List[Tree], Tree] = {
          case Typed(expr, Ident(typeNames.WILDCARD_STAR)) :: Nil => expr
        }

        override def transform(tree: Tree): Tree = {
          tree match {
            case q"""{
              val $$buf = new _root_.scala.xml.NodeBuffer()
              ..$pushChildrenTree
              $$buf
            }""" =>
              atPos(tree.pos) {
                q"""
                  _root_.com.thoughtworks.binding.Binding.Constants(
                    ..${
                  for {
                    pushChild <- pushChildrenTree
                  } yield {
                    val q"$$buf.$$amp$$plus($child)" = pushChild
                    atPos(child.pos) {
                      q"""
                            _root_.com.thoughtworks.each.Monadic.monadic[_root_.com.thoughtworks.binding.Binding] {
                              _root_.com.thoughtworks.binding.dom.Runtime.domBindingSeq(${transform(child)})
                            }
                          """
                    }
                  }
                }
                  ).flatMapBinding(_root_.scala.Predef.locally _)
                """
              }
            case q"""
              {
                var $$md: _root_.scala.xml.MetaData = _root_.scala.xml.Null;
                ..$attributes
                new _root_.scala.xml.Elem(null, ${Literal(Constant(label: String))}, $$md, $$scope, $_, ..$child)
              }
            """ =>
              val elementName = TermName(c.freshName("element"))
              val labelName = TermName(label)
              val attributeMountPoints = for {
                attribute <- attributes
              } yield {
                val (attributeAccess, value) = attribute match {
                  case q"""$$md = new _root_.scala.xml.UnprefixedAttribute(${Literal(Constant(key: String))}, $value, $$md)""" =>
                    val keyName = TermName(key)
                    q"""$elementName.$keyName""" -> value
                  case q"""$$md = new _root_.scala.xml.PrefixedAttribute(${Literal(Constant(pre: String))}, ${Literal(Constant(key: String))}, $value, $$md)""" =>
                    key.split(':').foldLeft(q"""$elementName.${TermName(pre)}""") { (prefixExpr, propertyName) =>
                      q"""$prefixExpr.${TermName(propertyName)}"""
                    } -> value
                }
                atPos(attribute.pos) {
                  q"""
                    new _root_.com.thoughtworks.binding.dom.Runtime.AttributeMountPoint({
                      implicit def ${TermName(c.freshName("currentTargetReference"))} =
                        new _root_.com.thoughtworks.binding.dom.Runtime.CurrentTargetReference($elementName)
                      _root_.com.thoughtworks.each.Monadic.monadic[_root_.com.thoughtworks.binding.Binding](${transform(value)})
                    })( value => $attributeAccess = value ).each
                  """
                }
              }
              atPos(tree.pos) {
                q"""
                  {
                    val $elementName = _root_.com.thoughtworks.binding.dom.Runtime.TagsAndTags2.$labelName().render
                    ..${
                  child match {
                    case Seq() =>
                      Nil
                    case Seq(q"""$nodeBuffer: _*""") =>
                      List(atPos(nodeBuffer.pos) {
                        q"""new _root_.com.thoughtworks.binding.dom.Runtime.NodeSeqMountPoint(
                              $elementName,
                              {
                                implicit def ${TermName(c.freshName("currentTargetReference"))} =
                                  new _root_.com.thoughtworks.binding.dom.Runtime.CurrentTargetReference($elementName)
                                ${transform(nodeBuffer)}
                              }
                            ).each"""
                      })
                  }
                }
                    ..$attributeMountPoints
                    $elementName
                  }
                """
              }
            case q"new _root_.scala.xml.Elem(null, ${Literal(Constant(label: String))}, _root_.scala.xml.Null, $$scope, $_, ..$child)" =>
              val elementName = TermName(c.freshName("element"))
              val labelName = TermName(label)
              atPos(tree.pos) {
                q"""
                  {
                    val $elementName = _root_.com.thoughtworks.binding.dom.Runtime.TagsAndTags2.$labelName().render
                    ..${
                  child match {
                    case Seq() =>
                      Nil
                    case Seq(q"""$nodeBuffer: _*""") =>
                      List(atPos(nodeBuffer.pos) {
                        q"""new _root_.com.thoughtworks.binding.dom.Runtime.NodeSeqMountPoint(
                          $elementName,
                          {
                            implicit def ${TermName(c.freshName("currentTargetReference"))} =
                              new _root_.com.thoughtworks.binding.dom.Runtime.CurrentTargetReference($elementName)
                            ${transform(nodeBuffer)}
                          }
                        ).each"""
                      })
                  }
                }
                    $elementName
                  }
                """
              }
            case q"""new _root_.scala.xml.EntityRef(${Literal(Constant(reference: String))})""" =>
              EntityRefMap.get(reference) match {
                case Some(unescapedCharacter) =>
                  atPos(tree.pos) {
                    q"""$unescapedCharacter"""
                  }
                case None =>
                  c.error(tree.pos, s"Unknown HTML entity reference: $reference")
                  q"""???"""
              }
            case q"""new _root_.scala.xml.Comment($text)""" =>
              atPos(tree.pos) {
                q"""_root_.org.scalajs.dom.document.createComment(${transform(text)})"""
              }
            case q"""new _root_.scala.xml.Text($text)""" =>
              atPos(tree.pos) {
                transform(text)
              }
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

      annottees match {
        case Seq(annottee@DefDef(mods, name, tparams, vparamss, tpt, rhs)) =>
          atPos(annottee.pos) {
            DefDef(
              mods, name, tparams, vparamss, tpt,
              q"""_root_.com.thoughtworks.each.Monadic.monadic[
                _root_.com.thoughtworks.binding.Binding
              ]{
                import _root_.com.thoughtworks.binding.dom.AutoImports._
                ${transform(rhs)}
              }"""
            )
          }
        case Seq(annottee@ValDef(mods, name, tpt, rhs)) =>
          atPos(annottee.pos) {
            ValDef(
              mods, name, tpt,
              q"""_root_.com.thoughtworks.each.Monadic.monadic[
                _root_.com.thoughtworks.binding.Binding
              ]{
                import _root_.com.thoughtworks.binding.dom.AutoImports._
                ${transform(rhs)}
              }"""
            )
          }
        case _ =>
          c.error(c.enclosingPosition, "Expect def or val")
          annottees.head
      }
    }

  }

}
