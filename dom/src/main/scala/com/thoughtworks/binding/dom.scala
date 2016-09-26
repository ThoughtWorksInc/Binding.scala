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
import com.thoughtworks.Extractor._
import com.thoughtworks.sde.core.Preprocessor
import macrocompat.bundle
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
            if (newChild.parentNode != null) {
              throw new IllegalStateException(raw"""Cannot insert a ${newChild.nodeName} element twice!""")
            }
            parent.appendChild(newChild)
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

    implicit final class DataOps @inline()(node: Element) {

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

    implicit final class StyleOps @inline()(node: HTMLElement) {
      @inline def style = node.style.cssText

      @inline def style_=(value: String) = node.style.cssText = value
    }

    implicit final class ClassOps @inline()(node: HTMLElement) {
      @inline def `class` = node.className

      @inline def class_=(value: String) = node.className = value
    }

    implicit final class ForOps @inline()(node: HTMLLabelElement) {
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
    **/
  @inline
  def render(parent: Node, children: Binding[BindingSeq[Node]], dummy: Unit = ()): Unit = {
    new NodeSeqMountPoint(parent, children).watch()
  }

  /**
    * Returns the current element. This method must be called in attribute value expressions.
    *
    * @example {{{<br id={ "This BR element's tagName is:" + dom.currentTarget.tagName } />}}}
    */
  @deprecated(message = "Use id attribute instead", since = "10.0.0")
  def currentTarget[A](implicit implicitCurrentTarget: Runtime.CurrentTargetReference[A]): A = {
    implicitCurrentTarget.value
  }

  private object Macros {

    private val EntityRefRegex = "&(.*);".r

    private val EntityRefMap = (for {
      Array(character, EntityRefRegex(reference)) <- EntityArrays.BASIC_ESCAPE.view ++ EntityArrays.ISO8859_1_ESCAPE ++ EntityArrays.HTML40_EXTENDED_ESCAPE
    } yield reference -> character).toMap

  }

  @bundle
  private[binding] final class Macros(context: whitebox.Context) extends Preprocessor(context) with XmlExtractor {

    import Macros._
    import c.universe._

    def macroTransform(annottees: Tree*): Tree = {
      val transformer = new ComprehensionTransformer {

        type TagName = String

        private def transformXml(tree: Tree): (Map[TermName, TagName], Tree) = {
          tree match {
            case partialTransformXml.extract(transformedPair) =>
              transformedPair
            case _ =>
              Map.empty -> super.transform(tree)
          }
        }

        private def partialTransformXml: PartialFunction[Tree, (Map[TermName, TagName], Tree)] = {
          case tree@NodeBuffer(children@_*) =>
            val transformedPairs = for {
              child <- children
            } yield {
              val (definitions, transformedChild) = transformXml(child)
              definitions -> atPos(child.pos) {
                q"""
                  _root_.com.thoughtworks.binding.Binding.apply {
                    _root_.com.thoughtworks.binding.dom.Runtime.domBindingSeq($transformedChild)
                  }
                """
              }
            }
            val (definitions, transformedChildren) = transformedPairs.unzip
            definitions.reduce(_ ++ _) -> atPos(tree.pos) {
              q"""_root_.com.thoughtworks.binding.Binding.Constants(..$transformedChildren).flatMapBinding(_root_.scala.Predef.locally _)"""
            }
          case tree@Elem(label, attributes, _, child) =>
            val idOption = attributes.collectFirst {
              case Left(("id", Text(id))) =>
                id
            }
            val elementName = idOption match {
              case None => TermName(c.freshName("element"))
              case Some(id) => TermName(id)
            }
            val labelName = TermName(label)

            val attributeMountPoints = for {
              attribute <- attributes
            } yield {
              val (attributeAccess, value) = attribute match {
                case Left((key, value)) =>
                  val keyName = TermName(key)
                  q"""$elementName.$keyName""" -> value
                case Right((pre, key, value)) =>
                  key.split(':').foldLeft(q"""$elementName.${TermName(pre)}""") { (prefixExpr, propertyName) =>
                    q"""$prefixExpr.${TermName(propertyName)}"""
                  } -> value
              }
              atPos(value.pos) {
                q"""
                  _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[
                    _root_.com.thoughtworks.binding.Binding,
                    _root_.scala.Unit
                  ](
                    new _root_.com.thoughtworks.binding.dom.Runtime.AttributeMountPoint({
                      implicit def ${TermName(c.freshName("currentTargetReference"))} =
                        new _root_.com.thoughtworks.binding.dom.Runtime.CurrentTargetReference($elementName)
                      _root_.com.thoughtworks.binding.Binding.apply(${transform(value)})
                    })({ attributeValue => if ($attributeAccess != attributeValue) $attributeAccess = attributeValue })
                  )
                """
              }
            }
            val (definitions, transformedChild) = child match {
              case Seq() =>
                Map.empty[TermName, TagName] -> Nil
              case Seq(q"""$nodeBuffer: _*""") =>
                val (definitions, transformedBuffer) = transformXml(nodeBuffer)
                definitions -> List(atPos(nodeBuffer.pos) {
                  q"""
                  _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[
                    _root_.com.thoughtworks.binding.Binding,
                    _root_.scala.Unit
                  ](
                    new _root_.com.thoughtworks.binding.dom.Runtime.NodeSeqMountPoint(
                      $elementName,
                      {
                        implicit def ${TermName(c.freshName("currentTargetReference"))} =
                          new _root_.com.thoughtworks.binding.dom.Runtime.CurrentTargetReference($elementName)
                        $transformedBuffer
                      }
                    )
                  )
                  """
                })
            }
            idOption match {
              case None =>
                definitions -> q"""
                  val $elementName = _root_.com.thoughtworks.binding.dom.Runtime.TagsAndTags2.$labelName().render
                  ..$transformedChild
                  ..$attributeMountPoints
                  $elementName
                """
              case Some(id) =>
                (definitions + (elementName -> label)) -> q"""
                  ..$transformedChild
                  ..$attributeMountPoints
                  $elementName
                """
            }
          case tree@EntityRef(reference)=>
            EntityRefMap.get(reference) match {
              case Some(unescapedCharacter) =>
                Map.empty -> atPos(tree.pos) {
                  q"""$unescapedCharacter"""
                }
              case None =>
                c.error(tree.pos, s"Unknown HTML entity reference: $reference")
                Map.empty -> q"""???"""
            }
          case tree@Comment(value) =>
            Map.empty -> atPos(tree.pos) {
              q"""_root_.org.scalajs.dom.document.createComment(${value})"""
            }
          case tree@Text(value) =>
            Map.empty -> atPos(tree.pos) {
              q"$value"
            }
        }

        override def transform(tree: Tree): Tree = {
          tree match {
            case partialTransformXml.extract((definitions, transformedTree)) =>
              q"""
                ..${
                  for {
                    (termName, tagName) <- definitions
                  } yield {
                    val methodName = TermName(tagName)
                    q"val $termName = _root_.com.thoughtworks.binding.dom.Runtime.TagsAndTags2.$methodName().render"
                  }
                }
                $transformedTree
              """
            case Block(stats, expr) =>
              super.transform(Block(stats.flatMap {
                case partialTransformXml.extract((definitions, transformedTree)) =>
                  ((for {
                    (termName, tagName) <- definitions
                  } yield {
                    val methodName = TermName(tagName)
                    q"val $termName = _root_.com.thoughtworks.binding.dom.Runtime.TagsAndTags2.$methodName().render"
                  }))(collection.breakOut(Vector.canBuildFrom)) :+ transformedTree
                case stat =>
                  Seq(stat)
              }, expr))
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

      replaceDefBody(annottees, { body =>
        q"""_root_.com.thoughtworks.binding.Binding.apply{
          import _root_.com.thoughtworks.binding.dom.AutoImports._
          ${transform(body)}
        }"""
      })
    }

  }

}
