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
import com.thoughtworks.binding.XmlExtractor.{PrefixedName, UnprefixedName}
import com.thoughtworks.sde.core.Preprocessor
import macrocompat.bundle
import org.scalajs.dom.raw._

import scala.annotation.{StaticAnnotation, compileTimeOnly, tailrec}
import scala.collection.GenSeq
import scala.reflect.macros.whitebox
import scala.language.experimental.macros
import scalatags.JsDom
import scalatags.jsdom
import org.scalajs.dom.document

import scala.collection.immutable.Queue
import scala.scalajs.runtime.AnonFunction1

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

  private[dom] sealed trait LowPriorityRuntime {
    @inline
    final def notEqual[A, B](left: A, right: B, dummy: Unit = ()) = left != right
  }

  /**
    * Internal helpers for `@dom` annotation
    *
    * @note Do not use methods and classes in this object.
    */
  object Runtime extends LowPriorityRuntime {

    final class CurrentTargetReference[A](val value: A) extends AnyVal

    final class NodeSeqMountPoint(parent: Node, childrenBinding: BindingSeq[Node])
      extends MultiMountPoint[Node](childrenBinding) {

      @inline
      def this(parent: Node, childBinding: Binding[BindingSeq[Node]], dummy: Unit = ()) = {
        this(parent, Constants(()).flatMapBinding { _ => childBinding })
      }

      @inline
      def this(parent: Node, childBinding: Binding[Node]) = {
        this(parent, Constants(()).mapBinding { _ => childBinding })
      }

      @inline
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

    @inline
    def domBindingSeq(bindingSeq: BindingSeq[Node]) = bindingSeq

    @inline
    def domBindingSeq(seq: Seq[Node]) = Constants(seq: _*)

    @inline
    def domBindingSeq(node: Node) = Constants(node)

    @inline
    def domBindingSeq(text: String) = Constants(document.createTextNode(text))

    @inline
    def notEqual[A](left: A, right: A) = left != right
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
                q"""_root_.com.thoughtworks.binding.dom.Runtime.domBindingSeq($transformedChild)"""
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
              }) (collection.breakOut(Queue.canBuildFrom))
              val (valDefs, transformedChildren) = transformedPairs.unzip
              valDefs.flatten -> q"""_root_.com.thoughtworks.binding.Binding.Constants(..$transformedChildren).flatMapBinding(_root_.scala.Predef.locally _)"""
          }
        }

        private def transformedWithValDefs: PartialFunction[Tree, (Queue[ValDef], Tree)] = {
          case tree@NodeBuffer(children@_*) =>
            nodeSeq(children)
          case tree@Elem(UnprefixedName(label), attributes, _, children) =>
            val idOption = attributes.collectFirst { case (UnprefixedName("id"), Text(id)) => id }
            val elementName = idOption match {
              case None => TermName(c.freshName("element"))
              case Some(id) => TermName(id).encodedName.toTermName
            }

            val attributeMountPoints = for {
              (key, value) <- attributes
            } yield {
              val attributeAccess = key match {
                case UnprefixedName(localPart) =>
                  val keyName = TermName(localPart)
                  q"""$elementName.$keyName"""
                case PrefixedName(prefix, localPart) =>
                  localPart.split(':').foldLeft(q"""$elementName.${TermName(prefix)}""") { (prefixExpr, propertyName) =>
                    q"""$prefixExpr.${TermName(propertyName)}"""
                  }
              }

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
                          implicit def ${TermName(c.freshName("currentTargetReference"))} =
                            new _root_.com.thoughtworks.binding.dom.Runtime.CurrentTargetReference($elementName)
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
            val elementDef = q"val $elementName = _root_.com.thoughtworks.binding.dom.Runtime.TagsAndTags2.${TermName(label)}.render"
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

        private def transformed: PartialFunction[Tree, Tree] = {
          case Block(stats, expr) =>
            super.transform(Block(stats.flatMap {
              case transformedWithValDefs.extract((valDefs, transformedTree)) =>
                valDefs.enqueue(transformedTree)
              case stat =>
                Seq(stat)
            }, expr))
          case tree@EntityRef(EntityName(unescapedCharacter)) =>
            atPos(tree.pos) {
              q"""$unescapedCharacter"""
            }
          case tree@Comment(value) =>
            atPos(tree.pos) {
              q"""_root_.org.scalajs.dom.document.createComment($value)"""
            }
          case tree@Text(value) =>
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

      replaceDefBody(annottees, { body =>
        q"""_root_.com.thoughtworks.binding.Binding.apply{
          import _root_.com.thoughtworks.binding.dom.AutoImports._
          ${transform(body)}
        }"""
      })
    }

  }

}
