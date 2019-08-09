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

import macrocompat.bundle

import scala.reflect.macros.blackbox
import com.thoughtworks.Extractor._
import com.thoughtworks.binding.XmlExtractor._
import org.apache.commons.lang3.text.translate.EntityArrays._

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
@bundle
trait XmlExtractor {
  val c: blackbox.Context

  import c.universe._

  private def nodeBuffer: PartialFunction[Tree, List[Tree]] = {
    case q"""{
      val $$buf = new _root_.scala.xml.NodeBuffer()
      ..$pushChildrenTree
      $$buf
    }""" =>
      for {
        pushChild <- pushChildrenTree
      } yield {
        val q"$$buf.$$amp$$plus($child)" = pushChild
        child
      }
  }

  protected final val NodeBuffer = nodeBuffer.extract

  private def nodeBufferStar(child: List[Tree]): List[Tree] = {
    child match {
      case Nil =>
        Nil
      case List(q"""${NodeBuffer(children)}: _*""") =>
        children
    }
  }

  private def prefix: PartialFunction[Tree, Option[String]] = {
    case q"null"                      => None
    case Literal(Constant(p: String)) => Some(p)
  }

  private val Prefix = prefix.extract

  private def elem: PartialFunction[Tree, (QName, List[(QName, Tree)], Boolean, List[Tree])] = {
    case Block(Nil,
               q"""
                 {
                   var $$md: _root_.scala.xml.MetaData = _root_.scala.xml.Null;
                   ..$attributes
                   new _root_.scala.xml.Elem(
                     ${Prefix(prefixOption)},
                     ${Literal(Constant(localPart: String))},
                     $$md, $$scope,
                     ${Literal(Constant(minimizeEmpty: Boolean))},
                     ..$child
                   )
                 }
               """) =>
      (QName(prefixOption, localPart), attributes.map {
        case q"""$$md = new _root_.scala.xml.UnprefixedAttribute(${Literal(Constant(key: String))}, $value, $$md)""" =>
          UnprefixedName(key) -> value
        case q"""$$md = new _root_.scala.xml.PrefixedAttribute(${Literal(Constant(pre: String))}, ${Literal(
              Constant(key: String))}, $value, $$md)""" =>
          PrefixedName(pre, key) -> value
      }, minimizeEmpty, nodeBufferStar(child))
    case Block(Nil,
               Block(
                 Nil,
                 q"""
                   new _root_.scala.xml.Elem(
                     ${Prefix(prefixOption)},
                     ${Literal(Constant(localPart: String))},
                     _root_.scala.xml.Null,
                     $$scope,
                     ${Literal(Constant(minimizeEmpty: Boolean))},
                     ..$child
                   )
                 """
               )) =>
      (QName(prefixOption, localPart), Nil, minimizeEmpty, nodeBufferStar(child))
  }

  protected final val Elem = elem.extract

  private def entityRef: PartialFunction[Tree, String] = {
    case q"""new _root_.scala.xml.EntityRef(${Literal(Constant(entityName: String))})""" =>
      entityName
  }

  protected final val EntityRef = entityRef.extract

  private def text: PartialFunction[Tree, String] = {
    case q"""new _root_.scala.xml.Text(${Literal(Constant(data: String))})""" =>
      data
  }

  protected final val Text = text.extract

  private def textAttribute: PartialFunction[Tree, String] = {
    case Text(data)       => data
    case EmptyAttribute() => ""
  }

  @deprecated("Use [[TextAttributes]] instead", "11.9.0")
  protected final val TextAttribute = textAttribute.extract

  private def textAttributes: PartialFunction[Tree, Seq[Tree]] = {
    case text @ (Text(_) | EntityRef(_))                     => Seq(text)
    case EmptyAttribute()                                    => Nil
    case NodeBuffer(texts @ ((Text(_) | EntityRef(_)) +: _)) => texts
  }

  protected final val TextAttributes = textAttributes.extract

  private def comment: PartialFunction[Tree, String] = {
    case q"""new _root_.scala.xml.Comment(${Literal(Constant(commentText: String))})""" =>
      commentText
  }

  protected final val Comment = comment.extract

  private def procInstr: PartialFunction[Tree, (String, String)] = {
    case q"""
      new _root_.scala.xml.ProcInstr(
        ${Literal(Constant(target: String))},
        ${Literal(Constant(proctext: String))}
      )
    """ =>
      (target, proctext)
  }

  protected final val ProcInstr = procInstr.extract

  private def pcData: PartialFunction[Tree, String] = {
    case q"""
      new _root_.scala.xml.PCData(
        ${Literal(Constant(data: String))}
      )
    """ =>
      data
  }

  protected final val PCData = pcData.extract

  protected final val HtmlEntityName = XmlExtractor.HtmlEntityRefMap.extract

  protected final val XmlEntityName = XmlExtractor.XmlEntityRefMap.extract


  protected object EmptyAttribute {
    def unapply(tree: Tree) = {
      val tpe = tree.tpe
      tpe != null && tpe =:= typeOf[Nil.type]
    }
  }

}

object XmlExtractor {

  sealed trait QName

  object QName {
    def apply(prefixOption: Option[String], localPart: String) = {
      prefixOption match {
        case None         => UnprefixedName(localPart)
        case Some(prefix) => PrefixedName(prefix, localPart)
      }
    }
  }

  final case class UnprefixedName(localPart: String) extends QName

  final case class PrefixedName(prefix: String, localPart: String) extends QName

  private val EntityRefRegex = "&(.*);".r

  private val XmlEntityRefMap = (for {
    Array(character, EntityRefRegex(reference)) <- BASIC_ESCAPE.view
  } yield reference -> character).toMap

  private val HtmlEntityRefMap = (for {
    entityArray <- Seq(BASIC_ESCAPE, ISO8859_1_ESCAPE, HTML40_EXTENDED_ESCAPE).view
    Array(character, EntityRefRegex(reference)) <- entityArray.view
  } yield reference -> character).toMap

}
