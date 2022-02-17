package com.thoughtworks.binding.html

import net.sourceforge.htmlunit.cyberneko.HTMLConfiguration
import net.sourceforge.htmlunit.cyberneko.HTMLEventInfo
import net.sourceforge.htmlunit.cyberneko.parsers.DOMFragmentParser
import org.apache.xerces.util.XMLAttributesImpl
import org.apache.xerces.xni.Augmentations
import org.apache.xerces.xni.QName
import org.apache.xerces.xni.XMLAttributes
import org.apache.xerces.xni.XMLString
import org.apache.xerces.xni.parser.XMLInputSource
import org.w3c.dom.Node
import org.xml.sax.InputSource

import java.io.StringReader
import scala.collection.Searching
import scala.collection.immutable.BitSet

object InterpolationParser:
  private val AUGMENTATIONS = "http://cyberneko.org/html/features/augmentations"
  private val SYNTHESIZED_ITEM = HTMLEventInfo.SynthesizedItem()
  private val Placeholder = "\"\""
  val ElementArgumentUserDataKey = "Binding.scala element argument"
  val AttributeArgumentsUserDataKey = "Binding.scala attribute arguments"

  private def consumedArgumentIndices(node: Node): BitSet =
    val nodeList = node.getChildNodes

    (0 until nodeList.getLength).view
      .flatMap(i => consumedArgumentIndices(nodeList.item(i)))
      .to(BitSet)
      ++ (node.getUserData(ElementArgumentUserDataKey) match
        case null =>
          BitSet.empty
        case arg: Int =>
          BitSet(arg)
      ) ++ (node.getUserData(AttributeArgumentsUserDataKey) match
        case null =>
          BitSet.empty
        case args: Map[_, Int @unchecked] =>
          args.values.to(BitSet)
      )

  def parseHtmlParts(
      parts: IndexedSeq[String],
      argumentErrorHandler: (message: String, argumentIndex: Int) => Unit
  ) =
    val parser = new InterpolationParser(parts, argumentErrorHandler)
    val document = org.apache.html.dom.HTMLDocumentImpl()
    val fragment = document.createDocumentFragment()
    val html = parts.mkString(Placeholder)
    parser.parse(InputSource(StringReader(html)), fragment)
    val isConsumed = this.consumedArgumentIndices(fragment)
    for
      argIndex <- parts.indices.dropRight(1)
      if !isConsumed(argIndex)
    do
      argumentErrorHandler(
        "A variable must be either an attribute value or child nodes under an element",
        argIndex
      )
    end for
    fragment
  end parseHtmlParts
end InterpolationParser

private class InterpolationParser(
    parts: IndexedSeq[String],
    argumentErrorHandler: (message: String, argumentIndex: Int) => Unit
) extends DOMFragmentParser:
  import InterpolationParser.*
  private val html = parts.mkString(Placeholder)

  private val partOffsets = parts.view
    .flatMap { part =>
      Seq(part.length, Placeholder.length)
    }
    .scanLeft(0)(_ + _)
    .dropRight(1)
    .toIndexedSeq

  fParserConfiguration
    .asInstanceOf[HTMLConfiguration]
    .setFeature(AUGMENTATIONS, true);
  override def startElement(
      element: QName,
      attrs: XMLAttributes,
      augs: Augmentations
  ): Unit =
    val dynamicAttributeIndices =
      (0 until attrs.getLength).view
        .collect(Function.unlift { i =>
          val htmlEventInfo = attrs
            .getAugmentations(i)
            .getItem(AUGMENTATIONS)
            .asInstanceOf[HTMLEventInfo]
          val beginCharacterOffset = htmlEventInfo.getBeginCharacterOffset
          val endCharacterOffset = htmlEventInfo.getEndCharacterOffset
          val beginSearchResult =
            partOffsets.search(beginCharacterOffset)
          val endSearchResult =
            partOffsets.search(endCharacterOffset)
          val beginIndex = beginSearchResult.insertionPoint
          val endIndex = endSearchResult.insertionPoint
          if beginIndex % 2 == 1 && endIndex == beginIndex + 1 then
            Some(i -> (beginIndex / 2))
          else None
        })
        .toMap
    val staticAttributes = XMLAttributesImpl()
    for i <- 0 until attrs.getLength do
      if !dynamicAttributeIndices.contains(i) then
        val qName = QName()
        attrs.getName(i, /* out */ qName)
        staticAttributes.addAttribute(
          qName,
          attrs.getType(i),
          attrs.getValue(i)
        )
      end if
    end for
    super.startElement(element, staticAttributes, augs)
    fCurrentNode.setUserData(
      AttributeArgumentsUserDataKey,
      for (i, arg) <- dynamicAttributeIndices yield
        if attrs.getValue(i) != "" then
          argumentErrorHandler(
            "String interpolation must be the whole attribute value, not a part of the attribute value.",
            arg
          )
        end if
        val qName = QName()
        attrs.getName(i, /* out */ qName)
        qName -> arg
      ,
      null
    )
  private var isProcessingCharacters = false
  override def characters(text: XMLString, augs: Augmentations): Unit =
    if isProcessingCharacters then super.characters(text, augs)
    else
      isProcessingCharacters = true
      try
        if augs == null then super.characters(text, augs)
        else
          val htmlEventInfo =
            augs.getItem(AUGMENTATIONS).asInstanceOf[HTMLEventInfo]
          val beginCharacterOffset = htmlEventInfo.getBeginCharacterOffset
          val endCharacterOffset = htmlEventInfo.getEndCharacterOffset
          val beginSearchResult =
            partOffsets.search(beginCharacterOffset)
          val endSearchResult =
            partOffsets.search(endCharacterOffset)
          val beginIndex = beginSearchResult.insertionPoint
          val endIndex = endSearchResult.insertionPoint
          if beginIndex == endIndex || (beginIndex % 2 == 0 && endIndex == beginIndex + 1)
          then super.characters(text, augs)
          else
            def partLoop(index: Int): Unit =
              assert(index % 2 == 0)
              endSearchResult match
                case Searching.InsertionPoint(`endIndex`)
                    if endIndex == index + 1 =>
                  fParserConfiguration
                    .asInstanceOf[HTMLConfiguration]
                    .evaluateInputSource(
                      XMLInputSource(
                        null,
                        null,
                        null,
                        StringReader(
                          html.substring(
                            partOffsets(index),
                            endCharacterOffset
                          )
                        ),
                        null
                      )
                    )
                case _ =>
                  fParserConfiguration
                    .asInstanceOf[HTMLConfiguration]
                    .evaluateInputSource(
                      XMLInputSource(
                        null,
                        null,
                        null,
                        StringReader(
                          parts(index / 2)
                        ),
                        null
                      )
                    )
                  argLoop(index + 1)
              end match
            end partLoop

            def argLoop(index: Int): Unit =
              assert(index % 2 == 1)
              if endIndex > index then
                val comment = fDocument.createComment("")
                comment.setUserData(
                  ElementArgumentUserDataKey,
                  index / 2,
                  null
                )
                fCurrentNode.appendChild(comment)
                partLoop(index + 1)
              end if
            end argLoop

            beginSearchResult match
              case Searching.InsertionPoint(`beginIndex`)
                  if beginIndex % 2 == 1 =>
                fParserConfiguration
                  .asInstanceOf[HTMLConfiguration]
                  .evaluateInputSource(
                    XMLInputSource(
                      null,
                      null,
                      null,
                      StringReader(
                        html.substring(
                          beginCharacterOffset,
                          partOffsets(beginIndex)
                        )
                      ),
                      null
                    )
                  )
                argLoop(beginIndex)
              case Searching.Found(`beginIndex`) if beginIndex % 2 == 1 =>
                argLoop(beginIndex)
              case Searching.Found(`beginIndex`) if beginIndex % 2 == 0 =>
                partLoop(beginIndex)
              case _ =>
                argumentErrorHandler(s"Unexpected text: $text", beginIndex / 2)
            end match
          end if
        end if
      finally isProcessingCharacters = false
  end characters
end InterpolationParser