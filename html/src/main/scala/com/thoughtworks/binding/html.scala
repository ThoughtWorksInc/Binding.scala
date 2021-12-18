package com.thoughtworks.binding
import scala.quoted.Expr
import scala.quoted.Quotes
import java.io.Reader
import net.sourceforge.htmlunit.cyberneko.HTMLConfiguration
import net.sourceforge.htmlunit.cyberneko.HTMLScanner
import net.sourceforge.htmlunit.cyberneko.parsers.DOMFragmentParser
import org.apache.xerces.xni.parser.XMLInputSource
import java.io.StringReader
import org.w3c.dom.DocumentFragment
import net.sourceforge.htmlunit.cyberneko.parsers.DOMParser
import org.apache.xerces.xni.XMLAttributes
import org.apache.xerces.xni.Augmentations
import org.apache.xerces.xni.QName
import net.sourceforge.htmlunit.cyberneko.HTMLEventInfo
import net.sourceforge.htmlunit.cyberneko.HTMLScanner
import org.apache.xerces.util.XMLStringBuffer
import org.apache.xerces.xni.XMLString
import java.io.CharArrayReader
import java.io.PipedReader
import java.io.PipedWriter
import org.xml.sax.InputSource
import scala.collection.Searching
import org.apache.xerces.util.XMLAttributesImpl
import net.sourceforge.htmlunit.cyberneko.HTMLAugmentations
import org.apache.xml.serialize.XMLSerializer
import org.w3c.dom.ls.LSSerializer
import org.w3c.dom.ls.DOMImplementationLS

private[binding] object Macros:
  val Placeholder = "\"\""
  val ElementArgumentUserDataKey = "Binding.scala element argument"
  val AttributeArgumentsUserDataKey =
    "Binding.scala attribute arguments"
  val AUGMENTATIONS = "http://cyberneko.org/html/features/augmentations"
  val SYNTHESIZED_ITEM =
    HTMLEventInfo.SynthesizedItem();
  def parseHtmlParts(using Quotes)(
      parts: IndexedSeq[String],
      argTerms: IndexedSeq[scala.quoted.quotes.reflect.Term]
  ) =
    import scala.quoted.quotes.reflect.report
    val html = parts.mkString(Placeholder)
    val partOffsets = parts.view
      .flatMap { part =>
        Seq(part.length, Placeholder.length)
      }
      .scanLeft(0)(_ + _)
      .dropRight(1)
      .toIndexedSeq
    val document = org.apache.html.dom.HTMLDocumentImpl()
    val fragment = document.createDocumentFragment()
    val parser = new DOMFragmentParser:
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
            staticAttributes.addAttribute(
              QName(
                attrs.getPrefix(i),
                attrs.getLocalName(i),
                attrs.getQName(i),
                attrs.getURI(i)
              ),
              attrs.getType(i),
              attrs.getValue(i)
            )
        // attrs.getAugmentations(0).getItem(AUGMENTATIONS).asInstanceOf[HTMLEventInfo]
        super.startElement(element, staticAttributes, augs)
        fCurrentNode.setUserData(
          AttributeArgumentsUserDataKey,
          for (i, argIndex) <- dynamicAttributeIndices.view
          yield
            if attrs.getValue(i) != "" then
              report.error(
                "String interpolation must be the whole attribute value, not a part of the attribute value.",
                argTerms(argIndex).pos
              )
            QName(
              attrs.getPrefix(i),
              attrs.getLocalName(i),
              attrs.getQName(i),
              attrs.getURI(i)
            ) -> argIndex
          ,
          null
        )

      override def characters(text: XMLString, augs: Augmentations): Unit =
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
        if beginIndex == endIndex || (beginIndex % 2 == 0 && endIndex == beginIndex + 1) then
          super.characters(text, augs)
        else
          def partLoop(index: Int): Unit =
            assert(index % 2 == 0)
            endSearchResult match {
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
            }

          def argLoop(index: Int): Unit =
            assert(index % 2 == 1)
            if (endIndex > index) {
              val comment = document.createComment(ElementArgumentUserDataKey)
              comment.setUserData(ElementArgumentUserDataKey, index / 2, null)
              fCurrentNode.appendChild(comment)
              partLoop(index + 1)
            }

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
              report.error("Unexpected text: " + text)

    parser.parse(InputSource(StringReader(html)), fragment)
    fragment

  def html(stringContext: Expr[StringContext], args: Expr[Any])(using
      Quotes
  ): Expr[org.scalajs.dom.Element] =
    import scala.quoted.quotes.reflect.Printer
    import scala.quoted.quotes.reflect.report
    import scala.quoted.quotes.reflect.asTerm
    import scala.quoted.quotes.reflect.TypeRepr
    import scala.quoted.quotes.reflect.Typed
    import scala.quoted.quotes.reflect.Literal
    import scala.quoted.quotes.reflect.Repeated
    val Typed(Repeated(argTermList, _), _) = args.asTerm.underlyingArgument
    val argTerms = argTermList.toIndexedSeq

    val '{ StringContext($partsExpr: _*) } = stringContext
    val Expr(partList) = partsExpr
    val parts = partList.toIndexedSeq

    val fragment = parseHtmlParts(parts, argTerms)
    report.info(
      "arg:" + fragment.getFirstChild.getChildNodes
        .item(1)
        .getUserData(ElementArgumentUserDataKey)
    )
    report.warning(
      fragment.getOwnerDocument.getImplementation
        .getFeature("LS", "3.0")
        .asInstanceOf[DOMImplementationLS]
        .createLSSerializer()
        .writeToString(fragment)
    )
    '{ ??? }

extension (inline stringContext: StringContext)
  transparent inline def html(inline args: Any*): org.scalajs.dom.Element = ${
    Macros.html('stringContext, 'args)
  }
