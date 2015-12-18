package au.com.realcommercial.binding

import au.com.realcommercial.binding.Binding.{SingleMountPoint, Constants, BindingSeq, MultiMountPoint}
import org.scalajs.dom.{Text, Node}

import scala.annotation.{tailrec, StaticAnnotation, compileTimeOnly}
import scala.collection.GenSeq
import scala.reflect.macros.whitebox
import scala.language.experimental.macros
import scalatags.JsDom
import scalatags.jsdom
import org.scalajs.dom.document

/**
  * Enable XML DOM literal for Binding.scala
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class dom extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro dom.Macros.macroTransform
}

/**
  * Internal helpers for `@dom` annotation
  */
object dom {

  final class AttributeMountPoint[Value](valueBinding: Binding[Value])(setter: Value => Unit)
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
          parent.insertBefore(newChild, refChild)
        }
      } else {
        for (newChild <- that) {
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

  private[binding] object Macros {

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
                  _root_.scala.Array[_root_.au.com.realcommercial.binding.Binding[
                    _root_.au.com.realcommercial.binding.Binding.BindingSeq[org.scalajs.dom.Node]]
                  ](
                    ..${
                  for {
                    pushChild <- pushChildrenTree
                  } yield {
                    val q"$$buf.$$amp$$plus($child)" = pushChild
                    atPos(pushChild.pos) {
                      q"""
                            _root_.com.thoughtworks.each.Monadic.monadic[_root_.au.com.realcommercial.binding.Binding] {
                              _root_.au.com.realcommercial.binding.dom.domBindingSeq(${transform(child)})
                            }
                          """
                    }
                  }
                }
                  ): _root_.scala.collection.Seq[
                    _root_.au.com.realcommercial.binding.Binding[
                      _root_.au.com.realcommercial.binding.Binding.BindingSeq[org.scalajs.dom.Node]
                    ]
                  ]
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
                val q"$$md = new _root_.scala.xml.UnprefixedAttribute(${Literal(Constant(key: String))}, $value, $$md)" = attribute
                val keyName = TermName(key)
                atPos(attribute.pos) {
                  q"""
                    new _root_.au.com.realcommercial.binding.dom.AttributeMountPoint(
                      _root_.com.thoughtworks.each.Monadic.monadic[_root_.au.com.realcommercial.binding.Binding]($value)
                    )( value => $elementName.$keyName = value ).each
                  """
                }
              }
              atPos(tree.pos) {
                q"""
                  {
                    val $elementName = _root_.au.com.realcommercial.binding.dom.TagsAndTags2.$labelName().render
                    ..$attributeMountPoints
                    ..${
                  if (child.isEmpty) {
                    Nil
                  } else {
                    List(
                      q"""new _root_.au.com.realcommercial.binding.dom.NodeSeqMountPoint(
                          $elementName,
                          _root_.au.com.realcommercial.binding.Binding.Constants(..${child.map(transform(_))}).flatMapBinding(_root_.scala.Predef.locally)
                        ).each""")
                  }
                }
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
                    val $elementName = _root_.au.com.realcommercial.binding.dom.TagsAndTags2.$labelName().render
                    ..${
                  if (child.isEmpty) {
                    Nil
                  } else {
                    List(
                      q"""new _root_.au.com.realcommercial.binding.dom.NodeSeqMountPoint(
                          $elementName,
                          _root_.au.com.realcommercial.binding.Binding.Constants(..${child.map(transform(_))}).flatMapBinding(_root_.scala.Predef.locally)
                        ).each""")
                  }
                }
                    $elementName
                  }
                """
              }
            case q"""new _root_.scala.xml.Text($text)""" =>
              val textName = TermName(c.freshName("text"))
              atPos(tree.pos) {
                q"""
                  {
                    val $textName = _root_.org.scalajs.dom.document.createTextNode("")
                    new _root_.au.com.realcommercial.binding.dom.TextMountPoint(
                      $textName,
                      _root_.com.thoughtworks.each.Monadic.monadic[
                        _root_.au.com.realcommercial.binding.Binding
                      ].apply[_root_.scala.Predef.String] {
                        $text
                      }
                    ).each
                    $textName
                  }
                """
              }
            case _ =>
              super.transform(tree)

          }
        }
      }

      def transform(tree: Tree): Tree = {
        val output = transformer.transform(tree)
        // c.info(c.enclosingPosition, show(output), true)
        c.untypecheck(output)
      }

      annottees match {
        case Seq(annottee@DefDef(mods, name, tparams, vparamss, tpt, rhs)) =>
          atPos(annottee.pos) {
            DefDef(
              mods, name, tparams, vparamss, tpt,
              q"""_root_.com.thoughtworks.each.Monadic.monadic[
                _root_.au.com.realcommercial.binding.Binding
              ](${transform(rhs)})"""
            )
          }
        case Seq(annottee@ValDef(mods, name, tpt, rhs)) =>
          atPos(annottee.pos) {
            ValDef(
              mods, name, tpt,
              q"""_root_.com.thoughtworks.each.Monadic.monadic[
                _root_.au.com.realcommercial.binding.Binding
              ](${transform(rhs)})"""
            )
          }
        case _ =>
          c.error(c.enclosingPosition, "Expect def or val")
          annottees.head
      }
    }

  }

}
