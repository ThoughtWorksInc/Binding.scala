package au.com.realcommercial.bindingScala.dom

import au.com.realcommercial.bindingScala.BindableRope.{Single, Subscriber}
import au.com.realcommercial.bindingScala.{BindableRope, Binding}
import org.scalajs.dom._
import org.scalajs.dom.raw.{HTMLElement, Node}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly
import scala.reflect.macros.whitebox
import scalatags.{jsdom, JsDom}
import scalatags.JsDom.TypedTag


object DomLiteral {

  type DomRope = BindableRope[Node]

  object DomRope {

    @inline
    def apply(domRope: DomRope) = domRope

    @inline
    def apply(text: String) = new BindableRope.Single(document.createTextNode(text))

    @inline
    def apply(node: Node) = new BindableRope.Single(node)

    @inline
    def apply(array: Array[DomRope]) = {
      new BindableRope.ArrayTree(array)
    }

    @inline
    def apply(array: Array[Node]) = {
      new BindableRope.ArrayLeaf(array)
    }

    @inline
    def apply(buffer: mutable.Buffer[Node])(implicit v: Any => Any) = {
      new BindableRope.BufferLeaf(buffer)
    }

    @inline
    def apply(buffer: mutable.Buffer[DomRope]) = {
      new BindableRope.BufferTree(buffer)
    }
  }

  def mount[N <: Node](host: Node, rope: BindableRope[N]): Unit = {
    for (child <- rope.flatten) {
      host.appendChild(child)
    }
    rope.subscribe(new Subscriber[N] {
      override def insert(target: BindableRope[N], index: Int, newChildren: N*): Unit = {
        val targetSeq = target.flatten
        val size = targetSeq.size
        if (index < size) {
          val refChild = targetSeq(index)
          for (newChild <- newChildren) {
            host.insertBefore(newChild, refChild)
          }
        } else if (index == size) {
          for (newChild <- newChildren) {
            host.appendChild(newChild)
          }
        } else {
          throw new IndexOutOfBoundsException
        }
      }

      override def update(target: BindableRope[N], index: Int, newChild: N): Unit = {
        val targetSeq = target.flatten
        val oldChild = targetSeq(index)
        host.replaceChild(newChild, oldChild)
      }

      override def remove(target: BindableRope[N], index: Int): Unit = {
        val targetSeq = target.flatten
        val oldChild = targetSeq(index)
        host.removeChild(oldChild)
      }

      override def splice(target: BindableRope[N], index: Int, numberOfOldChildren: Int, newChildren: N*): Unit = {
        val targetSeq = target.flatten
        val oldChildren = targetSeq.view(index, numberOfOldChildren)
        oldChildren.foreach(host.removeChild)
        val size = targetSeq.size
        val refIndex = index + numberOfOldChildren
        if (refIndex < size) {
          val refChild = targetSeq(refIndex)
          for (newChild <- newChildren) {
            host.insertBefore(newChild, refChild)
          }
        } else if (refIndex == size) {
          for (newChild <- newChildren) {
            host.appendChild(newChild)
          }
        } else {
          throw new IndexOutOfBoundsException
        }
      }
    })
  }

  object TagsAndTags2 extends JsDom.Cap with jsdom.Tags with jsdom.Tags2

  final def emptyAttributeList[Output <: org.scalajs.dom.Element](tag: TypedTag[Output]) = { element: Output =>
    Binding.Constant(())
  }

  final def renderInto(binding: Binding[Element], parent: Element): Unit = {
    def render(): Unit = {
      val element = binding.get
      if (parent.childElementCount == 0) {
        parent.appendChild(element)
      } else if (parent.firstChild != element) {
        parent.replaceChild(element, parent.firstChild)
      }
    }
    render()
    binding.subscribe(render)
  }

  /**
   * Enable XML literal for Binding.scala
   */
  @compileTimeOnly("enable macro paradise to expand macro annotations")
  final class bindingDom extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro bindingImpl
  }

  def bindingImpl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {

    import c.universe._

    val transformer = new Transformer {
      private def makeElement(pos: Position, label: String, attributes: Seq[(String, Tree)], childrenOption: Option[Tree]): Tree = {
        val tag = Select(reify(_root_.au.com.realcommercial.bindingScala.dom.DomLiteral.TagsAndTags2).tree, TermName(label))
        val elementName = c.freshName("element")

        atPos(pos)(Block(
          ValDef(
            NoMods,
            elementName,
            TypeTree(),
            Select(Apply(tag, Nil), TermName("render"))
          ) :: (
            childrenOption match {
              case None => Nil
              case Some(children) => List(Apply(Select(reify(au.com.realcommercial.bindingScala.dom.DomLiteral).tree, TermName("mount")), List(Ident(TermName(elementName)), transform(children))))
            }) ::: (for {
              (key, value) <- attributes
            } yield {
              val assignExpr = c.Expr[Unit](Assign(
                Select(Ident(TermName(elementName)), TermName(key)),
                transform(value)
              ))
              reify(
                _root_.com.thoughtworks.each.Monadic.EachOps[_root_.au.com.realcommercial.bindingScala.Binding, _root_.scala.Unit](
                  _root_.com.thoughtworks.each.Monadic.monadic[_root_.au.com.realcommercial.bindingScala.Binding].apply[_root_.scala.Unit](assignExpr.splice)).each
              ).tree
            })(collection.breakOut(List.canBuildFrom)),
          Ident(TermName(elementName)))

        )
      }

      private def extractChildren: PartialFunction[List[Tree], Tree] = {
        case Typed(expr, Ident(typeNames.WILDCARD_STAR)) :: Nil => expr
      }

      override def transform(tree: Tree): Tree = {
        tree match {
          case
            Block(
            ValDef(_, TermName("$buf"), TypeTree(), Apply(Select(New(Select(Select(Select(Ident(termNames.ROOTPKG), TermName("scala")), TermName("xml")), TypeName("NodeBuffer"))), termNames.CONSTRUCTOR), List())) :: pushChildrenTree,
            Ident(TermName("$buf")))
          =>
            val bufferName = c.freshName("buffer")
            val size = pushChildrenTree.size
            val sizeExpr = c.Expr[Int](Literal(Constant(size)))
            atPos(tree.pos)(Block(
              ValDef(
                NoMods,
                bufferName,
                TypeTree(),
                reify(_root_.au.com.realcommercial.bindingScala.dom.DomLiteral.DomRope(_root_.scala.Array.fill[_root_.au.com.realcommercial.bindingScala.dom.DomLiteral.DomRope](sizeExpr.splice)(_root_.au.com.realcommercial.bindingScala.BindableRope.Empty))).tree
              ) :: (for {
                (Apply(Select(Ident(TermName("$buf")), TermName("$amp$plus")), List(child)), i) <- pushChildrenTree.view.zipWithIndex
              } yield {
                  val updateExpr = c.Expr[Unit](
                Apply(
                  Select(Ident(TermName(bufferName)), TermName("update")),
                  List(
                    Literal(Constant(i)),
                    Apply(
                      reify(_root_.au.com.realcommercial.bindingScala.dom.DomLiteral.DomRope).tree,
                      List(transform(child))
                    )
                  )
                )
              )
                  reify(
                    _root_.com.thoughtworks.each.Monadic.EachOps[_root_.au.com.realcommercial.bindingScala.Binding, _root_.scala.Unit](
                _root_.com.thoughtworks.each.Monadic.monadic[_root_.au.com.realcommercial.bindingScala.Binding].apply[_root_.scala.Unit](updateExpr.splice)).each
                  ).tree
                }).toList,
              Ident(TermName(bufferName))
            ))
          case
            Block(
            ValDef(_, TermName("$md"), Select(Select(Select(Ident(termNames.ROOTPKG), TermName("scala")), TermName("xml")), TypeName("MetaData")), Select(Select(Select(Ident(termNames.ROOTPKG), TermName("scala")), TermName("xml")), TermName("Null"))) ::
              attributes,
            Apply(
            Select(New(Select(Select(Select(Ident(termNames.ROOTPKG), TermName("scala")), TermName("xml")), TypeName("Elem"))), termNames.CONSTRUCTOR),
            Literal(Constant(null)) ::
              Literal(Constant(label: String)) ::
              Ident(TermName("$md")) ::
              Ident(TermName("$scope")) ::
              _ ::
              child)
            )
          =>
            val extractedAttributes = for {
              Assign(Ident(TermName("$md")), Apply(Select(New(Select(Select(Select(Ident(termNames.ROOTPKG), TermName("scala")), TermName("xml")), TypeName("UnprefixedAttribute"))), termNames.CONSTRUCTOR), List(Literal(Constant(key: String)), value, Ident(TermName("$md"))))) <- attributes
            } yield key -> value
            makeElement(tree.pos, label, extractedAttributes, extractChildren.lift(child))
          case
            Apply(
            Select(New(Select(Select(Select(Ident(termNames.ROOTPKG), TermName("scala")), TermName("xml")), TypeName("Elem"))), termNames.CONSTRUCTOR),
            Literal(Constant(null)) ::
              Literal(Constant(label: String)) ::
              Select(Select(Select(Ident(termNames.ROOTPKG), TermName("scala")), TermName("xml")), TermName("Null")) ::
              Ident(TermName("$scope")) ::
              _ ::
              child)
          =>
            makeElement(tree.pos, label, Nil, extractChildren.lift(child))
          case Apply(Select(New(Select(Select(Select(Ident(termNames.ROOTPKG), TermName("scala")), TermName("xml")), TypeName("Text"))), termNames.CONSTRUCTOR), List(text)) =>
            val textName = c.freshName("text")
            val assignExpr = c.Expr[Unit](Assign(
              Select(Ident(TermName(textName)), TermName("textContent")),
              transform(text)
            ))
            atPos(tree.pos)(Block(
              List(
                ValDef(
                  NoMods,
                  textName,
                  TypeTree(),
                  reify(_root_.org.scalajs.dom.document.createTextNode("")).tree
                ),
                reify(
                  _root_.com.thoughtworks.each.Monadic.EachOps[_root_.au.com.realcommercial.bindingScala.Binding, _root_.scala.Unit](
                _root_.com.thoughtworks.each.Monadic.monadic[_root_.au.com.realcommercial.bindingScala.Binding].apply[_root_.scala.Unit](assignExpr.splice)).each
                ).tree
              ),
              Ident(TermName(textName))
            ))
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
      case Seq(annottee) => c.Expr(transform(annottee.tree))
      case _ => {
        c.Expr(Block(
          (for (annottee <- annottees) yield {
            transform(annottee.tree)
          })(collection.breakOut(List.canBuildFrom)),
          reify(()).tree
        ))
      }
    }
  }

}
