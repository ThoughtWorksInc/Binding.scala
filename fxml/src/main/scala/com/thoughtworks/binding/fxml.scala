package com.thoughtworks.binding

import java.beans.{Introspector, PropertyDescriptor}

import com.thoughtworks.binding.Binding.{BindingSeq, Constants, MultiMountPoint}
import com.thoughtworks.sde.core.Preprocessor
import macrocompat.bundle

import scala.annotation.{StaticAnnotation, compileTimeOnly, tailrec}
import scala.collection.GenSeq
import scala.reflect.macros.whitebox
import scala.language.experimental.macros
import com.thoughtworks.Extractor._
import com.thoughtworks.binding.XmlExtractor.{PrefixedName, UnprefixedName}

import scala.collection.immutable.Queue

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class fxml extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro fxml.Macros.macroTransform
}

object fxml {

  private[fxml] sealed trait LowPriorityRuntime {

    final def toBindingSeqBinding[A](binding: Binding[A], dummy: Unit = ()) = {
      Binding.Constant(Constants(()).mapBinding(_ => binding))
    }

  }

  object Runtime extends LowPriorityRuntime {

    final class EmptyText(val value: String) extends AnyVal

    def toBindingSeqBinding[A](bindingSeqBinding: Binding[BindingSeq[A]]) = bindingSeqBinding

    def bindProperty(parentBean: Any, propertyName: String, values: Any*): Unit = macro Macros.bindProperty

    def bindDefaultProperty(parentBean: Any, values: Any*): Unit = macro Macros.bindDefaultProperty

    // This macro does not work if it uses a whitebox Context.
    // I have to use deprecated `scala.reflect.macros.Context` instead.
    def autoBind(c: scala.reflect.macros.Context): c.Expr[Any] = {
      import c.universe._
      c.Expr[Any](
        c.macroApplication match {
          case q"$parent.$macroName" =>
            q"$parent.${newTermName(s"${macroName.decodedName}$$binding")}.bind"
          case Ident(macroName) =>
            q"${newTermName(s"${macroName.decodedName}$$binding")}.bind"
        }
      )
    }

    private[Runtime] sealed trait LowPriorityBuilderBuilder {
      implicit def manifestBuilderBuilder[A](implicit manifest: Manifest[A]) = new BuilderBuilder[A] {
        override final type Builder = A


        override final def newBuilder = manifest.runtimeClass.newInstance().asInstanceOf[A]


        override final def build(builder: Builder) = builder
      }
    }

    object BuilderBuilder extends LowPriorityBuilderBuilder {

      def apply[Value](implicit typeClass: BuilderBuilder[Value]): typeClass.type = typeClass
    }

    trait BuilderBuilder[Value] {

      type Builder

      def newBuilder: Builder

      def build(builder: Builder): Value
    }

    final class JavaListMountPoint[A](javaList: java.util.List[A], bindingSeq: BindingSeq[A]) extends MultiMountPoint[A](bindingSeq) {

      import collection.JavaConverters._

      override protected def set(children: Seq[A]): Unit = {
        javaList.clear()
        javaList.addAll(children.asJava)
      }

      override protected def splice(from: Int, that: GenSeq[A], replaced: Int): Unit = {
        val i = javaList.listIterator(from)
        for (_ <- 0 until replaced) {
          i.next()
          i.remove()
        }
        for (newElement <- that) {
          i.add(newElement)
        }
      }
    }

  }

  private object Macros {
    val Spaces = """\s*""".r
  }

  @bundle
  private[binding] final class Macros(context: whitebox.Context) extends Preprocessor(context) with XmlExtractor {

    import c.universe._

    private def bindPropertyFromDescriptor(parentBean: Tree, descriptorOption: Option[PropertyDescriptor], values: Seq[Tree]): Tree = {
      descriptorOption match {
        case None =>
          // TODO: map
          ???
        case Some(descriptor) =>
          descriptor.getWriteMethod match {
            case null if descriptor.getPropertyType.getInterfaces.contains(classOf[java.util.List[_]]) =>
              def list = q"$parentBean.${TermName(descriptor.getReadMethod.getName)}"
              values match {
                case Seq() =>
                  q"()"
                case Seq(name) =>
                  q"""new _root_.com.thoughtworks.binding.fxml.Runtime.JavaListMountPoint(
                    $list,
                    _root_.com.thoughtworks.binding.fxml.Runtime.toBindingSeqBinding($name).bind
                  ).bind"""
                case _ =>
                  val valueBindings = for (name <- values) yield {
                    q"_root_.com.thoughtworks.binding.fxml.Runtime.toBindingSeqBinding($name)"
                  }
                  q"""new _root_.com.thoughtworks.binding.fxml.Runtime.JavaListMountPoint(
                    $list,
                    _root_.com.thoughtworks.binding.Binding.Constants(..$valueBindings).flatMapBinding(_root_.scala.Predef.locally _)
                  ).bind"""
              }
            case writeMethod =>
              val Seq(value) = values
              atPos(value.pos) {
                if (value.tpe <:< typeOf[Binding.Constant[_]]) {
                  q"$parentBean.${TermName(writeMethod.getName)}($value.get)"
                } else {
                  val monadicBody = q"$parentBean.${TermName(writeMethod.getName)}($value.bind)"
                  q"_root_.com.thoughtworks.binding.Binding[_root_.scala.Unit]($monadicBody).bind"
                }
              }
          }
      }
    }

    def bindProperty(parentBean: Tree, propertyName: Tree, values: Tree*): Tree = {
      val beanClass = Class.forName(parentBean.tpe.typeSymbol.fullName)
      val beanInfo = Introspector.getBeanInfo(beanClass)
      val Literal(Constant(propertyNameString: String)) = propertyName
      val descriptorOption = beanInfo.getPropertyDescriptors.find(_.getName == propertyNameString)
      bindPropertyFromDescriptor(parentBean, descriptorOption, values)
    }

    def bindDefaultProperty(parentBean: Tree, values: Tree*): Tree = {
      val beanClass = Class.forName(parentBean.tpe.typeSymbol.fullName)
      val beanInfo = Introspector.getBeanInfo(beanClass)
      beanInfo.getDefaultPropertyIndex match {
        case -1 =>
          c.error(parentBean.pos, s"Default property for ${show(parentBean)}is not found.")
          q"???"
        case i =>
          val descriptor = beanInfo.getPropertyDescriptors.apply(i)
          bindPropertyFromDescriptor(parentBean, Some(descriptor), values)
      }
    }

    def macroTransform(annottees: Tree*): Tree = {
      val transformer = new ComprehensionTransformer {
        private def transformChildren(children: List[Tree]) = {
          @tailrec
          def loop(children: List[Tree],
                   accumulatedMembers: Queue[Tree],
                   accumulatedPropertyBindings: Queue[(String, Queue[Tree])],
                   accumulatedDefaultBindings: Queue[Tree])
          : (Queue[Tree], Queue[(String, Queue[Tree])], Queue[Tree]) = {
            children match {
              case Nil =>
                (accumulatedMembers, accumulatedPropertyBindings, accumulatedDefaultBindings)
              case head :: tail =>
                head match {
                  // TODO: other cases
                  case Text(Macros.Spaces()) =>
                    loop(
                      tail,
                      accumulatedMembers,
                      accumulatedPropertyBindings,
                      accumulatedDefaultBindings
                    )
                  case transformValue.extract(members, transformedValue) =>
                    loop(
                      tail,
                      accumulatedMembers ++ members,
                      accumulatedPropertyBindings,
                      accumulatedDefaultBindings.enqueue(transformedValue)
                    )
                  case Elem(UnprefixedName(propertyName), Seq(), _, nestedChildren) if propertyName.charAt(0).isLower =>
                    def singleEmptyString(value: String) = {
                      val id = c.freshName(s"$propertyName$$empty")
                      val bindingName = TermName(s"$id$$binding")
                      (
                        q"def $bindingName = _root_.com.thoughtworks.binding.Binding.Constant(new _root_.com.thoughtworks.binding.fxml.Runtime.EmptyText($value))",
                        q"def ${TermName(id)}: _root_.scala.Any = macro _root_.com.thoughtworks.binding.fxml.Runtime.autoBind",
                        q"$bindingName"
                        )
                    }
                    nestedChildren match {
                      case Seq() =>
                        val (member0, member1, binding) = singleEmptyString("")
                        loop(
                          tail,
                          accumulatedMembers.enqueue(member0).enqueue(member1),
                          accumulatedPropertyBindings.enqueue(propertyName -> Queue(binding)),
                          accumulatedDefaultBindings
                        )
                      case Seq(Text(singleText@Macros.Spaces())) =>
                        val (member0, member1, binding) = singleEmptyString(singleText)
                        loop(
                          tail,
                          accumulatedMembers.enqueue(member0).enqueue(member1),
                          accumulatedPropertyBindings.enqueue(propertyName -> Queue(binding)),
                          accumulatedDefaultBindings
                        )
                      case _ =>
                        @tailrec
                        def nestedLoop(nestedChildren: List[Tree],
                                       accumulatedMembers: Queue[Tree],
                                       accumulatedBindings: Queue[Tree])
                        : (Queue[Tree], Queue[Tree]) = {
                          nestedChildren match {
                            case Nil =>
                              (accumulatedMembers, accumulatedBindings)
                            case head :: tail =>
                              head match {
                                // TODO: other cases
                                case Text(Macros.Spaces()) =>
                                  nestedLoop(tail, accumulatedMembers, accumulatedBindings)
                                case transformValue.extract(members, transformedValue) =>
                                  nestedLoop(tail, accumulatedMembers ++ members, accumulatedBindings.enqueue(transformedValue))
                              }

                          }
                        }
                        val (members, currentPropertyBindings) = nestedLoop(nestedChildren, Queue.empty, Queue.empty)
                        loop(
                          tail,
                          accumulatedMembers ++ members,
                          accumulatedPropertyBindings.enqueue(propertyName -> currentPropertyBindings),
                          accumulatedDefaultBindings
                        )
                    }
                }
            }
          }
          loop(children, Queue.empty, Queue.empty, Queue.empty)
        }

        private def transformValue: PartialFunction[Tree, (Queue[Tree], Tree)] = {
          // TODO: static property
          case Text(data) =>
            Queue.empty -> q"_root_.com.thoughtworks.binding.Binding.Constant($data)"
          case tree@Elem(UnprefixedName(className), attributes, _, children) if className.charAt(0).isUpper =>

            // TODO: create new instance


            // TODO: fx:factory
            // TODO: <fx:include> (Read external files)
            // TODO: convert fx:value, fx:constant, <fx:reference> and <fx:copy> to @fxml val


            // Type Coercion
            // FXML uses "type coercion" to convert property values to the appropriate type as needed. Type coercion is required because the only data types supported by XML are elements, text, and attributes (whose values are also text). However, Java supports a number of different data types including built-in primitive value types as well as extensible reference types.
            // The FXML loader uses the coerce() method of BeanAdapter to perform any required type conversions. This method is capable of performing basic primitive type conversions such as String to boolean or int to double, and will also convert String to Class or String to Enum. Additional conversions can be implemented by defining a static valueOf() method on the target type.
            //
            // 不要支持Type Coercion、Location Resolution、Resource Resolution、Variable Resolution、Escape Sequences、Expression Binding，要求用户改用花括号{}以提供类型安全的代码

            // fx:define 生成valDefs

            // 不支持 @FXML

            // TODO: If an element represents a type that already implements Map (such as java.util.HashMap), it is not wrapped and its get() and put() methods are invoked directly. For example, the following FXML creates an instance of HashMap and sets its "foo" and "bar" values to "123" and "456", respectively:

            // fx:factory
            val factoryOption = attributes.collectFirst {
              case (PrefixedName("fx", "factory"), Text(factory)) => factory
            }

            // fx:id
            val idOption = attributes.collectFirst {
              case (PrefixedName("fx", "id"), Text(id)) => id
            }
            val elementName = idOption match {
              case None => TermName(c.freshName(className))
              case Some(id) => TermName(id)
            }

            factoryOption match {
              case None =>
                // TODO: attribute

                val (childrenMembers, childrenProperties, defaultProperties) = transformChildren(children)
                val builderBuilderName = c.freshName[TermName]("builderBuilder")
                val bindingName = TermName(s"${elementName.decodedName}$$binding")

                val definitions = childrenMembers.
                  enqueue(q"""
                      val $bindingName = {
                        val $builderBuilderName = _root_.com.thoughtworks.binding.fxml.Runtime.BuilderBuilder.apply[${TypeName(className)}]
                        val $elementName = $builderBuilderName.newBuilder
                        _root_.com.thoughtworks.binding.Binding.apply({
                          ..${
                    for ((propertyName, values) <- childrenProperties) yield {
                      q"""_root_.com.thoughtworks.binding.fxml.Runtime.bindProperty($elementName, $propertyName, ..$values)"""
                    }
                  }
                          ..${
                    if (defaultProperties.isEmpty) {
                      Nil
                    } else {
                      List(atPos(tree.pos) {
                        q"_root_.com.thoughtworks.binding.fxml.Runtime.bindDefaultProperty($elementName, ..$defaultProperties)"
                      })
                    }
                  }
                          $builderBuilderName.build($elementName)
                        })
                      }
                    """).
                  enqueue(q"""
                      def $elementName: _root_.scala.Any = macro _root_.com.thoughtworks.binding.fxml.Runtime.autoBind
                    """)
                definitions -> q"$bindingName"

              case Some(factory) =>
                ???
            }

          //
          //            val attributeMountPoints = for {
          //              attribute <- attributes
          //            } yield {
          //              val (attributeAccess, value) = attribute match {
          //                case Left((key, value)) =>
          //                  val keyName = TermName(key)
          //                  q"""new _root_.com.thoughtworks.binding.fxml.Runtime.StaticBeanAdapter($elementName).$keyName""" -> value
          //                case Right((pre, key, value)) =>
          //                  key.split(':').foldLeft(q"""new _root_.com.thoughtworks.binding.fxml.Runtime.StaticBeanAdapter($elementName).${TermName(pre)}""") { (prefixExpr, propertyName) =>
          //                    q"""new _root_.com.thoughtworks.binding.Runtime.StaticBeanAdapter($prefixExpr).${TermName(propertyName)}"""
          //                  } -> value
          //              }
          //              atPos(value.pos) {
          //                val assignName = TermName(c.freshName("assignAttribute"))
          //                val newValueName = TermName(c.freshName("newValue"))
          //                q"""
          //                  _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[
          //                    _root_.com.thoughtworks.binding.Binding,
          //                    _root_.scala.Unit
          //                  ](
          //                    _root_.com.thoughtworks.binding.Binding.apply[_root_.scala.Unit]({
          //                      val $newValueName = ${transform(value)}
          //                      @_root_.scala.inline def $assignName() = {
          //                        if ($attributeAccess != $newValueName) {
          //                          $attributeAccess = $newValueName
          //                        }
          //                      }
          //                      $assignName()
          //                    })
          //                  )
          //                """
          //              }
          //            }
          //
          //            val valDefs: Seq[ValOrDefDef] = {
          //              for {
          //                child <- children
          //              } yield {
          //
          //                ???
          //              }
          //              ???
          //            }
          //
          //            val elementDef = q"val $elementName = new ${Ident(TypeName(label))}" // TODO: other factory for new instance
          //            idOption match {
          //              case None =>
          //                valDefs -> q"""
          //                  $elementDef
          //                  ..$transformedChild
          //                  ..$attributeMountPoints
          //                  $elementName
          //                """
          //              case Some(id) =>
          //                (valDefs :+ elementDef) -> q"""
          //                  ..$transformedChild
          //                  ..$attributeMountPoints
          //                  $elementName
          //                """
          //            }

          //            ???
          case tree@Elem(PrefixedName("fx", "include"), attributes, _, children) =>
            ???
          case tree@Elem(PrefixedName("fx", "reference"), attributes, _, children) =>
            ???
          case tree@Elem(PrefixedName("fx", "copy"), attributes, _, children) =>
            ???
          case tree@Elem(PrefixedName("fx", "root"), attributes, _, children) =>
            ??? // Will not support

        }

        private def transformed: PartialFunction[Tree, Tree] = {
          case tree@ProcInstr("import", proctext) =>
            atPos(tree.pos) {
              c.parse(raw"import $proctext")
            }
        }

        override def transform(tree: Tree): Tree = {
          tree match {
            case transformValue.extract(definitions, transformedValue) =>
              val bindingsName = c.freshName[TermName]("Bindings")

              q"""
                object $bindingsName {
                  ..$definitions
                }
                import $bindingsName.{!= => _, ## => _, == => _, asInstanceOf => _, eq => _, equals => _, getClass => _, hashCode => _, ne => _, notify => _, notifyAll => _, isInstanceOf => _, synchronized => _, toString => _, wait => _, _}
                $transformedValue.bind
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
        q"""
          import _root_.scala.language.experimental.macros
          _root_.com.thoughtworks.binding.Binding.apply(${transform(body)})
        """
      })


    }
  }

}