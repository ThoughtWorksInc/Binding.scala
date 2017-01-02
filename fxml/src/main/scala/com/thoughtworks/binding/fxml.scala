package com.thoughtworks.binding

import java.beans.{BeanInfo, Introspector, PropertyDescriptor}
import javafx.application.Platform
import javafx.beans.DefaultProperty
import javax.swing.SwingUtilities

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

    def toBindingSeq[A](bindingSeqBinding: Binding[A], dummy: Unit = ()) = {
      bindingSeqBinding match {
        case Binding.Constant(bindingSeq) => Constants(bindingSeq)
        case _ => Constants(()).mapBinding(_ => bindingSeqBinding)
      }
    }

  }

  object Runtime extends LowPriorityRuntime {

    final class EmptyText(val value: String) extends AnyVal

    def toBindingSeqBinding[A](bindingSeqBinding: Binding[BindingSeq[A]]) = bindingSeqBinding

    def toBindingSeq[A](bindingSeqBinding: Binding[BindingSeq[A]]) = {
      bindingSeqBinding match {
        case Binding.Constant(bindingSeq) => bindingSeq
        case _ => Constants(()).flatMapBinding(_ => bindingSeqBinding)
      }
    }

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

    final class JavaListMountPoint[A](javaList: java.util.List[A], bindingSeq: BindingSeq[A])
        extends MultiMountPoint[A](bindingSeq) {

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

    private def initializeJavaFx() = {
      if (!Platform.isFxApplicationThread) {
        val lock = new AnyRef
        @volatile var initialized = false
        lock.synchronized {
          SwingUtilities.invokeLater(new Runnable {
            override def run(): Unit = {
              new javafx.embed.swing.JFXPanel
              Platform.runLater(new Runnable() {
                override def run(): Unit = {
                  lock.synchronized {
                    initialized = true
                    lock.notify()
                  }
                }
              })
            }
          })
          while (!initialized) {
            lock.wait()
          }
        }
      }
    }

    initializeJavaFx()

    val Spaces = """\s*""".r
  }

  @bundle
  private[binding] final class Macros(context: whitebox.Context) extends Preprocessor(context) with XmlExtractor {

    import c.universe._

    private def bindPropertyFromDescriptor(parentBean: Tree,
                                           descriptorOption: Option[PropertyDescriptor],
                                           values: Seq[Tree]): Tree = {
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
                  val mountPointName = c.freshName[TermName](s"${descriptor.getName}$$mountPoint")
                  q"""
                    val $mountPointName = new _root_.com.thoughtworks.binding.fxml.Runtime.JavaListMountPoint(
                      $list,
                      _root_.com.thoughtworks.binding.fxml.Runtime.toBindingSeq($name)
                    )
                    $mountPointName.bind
                  """
                case _ =>
                  val valueBindings = for (name <- values) yield {
                    q"_root_.com.thoughtworks.binding.fxml.Runtime.toBindingSeqBinding($name)"
                  }
                  val mountPointName = c.freshName[TermName](s"${descriptor.getName}$$mountPoint")
                  q"""
                    val $mountPointName = new _root_.com.thoughtworks.binding.fxml.Runtime.JavaListMountPoint(
                      $list,
                      _root_.com.thoughtworks.binding.Binding.Constants(..$valueBindings).flatMapBinding(_root_.scala.Predef.locally _)
                    )
                    $mountPointName.bind
                  """
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
      val beanInfo = Introspector.getBeanInfo(beanClass, classOf[AnyRef], Introspector.USE_ALL_BEANINFO)

      def findDefaultProperty: Option[PropertyDescriptor] = {
        beanInfo.getDefaultPropertyIndex match {
          case -1 =>
            beanClass.getAnnotation(classOf[DefaultProperty]) match {
              case null =>
                None
              case defaultProperty =>
                beanInfo.getPropertyDescriptors.find(_.getName == defaultProperty.value)
            }
          case i =>
            Some(beanInfo.getPropertyDescriptors.apply(i))
        }
      }
      findDefaultProperty match {
        case None =>
          c.error(parentBean.pos, s"Default property for ${beanClass.getCanonicalName} is not found.")
          q"???"
        case Some(descriptor) =>
          bindPropertyFromDescriptor(parentBean, Some(descriptor), values)
      }
    }

    def macroTransform(annottees: Tree*): Tree = {
      val transformer = new ComprehensionTransformer {
        private def transformChildren(children: List[Tree]) = {
          @tailrec
          def loop(children: List[Tree],
                   accumulatedDefinitions: Queue[Tree],
                   accumulatedPropertyBindings: Queue[(String, Position, Seq[Tree])],
                   accumulatedDefaultBindings: Queue[Tree])
            : (Queue[Tree], Queue[(String, Position, Seq[Tree])], Queue[Tree]) = {
            children match {
              case Nil =>
                (accumulatedDefinitions, accumulatedPropertyBindings, accumulatedDefaultBindings)
              case head :: tail =>
                head match {
                  // TODO: other cases
                  case transformImport.extract(transformedImport) =>
                    loop(
                      tail,
                      accumulatedDefinitions.enqueue(transformedImport),
                      accumulatedPropertyBindings,
                      accumulatedDefaultBindings
                    )
                  case Text(Macros.Spaces()) =>
                    loop(
                      tail,
                      accumulatedDefinitions,
                      accumulatedPropertyBindings,
                      accumulatedDefaultBindings
                    )
                  case transformNode.extract(defs, transformedValue) =>
                    loop(
                      tail,
                      accumulatedDefinitions ++ defs,
                      accumulatedPropertyBindings,
                      accumulatedDefaultBindings.enqueue(transformedValue)
                    )
                  case tree @ Elem(UnprefixedName(propertyName),
                                   Seq(),
                                   _,
                                   transformNodeSeq.extract(defs, transformedValues))
                      if propertyName.charAt(0).isLower =>
                    loop(
                      tail,
                      accumulatedDefinitions ++ defs,
                      accumulatedPropertyBindings.enqueue((propertyName, tree.pos, transformedValues)),
                      accumulatedDefaultBindings
                    )
                }
            }
          }
          loop(children, Queue.empty, Queue.empty, Queue.empty)
        }

        private def singleEmptyText(value: String) = {
          val bindingName = c.freshName[TermName]("emptyText")
          q"def $bindingName = _root_.com.thoughtworks.binding.Binding.Constant(new _root_.com.thoughtworks.binding.fxml.Runtime.EmptyText($value))" -> q"$bindingName"
        }

        private def transformImport: PartialFunction[Tree, Tree] = {
          case tree @ ProcInstr("import", proctext) =>
            atPos(tree.pos) {
              c.parse(raw"""import $proctext""") match {
                case q"import $parent.*" => q"import $parent._"
                case i => i
              }
            }
        }

        private def transformNodeSeq: PartialFunction[List[Tree], (Seq[Tree], Seq[Tree])] = {
          case Seq() =>
            val (defs, binding) = singleEmptyText("")
            Seq(defs) -> Seq(binding)
          case Seq(tree @ Text(singleText @ Macros.Spaces())) =>
            val (defs, binding) = singleEmptyText(singleText)
            Seq(defs) -> Seq(binding)
          case children =>
            @tailrec
            def loop(nestedChildren: List[Tree],
                     accumulatedDefinitions: Queue[Tree],
                     accumulatedBindings: Queue[Tree]): (Queue[Tree], Queue[Tree]) = {
              nestedChildren match {
                case Nil =>
                  (accumulatedDefinitions, accumulatedBindings)
                case head :: tail =>
                  head match {
                    // TODO: other cases
                    case transformImport.extract(transformedImport) =>
                      loop(tail, accumulatedDefinitions.enqueue(transformedImport), accumulatedBindings)
                    case Text(Macros.Spaces()) =>
                      loop(tail, accumulatedDefinitions, accumulatedBindings)
                    case transformNode.extract(defs, transformedValue) =>
                      loop(tail, accumulatedDefinitions ++ defs, accumulatedBindings.enqueue(transformedValue))
                  }
              }
            }
            loop(children, Queue.empty, Queue.empty)
        }

        private def transformNode: PartialFunction[Tree, (Seq[Tree], Tree)] = {
          // TODO: static property
          case tree @ Text(data) =>
            Nil -> atPos(tree.pos) {
              q"_root_.com.thoughtworks.binding.Binding.Constant($data)"
            }
          case tree @ Elem(UnprefixedName(className), attributes, _, children) if className.charAt(0).isUpper =>
            // TODO: create new instance

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
                // TODO: attributes
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
                val (childrenDefinitions, childrenProperties, defaultProperties) = transformChildren(children)
                val builderBuilderName = c.freshName[TermName]("builderBuilder")
                val bindingName = TermName(s"${elementName.decodedName}$$binding")

                def bindingDef = {
                  val bindProperties = for ((propertyName, pos, values) <- childrenProperties) yield {
                    atPos(pos) {
                      q"""_root_.com.thoughtworks.binding.fxml.Runtime.bindProperty($elementName, $propertyName, ..$values)"""
                    }
                  }
                  val bindDefaultProperties = if (defaultProperties.isEmpty) {
                    Nil
                  } else {
                    List(atPos(tree.pos) {
                      q"_root_.com.thoughtworks.binding.fxml.Runtime.bindDefaultProperty($elementName, ..$defaultProperties)"
                    })
                  }
                  atPos(tree.pos) {
                    q"""
                        val $bindingName = {
                          val $builderBuilderName = _root_.com.thoughtworks.binding.fxml.Runtime.BuilderBuilder.apply[${TypeName(
                      className)}]
                          val $elementName = $builderBuilderName.newBuilder
                          _root_.com.thoughtworks.binding.Binding.apply({
                            ..$bindProperties
                            ..$bindDefaultProperties
                            $builderBuilderName.build($elementName)
                          })
                        }
                      """
                  }
                }

                val defs = if (idOption.isDefined) {
                  val autoBindDef = atPos(tree.pos) {
                    q"def $elementName: _root_.scala.Any = macro _root_.com.thoughtworks.binding.fxml.Runtime.autoBind"
                  }
                  childrenDefinitions.enqueue(bindingDef).enqueue(autoBindDef)
                } else {
                  childrenDefinitions.enqueue(bindingDef)
                }

                defs -> atPos(tree.pos)(q"$bindingName")
              case Some(factory) =>
                c.error(tree.pos, "fx:factory is not supported yet.")
                Nil -> q"???"
            }

          case tree @ NodeBuffer(transformNodeSeq.extract(defs, values)) =>
            defs -> atPos(tree.pos) {
              values match {
                case Seq() =>
                  q"_root_.com.thoughtworks.binding.Binding.Constant(())"
                case Seq(value) =>
                  value
                case _ =>
                  val valueBindings = for (name <- values) yield {
                    q"_root_.com.thoughtworks.binding.fxml.Runtime.toBindingSeqBinding($name)"
                  }
                  q"_root_.com.thoughtworks.binding.Binding.Constant(_root_.com.thoughtworks.binding.Binding.Constants(..$valueBindings).flatMapBinding(_root_.scala.Predef.locally _))"
              }
            }
          case tree @ Elem(PrefixedName("fx", "include"), attributes, _, children) =>
            c.error(tree.pos, "fx:include is not supported yet.")
            Nil -> q"???"
          case tree @ Elem(PrefixedName("fx", "reference"), attributes, _, children) =>
            c.error(tree.pos, "fx:include is not supported yet.")
            Nil -> q"???"
          case tree @ Elem(PrefixedName("fx", "copy"), attributes, _, children) =>
            c.error(tree.pos, "fx:include is not supported yet.")
            Nil -> q"???"
          case tree @ Elem(PrefixedName("fx", "root"), attributes, _, children) =>
            c.error(tree.pos, "fx:root is not supported yet.")
            Nil -> q"???"
        }

        override def transform(tree: Tree): Tree = {
          tree match {
            case transformNode.extract(defs, transformedValue) =>
              val xmlScopeName = c.freshName[TypeName]("XmlScope")
              val rootName = c.freshName[TermName]("root")

              q"""
                final class $xmlScopeName {
                  ..$defs
                  def $rootName = $transformedValue
                }
                (new $xmlScopeName).$rootName.bind
              """
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

      replaceDefBody(
        annottees, { body =>
          q"""
          import _root_.scala.language.experimental.macros
          _root_.com.thoughtworks.binding.Binding.apply(${transform(body)})
        """
        }
      )

    }
  }

}
