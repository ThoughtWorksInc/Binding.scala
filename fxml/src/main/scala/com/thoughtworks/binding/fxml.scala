package com.thoughtworks.binding

import java.beans.{BeanInfo, Introspector, PropertyDescriptor}
import javafx.application.Platform
import javafx.beans.DefaultProperty
import javafx.fxml.{JavaFXBuilder, JavaFXBuilderFactory}
import javafx.scene.image.Image
import javafx.util.Builder
import javax.swing.SwingUtilities

import com.sun.javafx.fxml.builder.JavaFXImageBuilder
import com.thoughtworks.binding.Binding.{BindingSeq, Constants, MultiMountPoint}
import com.thoughtworks.sde.core.Preprocessor
import macrocompat.bundle

import scala.annotation.{StaticAnnotation, compileTimeOnly, tailrec}
import scala.collection.GenSeq
import scala.reflect.macros.whitebox
import scala.language.experimental.macros
import com.thoughtworks.Extractor._
import com.thoughtworks.binding.XmlExtractor._

import scala.collection.immutable.Queue
import scala.reflect._

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class fxml extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro fxml.Macros.macroTransform
}

object fxml {

  object Runtime {

    object EmptyText

//    final class EmptyText(val value: String) extends AnyVal

    final def toBindingSeqBinding[A](bindingSeqBinding: Binding[BindingSeq[A]]) = bindingSeqBinding

    final def toBindingSeqBinding[A](binding: Binding[A], dummy: Unit = ()) = {
      Binding.Constant(Constants(()).mapBinding(_ => binding))
    }

    final def toBindingSeq[A](bindingSeqBinding: Binding[BindingSeq[A]]) = {
      bindingSeqBinding match {
        case Binding.Constant(bindingSeq) => bindingSeq
        case _ => Constants(()).flatMapBinding(_ => bindingSeqBinding)
      }
    }

    final def toBindingSeq[A](bindingSeqBinding: Binding[A], dummy: Unit = ()) = {
      bindingSeqBinding match {
        case Binding.Constant(bindingSeq) => Constants(bindingSeq)
        case _ => Constants(()).mapBinding(_ => bindingSeqBinding)
      }
    }

    final def toBindingSeq[A](bindingSeq: BindingSeq[A]) = {
      bindingSeq
    }

    final def toBindingSeq[A](bindingSeq: A) = {
      Constants(bindingSeq)
    }

    def bindAttribute(parentBean: Any, attributeName: String, postprocessor: Any, value: Any): Unit =
      macro Macros.bindAttribute

    def bindProperty(parentBean: Any,
                     propertyName: String,
                     nestedAttributes: Seq[(String, Any, Any)],
                     valueSeq: Seq[(Any, Any)]): Unit = macro Macros.bindProperty

    def bindDefaultProperty(parentBean: Any, values: (Any, Any)*): Unit = macro Macros.bindDefaultProperty

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

    final class JavaBean[A: ClassTag] extends JavaBeanOrBuilder[A] {
      override final type Builder = A

      override final def newBuilder = classTag[A].runtimeClass.newInstance().asInstanceOf[A]

      override final def build(builder: Builder) = builder
    }

    final class JavaBeanBuilder[A, B <: Builder[A]: ClassTag] extends JavaBeanOrBuilder[A] {
      override final type Builder = B

      override final def newBuilder = classTag[B].runtimeClass.newInstance().asInstanceOf[B]

      override final def build(builder: Builder) = builder.build()
    }

    private[Runtime] sealed trait LowPriorityJavaBeanOrBuilder {

      implicit def javaBean[A: ClassTag]: JavaBean[A] = new JavaBean[A]

    }

    object JavaBeanOrBuilder extends LowPriorityJavaBeanOrBuilder {

      def apply[Value](implicit typeClass: JavaBeanOrBuilder[Value]): typeClass.type = typeClass

      implicit val imageBuilder = new JavaBeanBuilder[Image, JavaFXImageBuilder]

    }

    trait JavaBeanOrBuilder[Value] {

      type Builder

      def newBuilder: Builder

      def build(builder: Builder): Value
    }

    final class JavaListMountPoint[A](javaList: java.util.List[A])(bindingSeq: BindingSeq[A])
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

  import com.thoughtworks.binding.fxml.Runtime._

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

    private[Macros] val ControllerMethodEventHandler = """(?s)#(.*)""".r

    private[Macros] val Spaces = """\s*""".r

    private[Macros] val ExpressionBinding = """(?s)\$\{(.*)\}\s*""".r

    private[Macros] val VariableResolution = """(?s)\$(.*)""".r

    private[Macros] val EscapeSequences = """(?s)\\(.*)""".r

    private[Macros] val ResourceResolution = """(?s)%(.*)""".r

    private[Macros] val LocationResolution = """(?s)@(.*)""".r

    private[Macros] val OnXxxChangeEvent = """on(\w*)Change""".r

  }

  @bundle
  private[binding] final class Macros(context: whitebox.Context) extends Preprocessor(context) with XmlExtractor {

    import c.universe._
    import Macros._

    private def bindPropertyFromDescriptor(parentBean: Tree,
                                           descriptor: PropertyDescriptor,
                                           namedValues: Seq[(String, Tree, Tree)],
                                           values: Seq[(Tree, Tree)]): Tree = {
      def ensureNoNamedValue(): Unit = {
        namedValues match {
          case Seq() =>
          case (_, _, headValue) +: _ =>
            c.error(headValue.pos, "Only read-only map properties can contain attributes")
        }
      }
      def ensureNoChildElements(): Unit = {
        values match {
          case Seq() =>
          case Seq((postprocessor, _)) if postprocessor.tpe <:< typeOf[EmptyText.type] =>
          case (_, headValue) +: _ =>
            c.error(headValue.pos, "Read-only map properties cannot contain child elements")
        }
      }
      descriptor.getWriteMethod match {
        case null if classOf[java.util.Map[_, _]].isAssignableFrom(descriptor.getPropertyType) =>
          ensureNoChildElements()
          val map = q"$parentBean.${TermName(descriptor.getReadMethod.getName)}"
          q"""..${for ((key, postprocessor, value) <- namedValues) yield {
            val mountPointName = TermName(c.freshName(s"${descriptor.getName}$$$key$$mountPoint"))
            atPos(value.pos) {
              q"""
                val $mountPointName = _root_.com.thoughtworks.binding.Binding.apply[_root_.scala.Unit]($map.put($key, $postprocessor.rawValue($value)))
                $mountPointName.bind
              """
            }
          }}"""
        case null if classOf[java.util.List[_]].isAssignableFrom(descriptor.getPropertyType) =>
          ensureNoNamedValue()
          def list = q"$parentBean.${TermName(descriptor.getReadMethod.getName)}"
          values match {
            case Seq() =>
              q"()"
            case Seq((postprocessor, value)) =>
              val mountPointName = TermName(c.freshName(s"${descriptor.getName}$$mountPoint"))
              q"""
                val $mountPointName = new _root_.com.thoughtworks.binding.fxml.Runtime.JavaListMountPoint(
                  $list
                )(
                  $postprocessor.toBindingSeq($value)
                )
                $mountPointName.bind
              """
            case _ =>
              val valueBindings = for ((postprocessor, value) <- values) yield {
                q"$postprocessor.toBindingSeqBinding($value)"
              }
              val mountPointName = TermName(c.freshName(s"${descriptor.getName}$$mountPoint"))
              q"""
                val $mountPointName = new _root_.com.thoughtworks.binding.fxml.Runtime.JavaListMountPoint(
                  $list
                )(
                  _root_.com.thoughtworks.binding.Binding.Constants(..$valueBindings).flatMapBinding(_root_.scala.Predef.locally _)
                )
                $mountPointName.bind
              """
          }
        case writeMethod =>
          ensureNoNamedValue()
          val Seq((postprocessor, value)) = values
          atPos(value.pos) {
            val monadicBody = q"""
                $parentBean.${TermName(writeMethod.getName)}(
                   $postprocessor.typeCoercion($value.bind)
                )
              """
            q"_root_.com.thoughtworks.binding.Binding[_root_.scala.Unit]($monadicBody).bind"
          }
      }
    }

    def bindProperty(parentBean: Tree, propertyName: Tree, nestedAttributes: Tree, valueSeq: Tree): Tree = {
      // TODO: named value for read-only map

      val q"$seqApply[..$seqType](..$values)" = valueSeq
      val valuePairs = for (q"($postprocessor, $value)" <- values) yield {
        (postprocessor, value)
      }
      val q"$mapApply[..$mapType](..$mapValues)" = nestedAttributes
      val namedValues = for (q"(${Literal(Constant(name: String))}, $postprocessor, $value)" <- mapValues) yield {
        (name, postprocessor, value)
      }
      val beanClass = Class.forName(parentBean.tpe.typeSymbol.fullName)
      val beanInfo = Introspector.getBeanInfo(beanClass)
      val Literal(Constant(propertyNameString: String)) = propertyName
      val descriptorOption = beanInfo.getPropertyDescriptors.find(_.getName == propertyNameString)
      descriptorOption match {
        case None =>
          c.error(propertyName.pos, s"property $propertyNameString is not found")
          q"???"
        case Some(descriptor) =>
          bindPropertyFromDescriptor(parentBean, descriptor, namedValues, valuePairs)
      }
    }

    private def bindAttributeFromDescriptor(parentBean: Tree,
                                            descriptor: PropertyDescriptor,
                                            postprocessor: Tree,
                                            value: Tree): Tree = {
      descriptor.getWriteMethod match {
        case null =>
          c.error(value.pos, s"Property ${descriptor.getName} should not be read-only.")
          q"???"
        case writeMethod =>
          q"""_root_.com.thoughtworks.binding.Binding[_root_.scala.Unit](
            $parentBean.${TermName(writeMethod.getName)}(
              $postprocessor.typeCoercion($value)
            )
          ).bind"""
      }
    }

    def bindAttribute(parentBean: Tree, attributeName: Tree, postprocessor: Tree, value: Tree): Tree = {
      val beanClass = Class.forName(parentBean.tpe.typeSymbol.fullName)
      val Literal(Constant(attributeNameString: String)) = attributeName
      if (beanClass.isAssignableFrom(classOf[java.util.Map[String, _]])) {
        q"""_root_.com.thoughtworks.binding.Binding[_root_.scala.Unit](
          $parentBean.put($attributeNameString, $postprocessor.rawValue($value))
        ).bind"""
      } else {
        val beanInfo = Introspector.getBeanInfo(beanClass)
        attributeNameString match {
          case OnXxxChangeEvent(prefix) =>
            ???
          case _ =>
            val descriptorOption = beanInfo.getPropertyDescriptors.find(_.getName == attributeNameString)
            descriptorOption match {
              case None =>
                c.error(attributeName.pos, s"$beanClass does not have $attributeNameString property.")
                q"???"
              // TODO: parentBean is a Map
              case Some(descriptor) =>
                bindAttributeFromDescriptor(parentBean, descriptor, postprocessor, value)
            }
        }
      }
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
        // TODO: parentBean is a Map
        case Some(descriptor) =>
          val valuePairs = for (q"($postprocessor, $value)" <- values) yield {
            (postprocessor, value)
          }
          bindPropertyFromDescriptor(parentBean, descriptor, Seq.empty, valuePairs)
      }
    }

    def macroTransform(annottees: Tree*): Tree = {
      val transformer = new ComprehensionTransformer {
        private def transformChildren(children: List[Tree]) = {
          @tailrec
          def loop(
              children: List[Tree],
              accumulatedDefinitions: Queue[Tree],
              accumulatedPropertyBindings: Queue[(String, Position, Seq[(String, Tree, Tree)], Seq[(Tree, Tree)])],
              accumulatedDefaultBindings: Queue[(Tree, Tree)])
            : (Queue[Tree],
               Queue[(String, Position, Seq[(String, Tree, Tree)], Seq[(Tree, Tree)])],
               Queue[(Tree, Tree)]) = {
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
                  case transformNode.extract(defs, postprocessor, transformedBinding) =>
                    loop(
                      tail,
                      accumulatedDefinitions ++ defs,
                      accumulatedPropertyBindings,
                      accumulatedDefaultBindings.enqueue((postprocessor, transformedBinding))
                    )
                  case tree @ Elem(UnprefixedName(propertyName),
                                   attributes,
                                   _,
                                   transformNodeSeq.extract(defs, transformedValues))
                      if propertyName.charAt(0).isLower =>
                    val (attributeDefs, transformedAttributes) = transformAttributes(attributes)
                    loop(
                      tail,
                      accumulatedDefinitions ++ attributeDefs ++ defs,
                      accumulatedPropertyBindings.enqueue(
                        (propertyName, tree.pos, transformedAttributes, transformedValues)),
                      accumulatedDefaultBindings
                    )
                  case tree =>
                    loop(
                      tail,
                      accumulatedDefinitions,
                      accumulatedPropertyBindings,
                      accumulatedDefaultBindings.enqueue(
                        (q"_root_.com.thoughtworks.binding.fxml.Runtime.Dynamic", super.transform(tree)))
                    )
                }
            }
          }
          loop(children, Queue.empty, Queue.empty, Queue.empty)
        }

//        private def singleEmptyText(value: String) = {
//          val bindingName = TermName(c.freshName("emptyText"))
//          q"def $bindingName = _root_.com.thoughtworks.binding.Binding.Constant(new _root_.com.thoughtworks.binding.fxml.Runtime.EmptyText($value))" -> q"$bindingName"
//        }

        private def transformImport: PartialFunction[Tree, Tree] = {
          case tree @ ProcInstr("import", proctext) =>
            atPos(tree.pos) {
              c.parse(raw"""import $proctext""") match {
                case q"import $parent.*" => q"import $parent._"
                case i => i
              }
            }
        }

        private def transformAttributeValue(attributeValue: Tree): (Seq[Tree], Tree, Tree) = {
          attributeValue match {
            case TextAttribute(textValue) =>
              textValue match {
                case ExpressionBinding(code) =>
                  c.parse(code)
                  ???
                case VariableResolution(code) =>
                  c.parse(code)
                  ???
                case ResourceResolution(resource) =>
                  ???
                case LocationResolution(location) =>
                  (
                    Nil,
                    q"_root_.com.thoughtworks.binding.fxml.Runtime.TypeCoercion",
                    atPos(attributeValue.pos)(q"""this.getClass.getResource($location).toString()""")
                  )
                case ControllerMethodEventHandler(methodName) =>
                  ???
                case EscapeSequences(rawText) =>
                  (
                    Nil,
                    q"_root_.com.thoughtworks.binding.fxml.Runtime.TypeCoercion",
                    atPos(attributeValue.pos)(q"$rawText")
                  )
                case _ =>
                  (
                    Nil,
                    q"_root_.com.thoughtworks.binding.fxml.Runtime.TypeCoercion",
                    atPos(attributeValue.pos)(q"$textValue")
                  )
              }
            case _ =>
              ???
          }
        }

        private def transformAttributes(attributes: List[(QName, Tree)]): (Queue[Tree], Queue[(String, Tree, Tree)]) = {
          @tailrec
          def loop(attributes: List[(QName, Tree)],
                   accumulatedDefinitions: Queue[Tree],
                   accumulatedPairs: Queue[(String, Tree, Tree)]): (Queue[Tree], Queue[(String, Tree, Tree)]) = {
            attributes match {
              case Nil =>
                (accumulatedDefinitions, accumulatedPairs)
              case (key, value) :: tail =>
                val (attributeDefinitions, postprocessor, transformedAttributeValue) = transformAttributeValue(value)
                key match {
                  case UnprefixedName(attributeName) =>
                    loop(
                      tail,
                      accumulatedDefinitions ++ attributeDefinitions,
                      accumulatedPairs.enqueue((attributeName, postprocessor, transformedAttributeValue))
                    )
                  case _ =>
                    c.error(value.pos, "attributes should not be prefixed")
                    loop(tail, accumulatedDefinitions, accumulatedPairs)
                }
            }
          }
          loop(attributes, Queue.empty, Queue.empty)
        }

        private def transformNodeSeq: PartialFunction[List[Tree], (Queue[Tree], Queue[(Tree, Tree)])] = {
          case Seq() =>
            Queue.empty -> Queue((q"_root_.com.thoughtworks.binding.fxml.Runtime.EmptyText", q""" "" """))
          case Seq(tree @ Text(singleText @ Macros.Spaces())) =>
            Queue.empty -> Queue((q"_root_.com.thoughtworks.binding.fxml.Runtime.EmptyText", q"$singleText"))
          case children =>
            @tailrec
            def loop(nestedChildren: List[Tree],
                     accumulatedDefinitions: Queue[Tree],
                     accumulatedBindings: Queue[(Tree, Tree)]): (Queue[Tree], Queue[(Tree, Tree)]) = {
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
                    case transformNode.extract(defs, postprocessor, transformedValue) =>
                      loop(tail,
                           accumulatedDefinitions ++ defs,
                           accumulatedBindings.enqueue((postprocessor, transformedValue)))
                    case tree =>
                      loop(
                        tail,
                        accumulatedDefinitions,
                        accumulatedBindings.enqueue(
                          (q"_root_.com.thoughtworks.binding.fxml.Runtime.Dynamic", super.transform(tree)))
                      )

                  }
              }
            }
            loop(children, Queue.empty, Queue.empty)
        }

        private def transformNode: PartialFunction[Tree, (Seq[Tree], Tree, Tree)] = {
          // TODO: static property
          case tree @ Text(data) =>
            (Nil, q"_root_.com.thoughtworks.binding.fxml.Runtime.TypeCoercion", atPos(tree.pos)(q"$data"))
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

            val (fxAttributes, otherAttributes) = attributes.partition {
              case (PrefixedName("fx", _), _) => true
              case _ => false
            }

            val fxAttributeMap = fxAttributes.view.map {
              case (PrefixedName("fx", key), value) => key -> value
            }.toMap

            val fxIdOption = fxAttributeMap.get("id").map {
              case Text(nonEmptyId) =>
                nonEmptyId
              case EmptyAttribute() =>
                c.error(tree.pos, "fx:id must not be empty.")
                "<error>"
            }
            (fxAttributeMap.get("factory"), fxAttributeMap.get("value")) match {
              case (Some(_), Some(_)) =>
                c.error(tree.pos, "fx:factory and fx:value must not be present on the same element.")
                (Nil, q"???", q"???")
              case (None, None) =>
                val elementName = fxIdOption match {
                  case None =>
                    TermName(c.freshName(className))
                  case Some(id) =>
                    TermName(id)
                }
                val (attributesDefinitions, transformedPairs) = transformAttributes(otherAttributes)
                val (childrenDefinitions, childrenProperties, defaultProperties) = transformChildren(children)
                val builderBuilderName = TermName(c.freshName("builderBuilder"))
                val bindingName = TermName(s"${elementName.decodedName}$$binding")

                def bindingDef = {
                  val bindAttributes = for ((attributeName, postprocessor, value) <- transformedPairs) yield {
                    q"""_root_.com.thoughtworks.binding.fxml.Runtime.bindAttribute(
                      $elementName,
                      $attributeName,
                      $postprocessor,
                      $value
                    )"""
                  }
                  val bindProperties = for ((propertyName, pos, namedValues, values) <- childrenProperties) yield {
                    val namedTuples = for ((name, postprocessor, value) <- namedValues) yield {
                      q"($name, $postprocessor, $value)"
                    }
                    atPos(pos) {
                      q"""_root_.com.thoughtworks.binding.fxml.Runtime.bindProperty($elementName, $propertyName, Seq(..$namedTuples), Seq(..$values))"""
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
                        val $builderBuilderName = _root_.com.thoughtworks.binding.fxml.Runtime.JavaBeanOrBuilder.apply[${TypeName(
                      className)}]
                        val $elementName = $builderBuilderName.newBuilder
                        _root_.com.thoughtworks.binding.Binding.apply({
                          ..$bindAttributes
                          ..$bindProperties
                          ..$bindDefaultProperties
                          $builderBuilderName.build($elementName)
                        })
                      }
                    """
                  }
                }

                val defs = if (fxIdOption.isDefined) {
                  val autoBindDef = atPos(tree.pos) {
                    q"def $elementName: _root_.scala.Any = macro _root_.com.thoughtworks.binding.fxml.Runtime.autoBind"
                  }
                  (attributesDefinitions ++ childrenDefinitions).enqueue(bindingDef).enqueue(autoBindDef)
                } else {
                  (attributesDefinitions ++ childrenDefinitions).enqueue(bindingDef)
                }

                (defs, q"_root_.com.thoughtworks.binding.fxml.Runtime.Binding", atPos(tree.pos)(q"$bindingName"))
              case (Some(EmptyAttribute()), None) =>
                c.error(tree.pos, "fx:factory must not be empty.")
                (Nil, q"???", q"???")
              case (Some(Text(fxFactory)), None) =>
                transformChildren(children) match {
                  case (childrenDefinitions, Queue(), defaultProperties) =>
                    val elementName = fxIdOption match {
                      case None =>
                        TermName(c.freshName(className))
                      case Some(id) =>
                        TermName(id)
                    }
                    val bindingName = TermName(s"${elementName.decodedName}$$binding")
                    def bindingDef = {
                      val factoryArgumentNames = for (i <- 0 until defaultProperties.length) yield {
                        TermName(c.freshName(s"fxFactoryArgument$i"))
                      }
                      val factoryArguments = for (name <- factoryArgumentNames) yield {
                        q"val $name = $EmptyTree"
                      }
                      // TODO: Support more than 12 parameters by generate more sophisticated code
                      val applyN = TermName(s"apply${defaultProperties.length}")
                      q"""
                        val $bindingName = _root_.com.thoughtworks.binding.Binding.BindingInstances.$applyN(..$defaultProperties)({ ..$factoryArguments =>
                          ${TermName(className)}.${TermName(fxFactory)}(..$factoryArgumentNames)
                        })
                      """
                    }
                    val defs = if (fxIdOption.isDefined) {
                      val autoBindDef = atPos(tree.pos) {
                        q"def $elementName: _root_.scala.Any = macro _root_.com.thoughtworks.binding.fxml.Runtime.autoBind"
                      }
                      childrenDefinitions.enqueue(bindingDef).enqueue(autoBindDef)
                    } else {
                      childrenDefinitions.enqueue(bindingDef)
                    }
                    (defs, q"_root_.com.thoughtworks.binding.fxml.Runtime.Binding", atPos(tree.pos)(q"$bindingName"))
                  case (_, (_, pos, _, _) +: _, _) =>
                    c.error(pos, "fx:factory must not contain named property")
                    (Nil, q"???", q"???")
                }
              case (None, Some(TextAttribute(fxValue))) =>
                fxIdOption match {
                  case None =>
                    (Nil, q"_root_.com.thoughtworks.binding.fxml.Runtime.Raw", atPos(tree.pos) {
                      q"${TermName(className)}.valueOf($fxValue)"
                    })
                  case Some(fxId) =>
                    val idDef = atPos(tree.pos) {
                      q"val ${TermName(fxId)} = ${TermName(className)}.valueOf($fxValue)"
                    }
                    (Queue(idDef), q"_root_.com.thoughtworks.binding.fxml.Runtime.Raw", atPos(tree.pos) {
                      q"_root_.com.thoughtworks.binding.Binding.Constant(${TermName(fxId)})"
                    })
                }
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
                  q"_root_.com.thoughtworks.binding.Binding.Constants(..$valueBindings).flatMapBinding(_root_.scala.Predef.locally _)"
              }
            }
          case tree @ Elem(PrefixedName("fx", "include"), attributes, _, children) =>
            c.error(tree.pos, "fx:include is not supported yet.")
            (Nil, q"???", q"???")
          case tree @ Elem(PrefixedName("fx", "reference"), attributes, _, children) =>
            c.error(tree.pos, "fx:reference is not supported yet.")
            (Nil, q"???", q"???")
          case tree @ Elem(PrefixedName("fx", "copy"), attributes, _, children) =>
            c.error(tree.pos, "fx:copy is not supported yet.")
            (Nil, q"???", q"???")
          case tree @ Elem(PrefixedName("fx", "root"), attributes, _, children) =>
            c.error(tree.pos, "fx:root is not supported yet.")
            (Nil, q"???", q"???")
        }

        override def transform(tree: Tree): Tree = {
          tree match {
            case transformNode.extract(defs, postprocessor, transformedValue) =>
              val xmlScopeName = TypeName(c.freshName("XmlScope"))
              val rootName = TermName(c.freshName("root"))

              q"""
                final class $xmlScopeName {
                  ..$defs
                  def $rootName = _root_.com.thoughtworks.binding.Binding {
                    $postprocessor.rawValue($transformedValue)
                  }
                }
                (new $xmlScopeName).$rootName
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

      replaceDefBody(annottees, transform)

    }
  }

}
