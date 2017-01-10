package com.thoughtworks
package binding

import java.beans.{BeanInfo, Introspector, PropertyDescriptor}
import javafx.application.Platform
import javafx.beans.DefaultProperty
import javafx.fxml.JavaFXBuilderFactory
import javafx.scene.image.Image
import javax.swing.SwingUtilities

import com.sun.javafx.fxml.builder.JavaFXImageBuilder
import com.thoughtworks.binding.Binding.{BindingSeq, Constants, MultiMountPoint}
import com.thoughtworks.sde.core.Preprocessor
import macrocompat.bundle

import scala.annotation.{StaticAnnotation, compileTimeOnly, tailrec}
import scala.collection.GenSeq
import scala.reflect.macros.{TypecheckException, whitebox}
import scala.language.experimental.macros
import com.thoughtworks.Extractor._
import com.thoughtworks.binding.XmlExtractor._

import scala.collection.immutable.Queue
import scalaz.{Monoid, Semigroup}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class fxml extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro fxml.Macros.macroTransform
}

object fxml {

  object Runtime {

    val bindingUnitSemigroup: Semigroup[Binding[Unit]] = {
      implicit val unitSemigroup: Semigroup[Unit] = Semigroup.instance { case _ => () }
      Semigroup.liftSemigroup
    }
    val bindingStringSemigroup: Semigroup[Binding[String]] = {
      import scalaz.std.string._
      Semigroup.liftSemigroup
    }

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

    object EmptyConstructor {
      def apply[A](a: => A) = new EmptyConstructor(a _)
      implicit def emptyConstructor[A]: EmptyConstructor[A] = macro Macros.emptyConstructor[A]
    }

    final class EmptyConstructor[A](val f: () => A) extends AnyVal {
      def apply() = f()
    }

    final class JavaMapBuilder[Key, Value, M <: java.util.Map[Key, Value]](
        implicit val constructor: EmptyConstructor[M])
        extends Builder[M] {
      def build(initializer: M => Seq[(Seq[String], Seq[Binding[_]])]): Binding[M] = {
//        val map = constructor()
//        val properties = initializer(map)
//        for ((key, value) <- properties) yield {
//          key match {
//            case Seq()
//          }
//        }
//        Binding.BindingInstances.sequence()
        ??? // TODO: Replace this implementation to macro, so that we can raise an error if key is nested, and wipe out empty text

      }
    }

    final class JavaBeanBuilder[A](implicit val constructor: EmptyConstructor[A]) extends Builder[A] {

      def build(initializer: A => Seq[(Seq[String], Seq[Binding[_]])]): Binding[A] = macro Macros.buildJavaBean[A]

    }

    final class BuilderBuiler[A, B <: javafx.util.Builder[A]](implicit val constructor: EmptyConstructor[B])
        extends Builder[A] {

      def build(initializer: A => Seq[(Seq[String], Seq[Binding[_]])]): Binding[A] =
        macro Macros.buildFromBuilder[A, B]

    }

    private[Runtime] sealed trait LowPriorityBuilder {

      implicit final def buildJavaBeaner[A](implicit constructor: EmptyConstructor[A]): JavaBeanBuilder[A] = {
        new JavaBeanBuilder
      }

    }

    object Builder extends LowPriorityBuilder {

      implicit def imageBuilder(
          implicit constructor: EmptyConstructor[JavaFXImageBuilder]): BuilderBuiler[Image, JavaFXImageBuilder] =
        new BuilderBuiler

      implicit def JavaMapBuilder[Key, Value, M <: java.util.Map[Key, Value]](
          implicit constructor: EmptyConstructor[M]): JavaMapBuilder[Key, Value, M] = {
        new JavaMapBuilder
      }

      def apply[Value](implicit builder: Builder[Value]): builder.type = builder

    }

    trait Builder[Value]

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

    private[Macros] val Spaces = """\s*""".r

    private[Macros] val ExpressionBinding = """(?s)\$\{(.*)\}\s*""".r

    private[Macros] val VariableResolution = """(?s)\$(.*)""".r

    private[Macros] val EscapeSequences = """(?s)\\(.*)""".r

    private[Macros] val ResourceResolution = """(?s)%(.*)""".r

    private[Macros] val LocationResolution = """(?s)@(.*)""".r

  }

  @bundle
  private[binding] final class Macros(context: whitebox.Context) extends Preprocessor(context) with XmlExtractor {

    import c.universe._
    import c.internal.decorators._

    import Macros._

    private implicit def constantLiftable[A: Liftable]: Liftable[Binding.Constant[A]] =
      new Liftable[Binding.Constant[A]] {
        override def apply(value: Binding.Constant[A]): Tree = {
          q"_root_.com.thoughtworks.binding.Binding.Constant(..${value.get})"
        }
      }
    private implicit def seqLiftable[A: Liftable]: Liftable[Seq[A]] = new Liftable[Seq[A]] {
      override def apply(value: Seq[A]): Tree = {
        q"_root_.scala.Seq(..$value)"
      }
    }

    private implicit def queueLiftable[A: Liftable]: Liftable[Queue[A]] = new Liftable[Queue[A]] {
      override def apply(value: Queue[A]): Tree = {
        q"_root_.scala.collection.immutable.Queue(..$value)"
      }
    }

    private implicit def treeLiftable: Liftable[Tree] = new Liftable[Tree] {
      override def apply(value: Tree): Tree = value
    }

    // Workaround for Scala 2.10
    private def lift[A](a: A)(implicit liftable: Liftable[A]) = liftable(a)

    def emptyConstructor[A](implicit weakTypeTag: c.WeakTypeTag[A]): Tree = {
      q"_root_.com.thoughtworks.binding.fxml.Runtime.EmptyConstructor(new ${weakTypeTag.tpe}())"
    }

    private object EmptyBinding {
      private val ConstantSymbol = typeOf[Binding.Constant.type].termSymbol
      def unapply(tree: Tree): Boolean = {
        tree match {
          case q"$c.apply[$stringType](${Literal(Constant(Macros.Spaces()))})"
              if stringType.tpe <:< typeOf[String] && c.symbol == ConstantSymbol =>
            true
          case _ =>
            false
        }
      }
    }

    private def map(binding: Tree)(f: Tree => Tree) = {
      atPos(binding.pos) {
        val valueName = TermName(c.freshName("value"))
        q"""_root_.com.thoughtworks.binding.Binding.typeClass.map($binding)({ $valueName: ${TypeTree()} =>
          ${f(atPos(binding.pos)(q"$valueName"))}
        })"""
      }
    }

    private def bindPropertyFromDescriptor(parentBean: Tree,
                                           descriptor: PropertyDescriptor,
                                           bindings: Seq[Tree]): Tree = {

      descriptor.getWriteMethod match {
        case null if classOf[java.util.List[_]].isAssignableFrom(descriptor.getPropertyType) =>
          val nonEmptyBindings = bindings.filterNot(EmptyBinding.unapply)
          def list = q"$parentBean.${TermName(descriptor.getReadMethod.getName)}"
          nonEmptyBindings match {
            case Seq() =>
              q"_root_.com.thoughtworks.binding.Binding.Constant(())"
            case Seq(name) =>
              q"""
                new _root_.com.thoughtworks.binding.fxml.Runtime.JavaListMountPoint(
                  $list
                )(
                  _root_.com.thoughtworks.binding.fxml.Runtime.toBindingSeq($name)
                )
              """
            case _ =>
              val valueBindings = for (name <- nonEmptyBindings) yield {
                q"_root_.com.thoughtworks.binding.fxml.Runtime.toBindingSeqBinding($name)"
              }
              q"""
                 new _root_.com.thoughtworks.binding.fxml.Runtime.JavaListMountPoint(
                  $list
                )(
                  _root_.com.thoughtworks.binding.Binding.Constants(..$valueBindings).flatMapBinding(_root_.scala.Predef.locally _)
                )
              """
          }
        case writeMethod =>
          def mapSetter(binding: Tree) = {
            map(binding) { value =>
              q"$parentBean.${TermName(writeMethod.getName)}($value)"
            }
          }
          if (classOf[String].isAssignableFrom(descriptor.getPropertyType)) {
            bindings match {
              case Seq() =>
                q"""_root_.com.thoughtworks.binding.Binding.Constant(())"""
              case Seq(value) =>
                mapSetter(value)
              case nonEmptyBindings =>
                val value = nonEmptyBindings.reduce { (left, right) =>
                  q"_root_.com.thoughtworks.binding.fxml.Runtime.bindingStringSemigroup.append($left, $right)"
                }
                mapSetter(value)
            }
          } else {
            bindings.filterNot(EmptyBinding.unapply) match {
              case Seq() =>
                q"""_root_.com.thoughtworks.binding.Binding.Constant(())"""
              case Seq(value) =>
                mapSetter(value)
            }
          }
      }
    }

    private def arguments: PartialFunction[Tree, Seq[Tree]] = {
      case q"new $t(..$arguments)" => arguments
      case q"new $t()" => Nil
    }

    private def findDefaultProperty(beanClass: Class[_], beanInfo: BeanInfo): Option[PropertyDescriptor] = {
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

    @tailrec
    private def resolve(beanClass: Class[_],
                        beanInfo: BeanInfo,
                        bean: Tree,
                        getters: Seq[Tree]): (Class[_], BeanInfo, Tree) = {
      getters match {
        case Seq() =>
          (beanClass, beanInfo, bean)
        case (head @ Literal(Constant(name: String))) +: tail =>
          beanInfo.getPropertyDescriptors.find(_.getName == name) match {
            case None =>
              throw TypecheckException(head.pos, s"$name is not a property of ${beanInfo.getBeanDescriptor.getName}")
            case Some(propertyDescriptor) =>
              val nestedClass = propertyDescriptor.getPropertyType
              val nestedInfo = Introspector.getBeanInfo(nestedClass)
              val nestedBean = q"$bean.${TermName(propertyDescriptor.getReadMethod.getName)}"
              resolve(nestedClass, nestedInfo, nestedBean, tail)
          }
      }
    }

    private def mapMethodName(numberOfParamters: Int) = {
      numberOfParamters match {
        case 1 => TermName("map")
        case _ => TermName(s"apply$numberOfParamters")
      }
    }

    def buildFromBuilder[Out: WeakTypeTag, Builder: WeakTypeTag](initializer: Tree): Tree = {

      val q"{ $valDef => $seq(..$properties) }" = initializer

      val builderName = TermName(c.freshName("fxBuilder"))
      val beanId = q"$builderName"
      val beanType = weakTypeOf[Builder]
      val beanClass = Class.forName(beanType.typeSymbol.fullName)
      val beanInfo = Introspector.getBeanInfo(beanClass)
      val pairSeq: Seq[(Tree, TermName, Tree)] = for {
        property @ q"($keySeq(..$keyPath), $valueSeq(..$values))" <- properties
      } yield {
        def defaultResult = (q"???", TermName("<error>"), q"???")
        keyPath match {
          case Seq() =>
            // Default properties
            findDefaultProperty(beanClass, beanInfo) match {
              case None =>
                c.error(property.pos, s"No default property found in ${beanInfo.getBeanDescriptor.getName}")
                defaultResult
              case Some(descriptor) =>
                // TODO
                ???
            }
          case prefix :+ (lastProperty @ Literal(Constant(lastPropertyName: String))) =>
            val valueName = TermName(c.freshName(lastPropertyName))
            def defaultResult = (q"???", valueName, q"???")
            val (resolvedClass, resolvedInfo, resolvedBean) = resolve(beanClass, beanInfo, beanId, prefix)
            if (classOf[java.util.Map[_, _]].isAssignableFrom(resolvedClass)) {
              values match {
                case Seq(value) =>
                  // TODO
                  (value, valueName, q"$resolvedBean.put($lastPropertyName, $valueName)")
                case _ =>
                  values.filterNot(EmptyBinding.unapply) match {
                    case Seq(value) =>
                      // TODO
                      ???
                    case _ =>
                      c.error(lastProperty.pos, "An attribute for java.util.Map must have extractly one value.")
                      defaultResult
                  }
              }

            } else {
              resolvedInfo.getPropertyDescriptors.find(_.getName == lastPropertyName) match {
                case Some(descriptor) =>
                  ???
                case None =>
                  c.error(lastProperty.pos,
                          s"$lastPropertyName is not a property of ${resolvedInfo.getBeanDescriptor.getName}")
                  defaultResult
              }
            }
          // TODO: onChange
        }

      }
      val (bindings, names, setters) = pairSeq.unzip3
      if (bindings.isEmpty) {
        q"_root_.com.thoughtworks.binding.Binding.Constant(${c.prefix}.constructor().build())"
      } else {
        val applyN = mapMethodName(bindings.length)
        val argumentDefinitions = for (name <- names) yield {
          q"val $name = $EmptyTree"
        }
        q"""
          _root_.com.thoughtworks.binding.Binding.typeClass.$applyN(..$bindings)({ ..$argumentDefinitions =>
            val $builderName = ${c.prefix}.constructor()
            ..$setters
            $builderName.build()
          })
        """
      }
    }

    def buildJavaBean[Bean: WeakTypeTag](initializer: Tree): Tree = {
      val q"{ ${valDef: ValDef} => $seq(..$properties) }" = initializer
      val beanId = Ident(valDef.symbol)
      val beanType = weakTypeOf[Bean]
      val beanClass = Class.forName(beanType.typeSymbol.fullName)
      val beanInfo = Introspector.getBeanInfo(beanClass)
      val attributeBindings: Seq[Tree] = for {
        property @ q"($keySeq(..$keyPath), $valueSeq(..$values))" <- properties
      } yield {
        keyPath match {
          case Seq() =>
            // Default properties
            findDefaultProperty(beanClass, beanInfo) match {
              case None =>
                c.error(property.pos, s"No default property found in ${beanInfo.getBeanDescriptor.getName}")
                q"???"
              case Some(descriptor) =>
                bindPropertyFromDescriptor(beanId, descriptor, values)
            }
          case prefix :+ (lastProperty @ Literal(Constant(lastPropertyName: String))) =>
            val (resolvedClass, resolvedInfo, resolvedBean) = resolve(beanClass, beanInfo, beanId, prefix)
            if (classOf[java.util.Map[_, _]].isAssignableFrom(resolvedClass)) {
              def put(binding: Tree) = {
                map(binding) { value =>
                  q"$resolvedBean.put($lastPropertyName, $value)"
                }
              }
              values match {
                case Seq(value) =>
                  put(value)
                case _ =>
                  values.filterNot(EmptyBinding.unapply) match {
                    case Seq(value) =>
                      put(value)
                    case _ =>
                      c.error(lastProperty.pos, "An attribute for java.util.Map must have extractly one value.")
                      q"???"
                  }
              }

            } else {
              resolvedInfo.getPropertyDescriptors.find(_.getName == lastPropertyName) match {
                case Some(descriptor) =>
                  bindPropertyFromDescriptor(resolvedBean, descriptor, values)
                case None =>
                  c.error(lastProperty.pos,
                          s"$lastPropertyName is not a property of ${resolvedInfo.getBeanDescriptor.getName}")
                  q"???"
              }
            }
          // TODO: onChange

        }
      }
      val binding = if (attributeBindings.isEmpty) {
        q"_root_.com.thoughtworks.binding.Binding.Constant($beanId)"
      } else {
        val allBindingUnits = attributeBindings.reduce { (left, right) =>
          q"_root_.com.thoughtworks.binding.fxml.Runtime.bindingUnitSemigroup.append($left, $right)"
        }
        q"_root_.com.thoughtworks.binding.Binding.typeClass.map($allBindingUnits)({ _: _root_.scala.Unit => $beanId })"
      }

      q"""
        ${ValDef(NoMods, valDef.name, valDef.tpt, q"${c.prefix}.constructor()").setSymbol(valDef.symbol)}
        $binding
      """
    }

    def macroTransform(annottees: Tree*): Tree = {
      val transformer = new ComprehensionTransformer {
        private def transformChildren(children: List[Tree], skipEmptyText: Boolean) = {
          children match {
            case Seq(tree @ Text(singleText)) =>
              (Queue.empty, Queue.empty, Queue(atPos(tree.pos)(q"${Binding.Constant(singleText)}")))
            case _ =>
              @tailrec
              def loop(children: List[Tree],
                       accumulatedDefinitions: Queue[Tree],
                       accumulatedPropertyBindings: Queue[(Seq[String], Position, Seq[Tree])],
                       accumulatedDefaultBindings: Queue[Tree])
                : (Queue[Tree], Queue[(Seq[String], Position, Seq[Tree])], Queue[Tree]) = {
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
                                       attributes,
                                       _,
                                       transformNodeSeq.extract(defs: Seq[Tree], transformedValues: Seq[Tree]))
                          if propertyName.charAt(0).isLower =>
                        val (attributeDefs: Queue[Tree], transformedAttributes: Seq[(Seq[String], Tree)]) =
                          transformAttributes(attributes)
                        val nestedAttrbutesBindings: Seq[(Seq[String], Position, Seq[Tree])] =
                          for ((key, value) <- transformedAttributes) yield {
                            (propertyName +: key, tree.pos, Seq(value))
                          }
                        loop(
                          tail,
                          accumulatedDefinitions ++ attributeDefs ++ defs,
                          (accumulatedPropertyBindings ++ nestedAttrbutesBindings).enqueue(
                            (Seq(propertyName), tree.pos, transformedValues)),
                          accumulatedDefaultBindings
                        )
                      case tree =>
                        loop(
                          tail,
                          accumulatedDefinitions,
                          accumulatedPropertyBindings,
                          accumulatedDefaultBindings.enqueue(
                            q"_root_.com.thoughtworks.binding.Binding(${super.transform(tree)})")
                        )
                    }
                }
              }
              loop(children, Queue.empty, Queue.empty, Queue.empty)
          }
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

        private def transformAttributeValue(attributeValue: Tree): (Seq[Tree], Tree) = {
          attributeValue match {
            case TextAttribute(textValue) =>
              Nil -> atPos(attributeValue.pos)(q"${Binding.Constant(textValue)}")
            case _ =>
              Nil -> atPos(attributeValue.pos)(q"_root_.com.thoughtworks.binding.Binding($attributeValue)")
          }
        }

        private def transformAttributes(attributes: List[(QName, Tree)]): (Queue[Tree], Queue[(Seq[String], Tree)]) = {
          @tailrec
          def loop(attributes: List[(QName, Tree)],
                   accumulatedDefinitions: Queue[Tree],
                   accumulatedPairs: Queue[(Seq[String], Tree)]): (Queue[Tree], Queue[(Seq[String], Tree)]) = {
            attributes match {
              case Nil =>
                (accumulatedDefinitions, accumulatedPairs)
              case (key, value) :: tail =>
                val (attributeDefinitions, transformedAttributeValue) = transformAttributeValue(value)
                key match {
                  case UnprefixedName(attributeName) =>
                    loop(
                      tail,
                      accumulatedDefinitions ++ attributeDefinitions,
                      accumulatedPairs.enqueue((Seq(attributeName), transformedAttributeValue))
                    )
                  case _ =>
                    // TODO: support prefixed attributes
                    c.error(value.pos, "attributes should not be prefixed")
                    loop(tail, accumulatedDefinitions, accumulatedPairs)
                }
            }
          }
          loop(attributes, Queue.empty, Queue.empty)
        }

        private def transformNodeSeq: PartialFunction[List[Tree], (Seq[Tree], Seq[Tree])] = {
          case Seq(tree @ Text(singleText)) =>
            Nil -> Seq(atPos(tree.pos)(q"${Binding.Constant(singleText)}"))
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
                    case Text(Macros.Spaces()) =>
                      loop(tail, accumulatedDefinitions, accumulatedBindings)
                    case transformImport.extract(transformedImport) =>
                      loop(tail, accumulatedDefinitions.enqueue(transformedImport), accumulatedBindings)
                    case transformNode.extract(defs, transformedValue) =>
                      loop(tail, accumulatedDefinitions ++ defs, accumulatedBindings.enqueue(transformedValue))
                    case tree =>
                      loop(
                        tail,
                        accumulatedDefinitions,
                        accumulatedBindings.enqueue(
                          q"_root_.com.thoughtworks.binding.Binding(${super.transform(tree)})")
                      )

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
                Nil -> q"???"
              case (None, None) =>
                val elementName = fxIdOption match {
                  case None =>
                    TermName(c.freshName(className))
                  case Some(id) =>
                    TermName(id)
                }
                val (
                  childrenDefinitions: Queue[Tree],
                  childrenProperties: Queue[(Seq[String], Position, Seq[Tree])],
                  defaultProperties: Queue[Tree]
                ) = transformChildren(children, skipEmptyText = false)
                val typeName = TypeName(className)
                val (attributeDefs, attributesPairs) = transformAttributes(otherAttributes)
                val attributesParameter: Queue[Tree] = for ((key, value) <- attributesPairs) yield {
                  atPos(value.pos) {
                    q"""($key, ${lift(Seq(value))})"""
                  }
                }
                val propertiesParameter: Seq[Tree] = for {
                  (name, pos, values) <- childrenProperties
                } yield {
                  atPos(pos) {
                    q"""($name, $values)"""
                  }
                }
                val defaultPropertiesParameter = if (defaultProperties.isEmpty) {
                  Nil
                } else {
                  Seq(q"(${Seq.empty[String]}, $defaultProperties)")
                }
                val build = attributesParameter ++ propertiesParameter ++ defaultPropertiesParameter
                val binding = atPos(tree.pos) {
                  val builderName = TermName(c.freshName("builder"))
                  val f = fxIdOption match {
                    case None =>
                      q"{ _: $typeName => $build }"
                    case Some(id) =>
                      q"{ ${TermName(id)}: $typeName => $build }"
                  }
                  q"""
                    val $builderName = _root_.com.thoughtworks.binding.fxml.Runtime.Builder[$typeName]
                    $builderName.build($f)
                  """
                }
                fxIdOption match {
                  case None =>
                    (attributeDefs ++ childrenDefinitions) -> binding
                  case Some(id) =>
                    val bindingName = TermName(s"${elementName.decodedName}$$binding")
                    val bindingDef = atPos(tree.pos) {
                      q"val $bindingName = $binding"
                    }
                    val macroName = TermName(c.freshName("AutoBind"))
                    val autoBindDef = atPos(tree.pos) {
                      q"object $macroName { def $elementName: _root_.scala.Any = macro _root_.com.thoughtworks.binding.fxml.Runtime.autoBind }"
                    }
                    val autoBindImport = q"import $macroName.$elementName"
                    (attributeDefs ++ childrenDefinitions)
                      .enqueue(bindingDef)
                      .enqueue(autoBindDef)
                      .enqueue(autoBindImport) -> atPos(tree.pos) {
                      q"$bindingName"
                    }
                }
              case (Some(EmptyAttribute()), None) =>
                c.error(tree.pos, "fx:factory must not be empty.")
                Nil -> q"???"
              case (Some(Text(fxFactory)), None) =>
                transformChildren(children, skipEmptyText = true) match {
                  case (childrenDefinitions, Queue(), defaultProperties) =>
                    val elementName = fxIdOption match {
                      case None =>
                        TermName(c.freshName(className))
                      case Some(id) =>
                        TermName(id)
                    }
                    val bindingName = TermName(s"${elementName.decodedName}$$binding")
                    def bindingDef = {
                      val factoryArgumentNames = for (i <- defaultProperties.indices) yield {
                        TermName(c.freshName(s"fxFactoryArgument$i"))
                      }
                      val factoryArguments = for (name <- factoryArgumentNames) yield {
                        q"val $name = $EmptyTree"
                      }
                      if (defaultProperties.isEmpty) {
                        q"_root_.com.thoughtworks.binding.Binding.Constant(${TermName(className)}.${TermName(fxFactory)}())"
                      } else {
                        // TODO: Support more than 12 parameters by generate more sophisticated code
                        val applyN = mapMethodName(defaultProperties.length)
                        q"""
                          val $bindingName = _root_.com.thoughtworks.binding.Binding.BindingInstances.$applyN(..$defaultProperties)({ ..$factoryArguments =>
                            ${TermName(className)}.${TermName(fxFactory)}(..$factoryArgumentNames)
                          })
                        """
                      }
                    }
                    val defs = if (fxIdOption.isDefined) {
                      val autoBindDef = atPos(tree.pos) {
                        q"def $elementName: _root_.scala.Any = macro _root_.com.thoughtworks.binding.fxml.Runtime.autoBind"
                      }
                      childrenDefinitions.enqueue(bindingDef).enqueue(autoBindDef)
                    } else {
                      childrenDefinitions.enqueue(bindingDef)
                    }
                    defs -> atPos(tree.pos)(q"$bindingName")
                  case (_, (_, pos, _) +: _, _) =>
                    c.error(pos, "fx:factory must not contain named property")
                    Nil -> q"???"
                }
              case (None, Some(TextAttribute(fxValue))) =>
                fxIdOption match {
                  case None =>
                    Nil -> atPos(tree.pos) {
                      q"_root_.com.thoughtworks.binding.Binding.Constant(${TermName(className)}.valueOf($fxValue))"
                    }
                  case Some(fxId) =>
                    val idDef = atPos(tree.pos) {
                      q"val ${TermName(fxId)} = ${TermName(className)}.valueOf($fxValue)"
                    }
                    Queue(idDef) -> atPos(tree.pos) {
                      q"_root_.com.thoughtworks.binding.Binding.Constant(${TermName(fxId)})"
                    }
                }
            }

          case tree @ NodeBuffer(transformNodeSeq.extract(defs, values)) =>
            defs -> atPos(tree.pos) {
              values match {
                case Seq() =>
                  q"_root_.com.thoughtworks.binding.Binding.Constants()"
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
            Nil -> q"???"
          case tree @ Elem(PrefixedName("fx", "reference"), attributes, _, children) =>
            c.error(tree.pos, "fx:reference is not supported yet.")
            Nil -> q"???"
          case tree @ Elem(PrefixedName("fx", "copy"), attributes, _, children) =>
            c.error(tree.pos, "fx:copy is not supported yet.")
            Nil -> q"???"
          case tree @ Elem(PrefixedName("fx", "root"), attributes, _, children) =>
            c.error(tree.pos, "fx:root is not supported yet.")
            Nil -> q"???"
        }

        override def transform(tree: Tree): Tree = {
          tree match {
            case transformNode.extract(defs, transformedValue) =>
              val xmlScopeName = TypeName(c.freshName("XmlScope"))
              val rootName = TermName(c.freshName("root"))

              q"""
                import _root_.scala.language.experimental.macros
                final class $xmlScopeName {
                  ..$defs
                  def $rootName = $transformedValue
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
