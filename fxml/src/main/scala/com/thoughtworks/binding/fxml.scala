package com.thoughtworks
package binding

import java.beans
import java.beans.{BeanInfo, Introspector, PropertyDescriptor}
import javafx.application.Platform
import javafx.beans.DefaultProperty
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.event._
import javafx.collections._
import javafx.fxml.JavaFXBuilderFactory
import javafx.scene.Scene
import javafx.stage.{PopupWindow, Stage, Window, WindowEvent}
import javax.swing.SwingUtilities

import com.thoughtworks.binding.Binding.{BindingSeq, Constants, MultiMountPoint, SingleMountPoint, SingletonBindingSeq}
import com.thoughtworks.binding.XmlExtractor._
import com.thoughtworks.Extractor._
import com.thoughtworks.sde.core.Preprocessor
import macrocompat.bundle

import scala.annotation.{StaticAnnotation, compileTimeOnly, implicitNotFound, tailrec}
import scala.collection.{GenSeq, mutable}
import scala.collection.immutable.Queue
import scala.collection.JavaConverters._
import scala.language.experimental.macros
import scala.language.implicitConversions
import scalaz.Semigroup
import scalaz.syntax.all._
import scala.language.dynamics

/** An annotation to convert FXML literals to JavaFX GUI.
  *
  * @note The FXML support is still experimental.
  *       API or behavior of this annotation may change without bumping a majoy version number.
  * @see [[https://github.com/ThoughtWorksInc/Binding.scala/wiki/FXML]] for usage
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class fxml extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro fxml.Macros.macroTransform
}

object fxml {

  @enableIf(c => !c.compilerSettings.exists(_.matches("""^-Xplugin:.*scalajs-compiler_[0-9\.\-]*\.jar$""")))
  private def screenMountPoint[W <: Window](windowBinding: Binding[W])(show: W => Unit) = {
    var shownWindow: Option[W] = None
    lazy val unwatchHandler: EventHandler[WindowEvent] = new EventHandler[WindowEvent] {
      override def handle(event: WindowEvent): Unit = {
        event.getSource.asInstanceOf[W].removeEventHandler(WindowEvent.WINDOW_HIDDEN, this)
        mountPoint.unwatch()
      }
    }
    lazy val mountPoint: Binding[Unit] = Binding {
      val currentWindow = windowBinding.bind
      shownWindow match {
        case None =>
        case Some(originalWindow) =>
          originalWindow.removeEventHandler(WindowEvent.WINDOW_HIDDEN, unwatchHandler)
          originalWindow.hide()
      }
      shownWindow = Some(currentWindow)
      currentWindow.addEventHandler(WindowEvent.WINDOW_HIDDEN, unwatchHandler)
      show(currentWindow)
    }
    mountPoint
  }

  /**
    * [[com.thoughtworks.binding.Binding#watch Watch]]es the value of `sceneBinding`, renders it into `parent` and shows `parent`.
    *
    * @note `sceneBinding` will be automatically [[com.thoughtworks.binding.Binding#unwatch unwatch]]ed when `parent` is closed or hidden.
    */
  @enableIf(c => !c.compilerSettings.exists(_.matches("""^-Xplugin:.*scalajs-compiler_[0-9\.\-]*\.jar$""")))
  def show(parent: Stage, sceneBinding: Binding[Scene]): Unit = {
    lazy val handler = new EventHandler[WindowEvent] with ChangeListener[Scene] {
      private def cleanUp() = {
        parent.removeEventHandler(WindowEvent.WINDOW_HIDDEN, this)
        parent.sceneProperty.removeListener(this)
        mountPoint.unwatch()

      }
      override def handle(event: WindowEvent): Unit = {
        cleanUp()
      }

      override def changed(observable: ObservableValue[_ <: Scene], oldValue: Scene, newValue: Scene): Unit = {
        cleanUp()
      }
    }
    lazy val mountPoint: Binding[Unit] = Binding {
      val scene = sceneBinding.bind
      parent.sceneProperty.removeListener(handler)
      parent.removeEventHandler(WindowEvent.WINDOW_HIDDEN, handler)
      parent.setScene(scene)
      parent.sceneProperty.addListener(handler)
      parent.addEventHandler(WindowEvent.WINDOW_HIDDEN, handler)
      parent.show()
    }
    mountPoint.watch()
  }

  /**
    * [[com.thoughtworks.binding.Binding#watch Watch]]es the value of `popupWindowBinding` and shows it as a pop-up window onto `parent`.
    *
    * @note `popupWindowBinding` will be automatically [[com.thoughtworks.binding.Binding#unwatch unwatch]]ed when being closed or hidden.
    */
  @enableIf(c => !c.compilerSettings.exists(_.matches("""^-Xplugin:.*scalajs-compiler_[0-9\.\-]*\.jar$""")))
  def show(parent: Window, popupWindowBinding: Binding[PopupWindow]): Unit = {
    screenMountPoint(popupWindowBinding)(_.show(parent)).watch()
  }

  /**
    * [[com.thoughtworks.binding.Binding#watch Watch]]es the value of `stageBinding` and shows it on the screen.
    *
    * @note `stageBinding` will be automatically [[com.thoughtworks.binding.Binding#unwatch unwatch]]ed when being closed or hidden.
    */
  @enableIf(c => !c.compilerSettings.exists(_.matches("""^-Xplugin:.*scalajs-compiler_[0-9\.\-]*\.jar$""")))
  def show(stageBinding: Binding[Stage]): Unit = {
    screenMountPoint(stageBinding)(_.show()).watch()
  }

  object AutoImports {

    implicit final class FunctionEventHandler[E <: Event](f: E => Unit) extends EventHandler[E] {
      override def handle(event: E): Unit = f(event)
    }

    implicit final def functionBindingToEventHandlerBinding[E <: Event](
        binding: Binding[E => Unit]): Binding[FunctionEventHandler[E]] = {
      binding.map(new FunctionEventHandler[E](_))
    }

    implicit final class FunctionChangeListener[E](f: (ObservableValue[_ <: E], E, E) => Unit)
        extends ChangeListener[E] {
      override def changed(c: ObservableValue[_ <: E], oldValue: E, newValue: E): Unit = f(c, oldValue, newValue)
    }

    implicit final def functionBindingToListListenerBinding[E](
        binding: Binding[(ObservableValue[_ <: E], E, E) => Unit]): Binding[FunctionChangeListener[E]] = {
      binding.map(new FunctionChangeListener[E](_))
    }

    implicit final class FunctionListChangeListener[E](f: ListChangeListener.Change[_ <: E] => Unit)
        extends ListChangeListener[E] {
      override def onChanged(c: ListChangeListener.Change[_ <: E]): Unit = f(c)
    }

    implicit final def functionBindingToListChangeListenerBinding[E](
        binding: Binding[ListChangeListener.Change[_ <: E] => Unit]): Binding[FunctionListChangeListener[E]] = {
      binding.map(new FunctionListChangeListener[E](_))
    }

    implicit final class FunctionArrayChangeListener[T <: ObservableArray[T]](f: (T, Boolean, Int, Int) => Unit)
        extends ArrayChangeListener[T] {
      override def onChanged(observableArray: T, sizeChanged: Boolean, from: Int, to: Int): Unit =
        f(observableArray, sizeChanged, from, to)
    }

    implicit final def functionBindingToArrayChangeListenerBinding[T <: ObservableArray[T]](
        binding: Binding[(T, Boolean, Int, Int) => Unit]): Binding[FunctionArrayChangeListener[T]] = {
      binding.map(new FunctionArrayChangeListener[T](_))
    }

    implicit final class FunctionSetChangeListener[E](f: SetChangeListener.Change[_ <: E] => Unit)
        extends SetChangeListener[E] {
      override def onChanged(c: SetChangeListener.Change[_ <: E]): Unit = f(c)
    }

    implicit final def functionBindingToSetChangeListenerBinding[E](
        binding: Binding[SetChangeListener.Change[_ <: E] => Unit]): Binding[FunctionSetChangeListener[E]] = {
      binding.map(new FunctionSetChangeListener[E](_))
    }

    implicit final class FunctionMapChangeListener[K, V](f: MapChangeListener.Change[_ <: K, _ <: V] => Unit)
        extends MapChangeListener[K, V] {
      override def onChanged(c: MapChangeListener.Change[_ <: K, _ <: V]): Unit = f(c)
    }

    implicit final def functionBindingToMapChangeListenerBinding[K, V](
        binding: Binding[MapChangeListener.Change[_ <: K, _ <: V] => Unit])
      : Binding[FunctionMapChangeListener[K, V]] = {
      binding.map(new FunctionMapChangeListener[K, V](_))
    }

  }

  object Runtime {

    trait Listen[-Source] {
      type Listener
      def addListener(source: Source, listener: Listener): Unit
      def removeListener(source: Source, listener: Listener): Unit
    }

    object Listen {

      type Aux[-Source, -Listener0] = Listen[Source] {
        type Listener >: Listener0
      }

      implicit def ValueListen[Value]: Listen.Aux[ObservableValue[_ <: Value], ChangeListener[_ >: Value]] = {
        new Listen[ObservableValue[_ <: Value]] {
          override type Listener = ChangeListener[_ >: Value]
          override def addListener(source: ObservableValue[_ <: Value], listener: ChangeListener[_ >: Value]): Unit = {
            source.addListener(listener)
          }

          override def removeListener(source: ObservableValue[_ <: Value],
                                      listener: ChangeListener[_ >: Value]): Unit = {
            source.removeListener(listener)
          }
        }
      }

      implicit def MapListen[Key, Value]
        : Listen.Aux[ObservableMap[_ <: Key, _ <: Value], MapChangeListener[_ >: Key, _ >: Value]] = {
        new Listen[ObservableMap[_ <: Key, _ <: Value]] {
          override type Listener = MapChangeListener[_ >: Key, _ >: Value]
          override def addListener(source: ObservableMap[_ <: Key, _ <: Value],
                                   listener: MapChangeListener[_ >: Key, _ >: Value]): Unit = {
            source.addListener(listener)
          }

          override def removeListener(source: ObservableMap[_ <: Key, _ <: Value],
                                      listener: MapChangeListener[_ >: Key, _ >: Value]): Unit = {
            source.removeListener(listener)
          }
        }
      }

      implicit def SetListen[Element]: Listen.Aux[ObservableSet[_ <: Element], SetChangeListener[_ >: Element]] = {
        new Listen[ObservableSet[_ <: Element]] {
          override type Listener = SetChangeListener[_ >: Element]
          override def addListener(source: ObservableSet[_ <: Element],
                                   listener: SetChangeListener[_ >: Element]): Unit = {
            source.addListener(listener)
          }

          override def removeListener(source: ObservableSet[_ <: Element],
                                      listener: SetChangeListener[_ >: Element]): Unit = {
            source.removeListener(listener)
          }
        }
      }

      implicit def ArrayListen[T <: ObservableArray[T]]: Listen.Aux[ObservableArray[T], ArrayChangeListener[T]] = {
        new Listen[ObservableArray[T]] {
          override type Listener = ArrayChangeListener[T]
          override def addListener(source: ObservableArray[T], listener: Listener): Unit = {
            source.addListener(listener)
          }

          override def removeListener(source: ObservableArray[T], listener: Listener): Unit = {
            source.removeListener(listener)
          }
        }
      }

      implicit def ListListen[Element]: Listen.Aux[ObservableList[_ <: Element], ListChangeListener[_ >: Element]] = {
        new Listen[ObservableList[_ <: Element]] {
          override type Listener = ListChangeListener[_ >: Element]
          override def addListener(source: ObservableList[_ <: Element],
                                   listener: ListChangeListener[_ >: Element]): Unit = {
            source.addListener(listener)
          }

          override def removeListener(source: ObservableList[_ <: Element],
                                      listener: ListChangeListener[_ >: Element]): Unit = {
            source.removeListener(listener)
          }
        }
      }
    }

    def listenMountPoint[Source](source: Source)(
        implicit listen: Listen[Source]): Binding[listen.Listener] => ListenMountPoint[Source, listen.Listener] = {
      ListenMountPoint[Source, listen.Listener](source, _)(listen)
    }

    final case class ListenMountPoint[Source, Listener](source: Source, binding: Binding[Listener])(
        implicit listen: Listen.Aux[Source, Listener])
        extends SingleMountPoint[Listener](binding) {
      var lastListenerOption: Option[Listener] = None

      override protected def set(value: Listener): Unit = {
        lastListenerOption.foreach(listen.removeListener(source, _))
        lastListenerOption = Some(value)
        listen.addListener(source, value)
      }
      override protected def unmount(): Unit = {
        lastListenerOption.foreach(listen.removeListener(source, _))
        lastListenerOption = None
        super.unmount()
      }
    }

    def mountPoint(parent: AnyRef, propertyName: String with Singleton)(
        implicit mountPointFactory: MountPointFactory[parent.type, propertyName.type]): mountPointFactory.Out =
      mountPointFactory(parent, propertyName)

    @implicitNotFound(msg = "${PropertyName} is not a valid property")
    trait MountPointFactory[Parent, PropertyName <: String with Singleton] {
      type Out

      def apply(parent: Parent, propertyName: PropertyName): Out
    }

    object MountPointFactory {
      import scala.language.dynamics

      type Aux[Parent, PropertyName <: String with Singleton, Out0] = MountPointFactory[Parent, PropertyName] {
        type Out = Out0
      }

      implicit final class FunctionMountPointFactory[Parent, PropertyName <: String with Singleton, Out0](
          underlying: (Parent, PropertyName) => Out0)
          extends MountPointFactory[Parent, PropertyName] { this: MountPointFactory.Aux[Parent, PropertyName, Out0] =>
        type Out = Out0

        override def apply(parent: Parent, propertyName: PropertyName) = underlying(parent, propertyName)
      }

      implicit def onChangeMountPointFactory[Parent, PropertyName <: String with Singleton]: MountPointFactory[
        Parent,
        PropertyName] = macro Macros.onChangeMountPointFactory[Parent, PropertyName]

    }

    val bindingUnitSemigroup: Semigroup[Binding[Unit]] = {
      implicit val unitSemigroup: Semigroup[Unit] = Semigroup.instance((_, _) => ())
      Semigroup.liftSemigroup
    }

    val bindingAnySemigroup: Semigroup[Binding[Any]] = {
      implicit val unitSemigroup: Semigroup[Any] = Semigroup.instance((_, _) => ())
      Semigroup.liftSemigroup
    }

    val bindingStringSemigroup: Semigroup[Binding[String]] = {
      import scalaz.std.string._
      Semigroup.liftSemigroup
    }

    trait ToBindingSeq[OneOrMany] { outer =>
      type Element
      def toBindingSeq(binding: Binding[OneOrMany]): BindingSeq[Element]
      def toBindingSeqBinding(binding: Binding[OneOrMany]): Binding[BindingSeq[Element]] = {
        Binding.Constant(toBindingSeq(binding))
      }
      final def compose[A](f: Binding[A] => Binding[OneOrMany]): ToBindingSeq.Aux[A, Element] = new ToBindingSeq[A] {
        override type Element = outer.Element
        override final def toBindingSeq(binding: Binding[A]) = outer.toBindingSeq(f(binding))
        override final def toBindingSeqBinding(binding: Binding[A]) = outer.toBindingSeqBinding(f(binding))
      }
    }

    trait ToBindingSeqId[Element0] extends ToBindingSeq[Element0]

    object ToBindingSeqId {

      type Aux[OneOrMany, Element0] = ToBindingSeqId[OneOrMany] {
        type Element = Element0
      }
      implicit final def fromSingleElement[Element0, Element2 >: Element0]: ToBindingSeqId.Aux[Element0, Element2] =
        new ToBindingSeqId[Element0] {
          override type Element = Element2

          override final def toBindingSeq(binding: Binding[Element0]): SingletonBindingSeq[Element2] = {
            SingletonBindingSeq[Element2](binding)
          }
        }
    }

    private[Runtime] trait LowPriorityToBindingSeq1 {

      implicit final def fromSingleElement[From, Element0](
          implicit toBindingSeqId: ToBindingSeqId.Aux[From, Element0]): ToBindingSeq.Aux[From, Element0] =
        toBindingSeqId
    }

    private[Runtime] trait LowPriorityToBindingSeq0 extends LowPriorityToBindingSeq1 {

      private[Runtime] type InvariantBindingBindingSeq[E] = Binding[BindingSeq[E]]

      implicit def fromBindingSeq[From, Element0](
          implicit constraint: Binding[From] <:< InvariantBindingBindingSeq[Element0]
      ): ToBindingSeq.Aux[From, Element0] = {
        new ToBindingSeq[From] {
          override type Element = Element0
          override def toBindingSeq(from: Binding[From]): BindingSeq[Element] = {
            (from: Binding[BindingSeq[Element0]]) match {
              case Binding.Constant(bindingSeq) => bindingSeq
              case binding                      => Constants(binding).flatMapBinding(identity)
            }
          }

          override def toBindingSeqBinding(binding: Binding[From]) = binding
        }
      }
    }

    object ToBindingSeq extends LowPriorityToBindingSeq0 {
      type Aux[OneOrMany, Element0] = ToBindingSeq[OneOrMany] {
        type Element = Element0
      }

      def apply[OneOrMany](implicit toBindingSeq: ToBindingSeq[OneOrMany]): toBindingSeq.type = toBindingSeq

      private[Runtime] type InvariantBindingBindingSeqBinding[E] = Binding[BindingSeq[Binding[E]]]

      implicit def fromBindingBindingSeq[From, Element0](
          implicit constraint: Binding[From] <:< InvariantBindingBindingSeqBinding[Element0]
      ): ToBindingSeq.Aux[From, Element0] = {
        import scalaz.syntax.all._
        new ToBindingSeq[From] {
          override type Element = Element0
          override def toBindingSeq(from: Binding[From]): BindingSeq[Element] = {
            (from: Binding[BindingSeq[Binding[Element]]]) match {
              case Binding.Constant(bindingSeq) =>
                bindingSeq.mapBinding(identity)
              case binding =>
                Constants(binding).flatMapBinding(_.map(_.mapBinding(identity)))
            }
          }

          override def toBindingSeqBinding(from: Binding[From]) = {
            (from: Binding[BindingSeq[Binding[Element]]]).map(_.mapBinding(identity))
          }
        }
      }

      private[Runtime] type InvariantSeq[E] = Seq[E]

      implicit def fromSeq[From, Element](
          implicit constraint: From <:< InvariantSeq[Element]
      ): ToBindingSeq.Aux[From, Element] = {
        import scalaz.syntax.all._
        fromBindingSeq[BindingSeq[Element], Element].compose[From](_.map { seq =>
          Constants(seq: _*)
        })
      }

      implicit def fromJavaList[From, Element](
          implicit constraint: From <:< java.util.List[Element]
      ): ToBindingSeq.Aux[From, Element] = {
        import scalaz.syntax.all._
        fromBindingSeq[BindingSeq[Element], Element].compose[From](_.map { list =>
          Constants(constraint(list).asScala: _*)
        })
      }
      private[Runtime] type InvariantBinding[E] = Binding[E]

      implicit def fromBindingBinding[From, Element](
          implicit constraint: From <:< InvariantBinding[Element]
      ): ToBindingSeq.Aux[From, Element] = {
        import scalaz.syntax.all._
        fromSingleElement[Element, Element].compose(_.flatMap(constraint))
      }

    }

    final def toBindingSeq[OneOrMany](binding: Binding[OneOrMany])(
        implicit typeClass: ToBindingSeq[OneOrMany]): BindingSeq[typeClass.Element] = {
      typeClass.toBindingSeq(binding)
    }

    final def toBindingSeqBinding[OneOrMany](binding: Binding[OneOrMany])(
        implicit typeClass: ToBindingSeq[OneOrMany]): Binding[BindingSeq[typeClass.Element]] = {
      typeClass.toBindingSeqBinding(binding)
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

    final class JavaBeanPropertyTyper[A](implicit val constructor: EmptyConstructor[A]) extends PropertyTyper[A] {

      def resolveProperties(initializer: A => Seq[(Seq[String], Seq[Binding[_]])]): Binding[A] =
        macro Macros.resolvePropertiesForJavaBean[A]

    }

    object JavaFXPropertyTyper {

      final class CurrentJavaFXBuilderFactory(val underlying: JavaFXBuilderFactory)

      object CurrentJavaFXBuilderFactory {

        implicit val defaultJavaFXBuilderFactory = new CurrentJavaFXBuilderFactory(new JavaFXBuilderFactory())

      }

    }
    import JavaFXPropertyTyper._

    final class JavaFXPropertyTyper[A, B](val constructor: () => B) extends PropertyTyper[A] {

      def resolveProperties(initializer: A => Seq[(Seq[String], Seq[Binding[_]])]): Binding[A] =
        macro Macros.resolvePropertiesFromJavaFXBuilder[A, B]

    }

    private[Runtime] sealed trait LowPriorityBuilder {

      implicit final def javaBeanTyper[A](implicit constructor: EmptyConstructor[A]): JavaBeanPropertyTyper[A] = {
        new JavaBeanPropertyTyper
      }

    }

    object PropertyTyper extends LowPriorityBuilder {

      @enableIf(c => !c.compilerSettings.exists(_.matches("""^-Xplugin:.*scalajs-compiler_[0-9\.\-]*\.jar$""")))
      implicit def javafxTyper[A]: PropertyTyper[A] = macro Macros.javafxTyper[A]

      def apply[Value](implicit typer: PropertyTyper[Value]): typer.type = typer

    }

    @implicitNotFound(msg = "${Value} is not a Java Bean nor a type built from JavaFXBuilderFactory")
    trait PropertyTyper[Value]

    final class JavaListMountPoint[A](javaList: java.util.List[A])(bindingSeq: BindingSeq[A])
        extends MultiMountPoint[A](bindingSeq) {

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
        javaList.addAll(from, that.seq.asJava)
      }
    }

  }

  private object Macros {

    @enableIf(c => !c.compilerSettings.exists(_.matches("""^-Xplugin:.*scalajs-compiler_[0-9\.\-]*\.jar$""")))
    private[Macros] val javafxBuilderFactory = {
      if (!SwingUtilities.isEventDispatchThread) {
        val lock = new AnyRef
        @volatile var initialized = false
        lock.synchronized {
          SwingUtilities.invokeLater(new Runnable {
            override def run(): Unit = {
              new javafx.embed.swing.JFXPanel
              lock.synchronized {
                initialized = true
                lock.notify()
              }
            }
          })
          while (!initialized) {
            lock.wait()
          }
        }
      } else {
        new javafx.embed.swing.JFXPanel
      }

      new JavaFXBuilderFactory()
    }

    private[Macros] val Spaces = """\s*""".r

    private[Macros] val ExpressionBinding = """(?s)\$\{(.*)\}\s*""".r

    private[Macros] val VariableResolution = """(?s)\$(.*)""".r

    private[Macros] val EscapeSequences = """(?s)\\(.*)""".r

    private[Macros] val ResourceResolution = """(?s)%(.*)""".r

    private[Macros] val LocationResolution = """(?s)@(.*)""".r

    private[Macros] val ClassName = """[A-Z][^\.]+""".r

    private[Macros] val StaticProperty = """([^\.]+)\.([^\.]+)""".r

    private[Macros] val OnXxxChange = """on(\w*)Change""".r

  }

  import scala.reflect.macros.whitebox

  @bundle
  private[binding] final class Macros(context: whitebox.Context) extends Preprocessor(context) with XmlExtractor {

    import Macros._
    import c.internal.decorators._
    import c.universe._

    def onChangeMountPointFactory[Parent: WeakTypeTag, PropertyName <: String with Singleton: WeakTypeTag]: Tree = {

      val parentType = weakTypeOf[Parent]
      val propertyNameType = weakTypeOf[PropertyName]
      val ConstantType(Constant(OnXxxChange(propertyPrefix))) = propertyNameType
      val parentName = TermName(c.freshName("parent"))
      if (propertyPrefix == "") {
        q"""new _root_.com.thoughtworks.binding.fxml.Runtime.MountPointFactory.FunctionMountPointFactory({ ($parentName: $parentType, _: $propertyNameType) =>
          _root_.com.thoughtworks.binding.fxml.Runtime.listenMountPoint($parentName)
        })"""
      } else {
        val propertyName = TermName(s"${Introspector.decapitalize(propertyPrefix)}Property")
        q"""new _root_.com.thoughtworks.binding.fxml.Runtime.MountPointFactory.FunctionMountPointFactory({ ($parentName: $parentType, _: $propertyNameType) =>
          _root_.com.thoughtworks.binding.fxml.Runtime.listenMountPoint($parentName.$propertyName)
        })"""
      }
    }

    private implicit def constantLiftable[A: Liftable]: Liftable[Binding.Constant[A]] =
      new Liftable[Binding.Constant[A]] {
        override def apply(value: Binding.Constant[A]): Tree = {
          q"_root_.com.thoughtworks.binding.Binding.Constant(..${value.value})"
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

    private def buildFromDescriptor(parentBean: Tree,
                                    descriptor: PropertyDescriptor,
                                    bindings: Seq[Tree]): (Tree, TermName, Tree) = {
      val name = TermName(c.freshName(descriptor.getName))

      descriptor.getWriteMethod match {
        case _ if classOf[java.util.List[_]].isAssignableFrom(descriptor.getPropertyType) =>
          val nonEmptyBindings = bindings.filterNot(EmptyBinding.unapply)
          def list = q"$parentBean.${TermName(descriptor.getReadMethod.getName)}"
          val bindingSeq = nonEmptyBindings match {
            case Seq() =>
              q"_root_.com.thoughtworks.binding.Binding.Constants(())"
            case Seq(binding) =>
              q"_root_.com.thoughtworks.binding.fxml.Runtime.toBindingSeq($binding)"
            case _ =>
              val valueBindings = for (binding <- nonEmptyBindings) yield {
                q"_root_.com.thoughtworks.binding.fxml.Runtime.toBindingSeqBinding($binding)"
              }
              q"_root_.com.thoughtworks.binding.Binding.Constants(..$valueBindings).flatMapBinding(_root_.scala.Predef.locally _)"
          }

          (
            bindingSeq,
            name,
            q"""
              import _root_.scala.collection.JavaConverters._
              $list.addAll($name.asJava)
            """
          )
        case null =>
          c.error(parentBean.pos, s"${descriptor.getName} is not writeable")
          (q"???", TermName("<error>"), q"???")
        case writeMethod =>
          val setterName = TermName(writeMethod.getName)
          if (classOf[String].isAssignableFrom(descriptor.getPropertyType)) {
            bindings match {
              case Seq() =>
                (q"""_root_.com.thoughtworks.binding.Binding.Constant(())""", name, q"()")
              case Seq(value) =>
                (value, name, q"$parentBean.$setterName($name)")
              case nonEmptyBindings =>
                val value = nonEmptyBindings.reduce { (left, right) =>
                  q"_root_.com.thoughtworks.binding.fxml.Runtime.bindingStringSemigroup.append($left, $right)"
                }
                (value, name, q"$parentBean.$setterName($name)")
            }
          } else {
            bindings match {
              case Seq() =>
                c.error(parentBean.pos, s"expect a value for ${descriptor.getName}")
                (q"???", TermName("<error>"), q"???")
              case Seq(value) =>
                (value, name, q"$parentBean.$setterName($name)")
              case _ =>
                c.error(parentBean.pos, s"expect only one value for ${descriptor.getName}")
                (q"???", TermName("<error>"), q"???")
            }

          }
      }
    }

    private def bindPropertyFromDescriptor(parentBean: Tree,
                                           descriptor: PropertyDescriptor,
                                           bindings: Seq[Tree]): Tree = {

      descriptor.getWriteMethod match {
        case _ if classOf[java.util.List[_]].isAssignableFrom(descriptor.getPropertyType) =>
          val nonEmptyBindings = bindings.filterNot(EmptyBinding.unapply)
          def list = q"$parentBean.${TermName(descriptor.getReadMethod.getName)}"
          nonEmptyBindings match {
            case Seq() =>
              q"_root_.com.thoughtworks.binding.Binding.Constant(())"
            case Seq(binding) =>
              q"""
                new _root_.com.thoughtworks.binding.fxml.Runtime.JavaListMountPoint(
                  $list
                )(
                  _root_.com.thoughtworks.binding.fxml.Runtime.toBindingSeq($binding)
                )
              """
            case _ =>
              val valueBindings = for (binding <- nonEmptyBindings) yield {
                q"_root_.com.thoughtworks.binding.fxml.Runtime.toBindingSeqBinding($binding)"
              }
              q"""
                 new _root_.com.thoughtworks.binding.fxml.Runtime.JavaListMountPoint(
                  $list
                )(
                  _root_.com.thoughtworks.binding.Binding.Constants(..$valueBindings).flatMapBinding(_root_.scala.Predef.locally _)
                )
              """
          }
        case null =>
          c.error(parentBean.pos, s"${descriptor.getName} is not writeable")
          q"???"
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
            bindings match {
              case Seq() =>
                c.error(parentBean.pos, s"expect a value for ${descriptor.getName}")
                q"???"
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
    private def resolveGetters(beanClass: Class[_],
                               beanInfo: BeanInfo,
                               bean: Tree,
                               getters: Seq[Tree]): (Class[_], BeanInfo, Tree) = {
      getters match {
        case Seq() =>
          (beanClass, beanInfo, bean)
        case (head @ Literal(Constant(name: String))) +: tail =>
          beanInfo.getPropertyDescriptors.find(_.getName == name) match {
            case None =>
              c.error(head.pos, s"$name is not a property of ${beanInfo.getBeanDescriptor.getName}")
              (beanClass, beanInfo, bean)
            case Some(propertyDescriptor) =>
              val nestedClass = propertyDescriptor.getPropertyType
              val nestedInfo = Introspector.getBeanInfo(nestedClass)
              val nestedBean = atPos(head.pos)(q"$bean.${TermName(propertyDescriptor.getReadMethod.getName)}")
              resolveGetters(nestedClass, nestedInfo, nestedBean, tail)
          }
      }
    }

    private def mapMethodName(numberOfParamters: Int) = {
      numberOfParamters match {
        case 1 => TermName("map")
        case _ => TermName(s"apply$numberOfParamters")
      }
    }

    // FIXME: Use the same logic as JavaBean
    def resolvePropertiesFromJavaFXBuilder[Out: WeakTypeTag, Builder: WeakTypeTag](initializer: Tree): Tree = {

      val outType = weakTypeOf[Out]
      val q"{ $valDef => $seq(..$properties) }" = initializer

      val builderName = TermName(c.freshName("builder"))
      val beanId = q"$builderName"
      val beanType = weakTypeOf[Builder]
      val beanClass = Class.forName(beanType.typeSymbol.fullName)
      val beanInfo = Introspector.getBeanInfo(beanClass)
      val tripleSeq: Seq[(Tree, TermName, Tree)] = for {
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
                buildFromDescriptor(beanId, descriptor, values)
            }
          case prefix :+ (lastProperty @ Literal(Constant(lastPropertyName: String))) =>
            val valueName = TermName(c.freshName(lastPropertyName))
            def defaultResult = (q"???", valueName, q"???")
            val (resolvedClass, resolvedInfo, resolvedBean) = resolveGetters(beanClass, beanInfo, beanId, prefix)
            lastPropertyName match {
              case StaticProperty(classPrefix, propertyName) =>
                val setterName = TermName(s"set${propertyName.capitalize}")
                val className = TermName(classPrefix)
                values match {
                  case Seq() =>
                    c.error(resolvedBean.pos, s"Expect a value for $lastPropertyName")
                    (q"???", TermName("<error>"), q"???")
                  case Seq(value) =>
                    (value, valueName, q"$className.$setterName($resolvedBean, $valueName)")
                  case _ =>
                    c.error(resolvedBean.pos, s"Expect only one value for $lastPropertyName")
                    (q"???", TermName("<error>"), q"???")
                }
              case _ =>
                if (classOf[java.util.Map[_, _]].isAssignableFrom(resolvedClass)) {
                  values match {
                    case Seq(value) =>
                      (value, valueName, q"$resolvedBean.put($lastPropertyName, $valueName)")
                    case _ =>
                      values.filterNot(EmptyBinding.unapply) match {
                        case Seq(value) =>
                          (value, valueName, q"$resolvedBean.put($lastPropertyName, $valueName)")
                        case _ =>
                          c.error(lastProperty.pos, "An attribute for java.util.Map must have extractly one value.")
                          defaultResult
                      }
                  }
                } else {
                  resolvedInfo.getPropertyDescriptors.find(_.getName == lastPropertyName) match {
                    case Some(descriptor) =>
                      buildFromDescriptor(resolvedBean, descriptor, values)
                    case None =>
                      c.error(lastProperty.pos,
                              s"$lastPropertyName is not a property of ${resolvedInfo.getBeanDescriptor.getName}")
                      defaultResult
                  }
                }
            }
        }

      }
      val (bindings, names, setters) = tripleSeq.unzip3
      val result = if (bindings.isEmpty) {
        q"_root_.com.thoughtworks.binding.Binding.Constant(${c.prefix}.constructor().build().asInstanceOf[$outType])"
      } else {
        val applyN = mapMethodName(bindings.length)
        val argumentDefinitions = for (name <- names) yield {
          q"val $name = $EmptyTree"
        }
        q"""
          _root_.com.thoughtworks.binding.Binding.typeClass.$applyN(..$bindings)({ ..$argumentDefinitions =>
            val $builderName: $beanType = ${c.prefix}.constructor()
            ..$setters
            $builderName.build().asInstanceOf[$outType]
          })
        """
      }
      c.untypecheck(result)
    }

    def resolvePropertiesForJavaBean[Bean: WeakTypeTag](initializer: Tree): Tree = {
      val q"{ ${valDef: ValDef} => $seq(..$properties) }" = initializer
      val beanId = Ident(valDef.name)
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
            val (resolvedClass, resolvedInfo, resolvedBean) = resolveGetters(beanClass, beanInfo, beanId, prefix)
            lastPropertyName match {
              case StaticProperty(classPrefix, propertyName) =>
                val setterName = TermName(s"set${propertyName.capitalize}")
                val className = TermName(classPrefix)
                values match {
                  case Seq() =>
                    c.error(resolvedBean.pos, s"Expect a value for $lastPropertyName")
                    q"???"
                  case Seq(binding) =>
                    map(binding) { value =>
                      q"$className.$setterName($resolvedBean, $value)"
                    }
                  case _ =>
                    c.error(resolvedBean.pos, s"Expect only one value for $lastPropertyName")
                    q"???"
                }
              case _ =>
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
                      atPos(property.pos) {
                        q"_root_.com.thoughtworks.binding.fxml.Runtime.mountPoint($resolvedBean, $lastPropertyName).apply(..$values)"
                      }
                  }
                }
            }
        }
      }

      val allBindingUnits = if (attributeBindings.isEmpty) {
        q"_root_.com.thoughtworks.binding.Binding.Constant(())"
      } else {
        attributeBindings.reduce { (left, right) =>
          q"_root_.com.thoughtworks.binding.fxml.Runtime.bindingUnitSemigroup.append($left, $right)"
        }
      }

      val result = q"""
        ${q"val ${valDef.name}: $beanType = ${c.prefix}.constructor()".setSymbol(valDef.symbol)}
        _root_.com.thoughtworks.binding.Binding.typeClass.map($allBindingUnits)({ _: _root_.scala.Unit => $beanId })
      """
//      c.info(c.enclosingPosition, show(result), true)
      c.untypecheck(result)
    }

    @enableIf(c => !c.compilerSettings.exists(_.matches("""^-Xplugin:.*scalajs-compiler_[0-9\.\-]*\.jar$""")))
    def javafxTyper[Out: WeakTypeTag]: Tree = {
      import Runtime.JavaFXPropertyTyper.CurrentJavaFXBuilderFactory
      val currentJavaFXBuilderFactory: Tree = c.inferImplicitValue(typeOf[CurrentJavaFXBuilderFactory])
      val outType = weakTypeOf[Out]
      val outClass: Class[_] = Class.forName(outType.typeSymbol.fullName)
      synchronized(javafxBuilderFactory.getBuilder(outClass)) match {
        case null =>
          c.abort(c.enclosingPosition, s"No javafx.util.PropertyTyper found for $outType")
        case builder =>
          val runtimeSymbol = reflect.runtime.currentMirror.classSymbol(builder.getClass)
          val builderInfo = runtimeSymbol.typeSignature
          val qBundle = new Q.MacroBundle[c.universe.type](c.universe)
          val rawbuilderTypeTree: Tree = qBundle.fullyQualifiedSymbolTreeWithRootPrefix(runtimeSymbol)

          val builderTypeTree = if (builderInfo.takesTypeArgs) {
            val typeNames = for (typeParam <- builderInfo.asInstanceOf[PolyType].typeParams) yield {
              TypeName(c.freshName(typeParam.name.toString))
            }
            val typeIds = for (typeName <- typeNames) yield {
              tq"$typeName"
            }
            val typeDefs = for (typeName <- typeNames) yield {
              q"type $typeName"
            }

            tq"$rawbuilderTypeTree[..$typeIds] forSome { ..$typeDefs }"

          } else {
            rawbuilderTypeTree
          }
          val result = q"""
            new _root_.com.thoughtworks.binding.fxml.Runtime.JavaFXPropertyTyper[$outType, $builderTypeTree]({() =>
              $currentJavaFXBuilderFactory.underlying.getBuilder(_root_.scala.Predef.classOf[$outType]).asInstanceOf[$builderTypeTree]
            })
          """
          c.untypecheck(result)
      }
    }

    private final class XmlTransformer extends ComprehensionTransformer {

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
                    case transformXmlDefinition.extract(transformedDefinitions) =>
                      loop(
                        tail,
                        accumulatedDefinitions ++ transformedDefinitions,
                        accumulatedPropertyBindings,
                        accumulatedDefaultBindings
                      )
                    case transformXmlValue.extract(defs, transformedValue) =>
                      loop(
                        tail,
                        accumulatedDefinitions ++ defs,
                        accumulatedPropertyBindings,
                        accumulatedDefaultBindings.enqueue(transformedValue)
                      )
                    case tree @ Elem(UnprefixedName(propertyName),
                                     attributes,
                                     _,
                                     transformNodeSeq.extract(defs: Seq[Tree], transformedValues: Seq[Tree])) =>
                      val (attributeDefs: Queue[Tree], transformedAttributes: Seq[(Seq[String], Tree)]) =
                        transformAttributes(attributes)
                      val nestedAttrbutesBindings: Seq[(Seq[String], Position, Seq[Tree])] =
                        for ((key, value) <- transformedAttributes) yield {
                          (propertyName +: key, tree.pos, Seq(value))
                        }
                      val propertiesBindings = if (transformedValues.isEmpty) {
                        Nil
                      } else {
                        Seq((Seq(propertyName), tree.pos, transformedValues))
                      }
                      loop(
                        tail,
                        accumulatedDefinitions ++ attributeDefs ++ defs,
                        accumulatedPropertyBindings ++ nestedAttrbutesBindings ++ propertiesBindings,
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

      private def transformXmlDefinition: PartialFunction[Tree, Seq[Tree]] = {
        case Text(Macros.Spaces()) | Comment(_) =>
          Nil
        case tree @ ProcInstr("import", proctext) =>
          Seq(atPos(tree.pos) {
            c.parse(raw"""import $proctext""") match {
              case q"import $parent.*" => q"import $parent._"
              case i => i
            }
          })
        case tree @ Elem(PrefixedName("fx", "define"), attributes, _, children) =>
          attributes match {
            case (_, firstAttributeValue) +: _ =>
              c.error(firstAttributeValue.pos, "fx:definie element must not contain any attributes")
            case _ =>
          }
          val (
            childrenDefinitions: Queue[Tree],
            childrenProperties: Queue[(Seq[String], Position, Seq[Tree])],
            defaultProperties: Queue[Tree]
          ) = transformChildren(children, skipEmptyText = true)
          childrenProperties match {
            case (_, firstPropertyPos, _) +: _ =>
              c.error(firstPropertyPos, "fx:definie element must not contain any properties")
            case _ =>
          }
          childrenDefinitions
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
                  loop(tail,
                       accumulatedDefinitions ++ attributeDefinitions,
                       accumulatedPairs.enqueue((Seq(attributeName), transformedAttributeValue)))
                case PrefixedName(prefix, localPart) =>
                  loop(tail,
                       accumulatedDefinitions ++ attributeDefinitions,
                       accumulatedPairs.enqueue((prefix.split(':').view :+ localPart, transformedAttributeValue)))
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
                  case transformXmlDefinition.extract(transformedDefinitions) =>
                    loop(tail, accumulatedDefinitions ++ transformedDefinitions, accumulatedBindings)
                  case transformXmlValue.extract(defs, transformedValue) =>
                    loop(tail, accumulatedDefinitions ++ defs, accumulatedBindings.enqueue(transformedValue))
                  case tree =>
                    val binding = TermName(c.freshName("binding"))
                    loop(
                      tail,
                      accumulatedDefinitions,
                      accumulatedBindings.enqueue(q"_root_.com.thoughtworks.binding.Binding(${super.transform(tree)})")
                    )

                }
            }
          }
          loop(children, Queue.empty, Queue.empty)
      }

      /**
        * Returns a [[PartialFunction]] that transforms any XML nodes or node sequences to a pair of a definition list tree and a [[Binding]] of the value.
        *
        * Throws a [[MatchError]] if the input is not XML or it is not a value.
        */
      private def transformXmlValue: PartialFunction[Tree, (Seq[Tree], Tree)] = {
        case tree @ Text(data) =>
          Nil -> atPos(tree.pos) {
            q"_root_.com.thoughtworks.binding.Binding.Constant($data)"
          }
        case tree @ EntityRef(XmlEntityName(unescapedText)) =>
          Nil -> atPos(tree.pos) {
            q"_root_.com.thoughtworks.binding.Binding.Constant($unescapedText)"
          }
        case tree @ Elem(UnprefixedName(className @ ClassName()), attributes, _, children) =>
          // Create new instance

          // TODO: <fx:include> (Read external files)
          // TODO: convert fx:value, fx:constant, <fx:reference> and <fx:copy> to @fxml val

          // 不要支持Type Coercion、Location Resolution、Resource Resolution、Variable Resolution、Escape Sequences、Expression Binding，要求用户改用花括号{}以提供类型安全的代码

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
                val f = fxIdOption match {
                  case None =>
                    q"{ _: $typeName => $build }"
                  case Some(id) =>
                    q"{ ${TermName(id)}: $typeName => $build }"
                }
                q"""
                    _root_.com.thoughtworks.binding.fxml.Runtime.PropertyTyper[$typeName].resolveProperties($f)
                  """
              }
              val id = fxIdOption.getOrElse(c.freshName(className))
              val elementName = TermName(id)
              val bindingName = TermName(s"$id$$binding")
              val macroName = TermName(c.freshName(s"$id$$AutoBind"))
              val initializerName = TermName(c.freshName(s"$$initialize$id"))
              val q"..$autoBindDef" = atPos(tree.pos) {
                q"""
                    def $initializerName: _root_.com.thoughtworks.binding.Binding[$typeName] = $binding
                    object $macroName {
                      val $bindingName: _root_.com.thoughtworks.binding.Binding[$typeName] = $initializerName
                      def $elementName: _root_.scala.Any = macro _root_.com.thoughtworks.binding.fxml.Runtime.autoBind
                    }
                  """
              }
              val autoDefImport = atPos(tree.pos)(q"import $macroName.{$bindingName, $elementName}")
              (autoDefImport +: (attributeDefs ++ childrenDefinitions ++ autoBindDef)) -> atPos(tree.pos)(
                q"$bindingName")
            case (Some(EmptyAttribute()), None) =>
              c.error(tree.pos, "fx:factory must not be empty.")
              Nil -> q"???"
            case (Some(Text(fxFactory)), None) =>
              otherAttributes match {
                case (_, firstAttributeValue) +: _ =>
                  c.error(firstAttributeValue.pos,
                          "An element with a fx:factory attribute must not contain other attributes")
                case _ =>
              }
              transformChildren(children, skipEmptyText = true) match {
                case (childrenDefinitions, Queue(), defaultProperties) =>
                  def binding = {
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
                          _root_.com.thoughtworks.binding.Binding.BindingInstances.$applyN(..$defaultProperties)({ ..$factoryArguments =>
                            ${TermName(className)}.${TermName(fxFactory)}(..$factoryArgumentNames)
                          })
                        """
                    }
                  }
                  val id = fxIdOption.getOrElse(c.freshName(className))
                  val elementName = TermName(id)
                  val bindingName = TermName(s"$id$$binding")
                  val macroName = TermName(c.freshName(s"$id$$AutoBind"))
                  val initializerName = TermName(c.freshName(s"$$initialize$id"))
                  val q"..$autoBindDef" = atPos(tree.pos) {
                    q"""
                      def $initializerName = $binding
                      object $macroName {
                        val $bindingName = $initializerName
                        def $elementName: _root_.scala.Any = macro _root_.com.thoughtworks.binding.fxml.Runtime.autoBind
                      }
                    """
                  }
                  val autoDefImport = atPos(tree.pos)(q"import $macroName.{$bindingName, $elementName}")
                  (autoDefImport +: (childrenDefinitions ++ autoBindDef)) -> atPos(tree.pos)(q"$bindingName")
                case (_, (_, pos, _) +: _, _) =>
                  c.error(pos, "fx:factory must not contain named property")
                  Nil -> q"???"
              }
            case (None, Some(value)) =>
              value match {
                case TextAttribute(fxValue) =>
                  c.warning(
                    value.pos,
                    "fx:value is not type safe. Use embedded Scala expression in curly bracket syntax instead of elements with fx:value.")
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
                case _ =>
                  c.error(value.pos, "fx:value attributes do not support embedded Scala expression")
                  Nil -> q"???"
              }
          }

        case tree @ NodeBuffer(transformNodeSeq.extract(defs, values)) =>
          defs -> atPos(tree.pos) {
            values match {
              case Seq() =>
                q"_root_.com.thoughtworks.binding.Binding.Constants(())"
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

      private def expand: PartialFunction[Tree, (Seq[Tree], Tree)] = {
        case tree @ ValDef(mods, name, tpt, transformXmlValue.extract(defs, transformedValue)) => //transformXmlValue.extract(defs, transformedValue)) =>
          (defs, treeCopy.ValDef(tree, mods, name, tpt, transformedValue))
        case tree @ DefDef(mods, name, tparams, vparamss, tpt, transformXmlValue.extract(defs, transformedValue)) =>
          (defs, treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt, transformedValue))
        case transformXmlValue.extract(defs, transformedValue) =>
          (defs, transformedValue)
      }

      private def transformBlock: PartialFunction[Block, Block] = {
        case Block(stats, expr) =>
          val (transformedStats :+ transformedExpr) = (stats :+ expr).flatMap { x =>
            x match {
              case expand.extract(defs, transformedValue) =>
                defs :+ transformedValue
              case b @ transformBlock.extract(transformedBlock) =>
                Seq(transformedBlock)
              case subtree =>
                Seq(super.transform(subtree))
            }
          }
          Block(transformedStats.toList, transformedExpr)
      }

      override def transform(tree: Tree): Tree = {
        tree match {
          case expand.extract(defs, transformedValue) =>
            q"""
              ..$defs
              $transformedValue
            """
          case transformBlock.extract(transformedBlock) =>
            transformedBlock
          case _ =>
            super.transform(tree)
        }
      }

    }

    def macroTransform(annottees: Tree*): Tree = {
      val transformer = new XmlTransformer

      import transformer.transform
//      def transform(tree: Tree): Tree = {
//        val output = transformer.transform(tree)
//        c.info(c.enclosingPosition, c.universe.show(output), true)
//        output
//      }

      replaceDefBody(
        annottees, { body =>
          q"""
            import _root_.scala.language.experimental.macros
            import _root_.com.thoughtworks.binding.fxml.AutoImports.{
              != => _,
              ## => _,
              == => _,
              eq => _,
              equals => _,
              getClass => _,
              hashCode => _,
              ne => _,
              notify => _,
              notifyAll => _,
              synchronized => _,
              toString => _,
              wait => _,
              _
            }
            ${transform(body)}
          """
        }
      )

    }

  }

}
