package com.thoughtworks
package binding

import com.thoughtworks.sde.core.MonadicFactory
import scalaz.Monad
import scalaz.MonadPlus

import scala.collection.SeqOps
import scala.language.existentials
import scala.language.experimental.macros

import Binding.BindingSeq

private[binding] object Binding2Or3 {

  private[binding] type SeqOpsIterable[+A] = Iterable[A] with SeqOps[A, CC, CC[A]] forSome {
    type CC[+A] <: Iterable[A]
  }

  final class Macros(val c: scala.reflect.macros.blackbox.Context) {

    import c.universe._

    lazy val functionOrFunctionLiteral: PartialFunction[Tree, (List[ValDef], Tree)] = {
      case Function(vparams, body) =>
        (vparams, body)
      case f =>
        val elementName = TermName(c.freshName("bindingElement"))
        (List(q"val $elementName: ${TypeTree()} = $EmptyTree"), atPos(f.pos)(q"$f($elementName)"))
    }

    final def foreach(f: Tree): Tree = {
      val apply @ Apply(
        TypeApply(Select(self, TermName("foreach")), List(u)),
        List(f @ functionOrFunctionLiteral(vparams, body))
      ) =
        c.macroApplication
      val monadicBody =
        q"""_root_.com.thoughtworks.binding.Binding.apply[$u]($body)"""
      val monadicFunction = atPos(f.pos)(Function(vparams, monadicBody))
      atPos(apply.pos)(
        q"""_root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[
          _root_.com.thoughtworks.binding.Binding,
          _root_.scala.Unit
        ]($self.foreachBinding[$u]($monadicFunction))"""
      )
    }

    final def map(f: Tree): Tree = {
      val apply @ Apply(
        TypeApply(Select(self, TermName("map")), List(b)),
        List(f @ functionOrFunctionLiteral(vparams, body))
      ) =
        c.macroApplication
      val monadicBody =
        q"""_root_.com.thoughtworks.binding.Binding.apply[$b]($body)"""
      val monadicFunction = atPos(f.pos)(Function(vparams, monadicBody))
      atPos(apply.pos)(q"""$self.mapBinding[$b]($monadicFunction)""")
    }

    final def flatMap(f: Tree): Tree = {
      val apply @ Apply(
        TypeApply(Select(self, TermName("flatMap")), List(b)),
        List(f @ functionOrFunctionLiteral(vparams, body))
      ) =
        c.macroApplication
      val monadicBody =
        q"""_root_.com.thoughtworks.binding.Binding.apply[_root_.com.thoughtworks.binding.Binding.BindingSeq[$b]]($body)"""
      val monadicFunction = atPos(f.pos)(Function(vparams, monadicBody))
      atPos(apply.pos)(q"""$self.flatMapBinding[$b]($monadicFunction)""")
    }

    final def withFilter(condition: Tree): Tree = {
      val apply @ Apply(Select(self, TermName("withFilter")), List(f @ functionOrFunctionLiteral(vparams, body))) =
        c.macroApplication
      val monadicBody =
        q"""_root_.com.thoughtworks.binding.Binding.apply[_root_.scala.Boolean]($body)"""
      val monadicFunction = atPos(f.pos)(Function(vparams, monadicBody))
      atPos(apply.pos)(q"""$self.withFilterBinding($monadicFunction)""")
    }

    final def bind: Tree = {
      val q"$binding.$methodName" = c.macroApplication
      q"""_root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[
        _root_.com.thoughtworks.binding.Binding,
        ${TypeTree(c.macroApplication.tpe)}
      ]($binding)"""
    }

  }

  trait Companion extends MonadicFactory.WithTypeClass[Monad, binding.Binding] { this: binding.Binding.type =>

    /** Monad instances for [[Binding]].
      *
      * @group typeClasses
      */
    implicit object BindingInstances extends Monad[Binding] {

      override def map[A, B](fa: Binding[A])(f: A => B): Binding[B] = {
        fa.map(f)
      }

      override def bind[A, B](fa: Binding[A])(f: A => Binding[B]): Binding[B] = {
        fa.flatMap(f)
      }

      @inline
      override def point[A](a: => A): Binding[A] = Constant(a)

      override def ifM[B](value: Binding[Boolean], ifTrue: => Binding[B], ifFalse: => Binding[B]): Binding[B] = {
        bind(value)(if (_) ifTrue else ifFalse)
      }

      override def whileM[G[_], A](p: Binding[Boolean], body: => Binding[A])(implicit
          G: MonadPlus[G]
      ): Binding[G[A]] = {
        ifM(p, bind(body)(x => map(whileM(p, body))(xs => G.plus(G.point(x), xs))), point(G.empty))
      }

      override def whileM_[A](p: Binding[Boolean], body: => Binding[A]): Binding[Unit] = {
        ifM(p, bind(body)(_ => whileM_(p, body)), point(()))
      }

      override def untilM[G[_], A](f: Binding[A], cond: => Binding[Boolean])(implicit
          G: MonadPlus[G]
      ): Binding[G[A]] = {
        bind(f)(x => map(whileM(map(cond)(!_), f))(xs => G.plus(G.point(x), xs)))
      }

      override def untilM_[A](f: Binding[A], cond: => Binding[Boolean]): Binding[Unit] = {
        bind(f)(_ => whileM_(map(cond)(!_), f))
      }

    }
    override val typeClass = BindingInstances
  }

  trait BindingSeq2Or3[+A] { this: BindingSeq[A] =>
    def foreach[U](f: A => U): Unit = macro Macros.foreach

    /** Returns a [[BindingSeq]] that maps each element of this [[BindingSeq]] via `f`
      *
      * @param f
      *   The mapper function, which may contain magic [[Binding#bind bind]] calls.
      */
    def map[B](f: A => B): BindingSeq[B] = macro Macros.map

    /** Returns a [[BindingSeq]] that flat-maps each element of this [[BindingSeq]] via `f`
      *
      * @param f
      *   The mapper function, which may contain magic [[Binding#bind bind]] calls.
      */
    def flatMap[B](f: A => BindingSeq[B]): BindingSeq[B] = macro Macros.flatMap

    /** Returns a view of this [[BindingSeq]] that applied a filter of `condition`
      *
      * @param f
      *   The mapper function, which may contain magic [[Binding#bind bind]] calls.
      */
    def withFilter(condition: A => Boolean): BindingSeq.WithFilter[A] = macro Macros.withFilter

  }

  object BindingSeq2Or3 {
    trait WithFilter2Or3[+A] {

      /** Returns a [[BindingSeq]] that maps each element of this [[BindingSeq]] via `f`
        */
      def map[B](f: A => B): BindingSeq[B] = macro Macros.map

      /** Returns a [[BindingSeq]] that flat-maps each element of this [[BindingSeq]] via `f`
        */
      def flatMap[B](f: A => BindingSeq[B]): BindingSeq[B] = macro Macros.flatMap

      /** Returns a view of this [[BindingSeq]] that applied a filter of `condition`
        */
      def withFilter(condition: A => Boolean): BindingSeq.WithFilter[A] = macro Macros.withFilter

    }
  }
}

private[binding] trait Binding2Or3[+A] { this: binding.Binding[A] =>

  /** Returns the current value of this [[Binding]] and marks the current `@dom` method depend on this [[Binding]].
    *
    * Each time the value changes, in the current `@dom` method, all code after the current `bind` expression will be
    * re-evaluated if the current `@dom` method is [[#watch watch]]ing. However, code in current `@dom` method and
    * before the current `bind` expression will not be re-evaluated. The above rule is not applied to DOM nodes created
    * by XHTML literal. A `bind` expression under a DOM node does not affect siblings and parents of that node.
    *
    * @note
    *   This method must be invoked inside a `@dom` method body or a `Binding { ... }` block..
    */
  final def bind: A = macro Binding2Or3.Macros.bind

}
