package com.thoughtworks
package binding

import scala.collection.SeqOps
import scala.quoted.Quotes
import scala.quoted.Expr
import scala.quoted.Type
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.macros.Reset

import Binding.BindingSeq

private[binding] object Binding2Or3:
  type SeqOpsIterable[+A] = Iterable[A]
    with SeqOps[A, _ <: Iterable, _ <: Iterable[A]]

  trait BindingInstances2Or3

  val BindingReset = new Reset:
    type ShouldResetNestedFunctions = false
    type DontSuspend = true

  trait Companion:
    this: binding.Binding.type =>

    inline def apply[A](inline a: A): Binding[A] = BindingReset.*[Binding](a)

    opaque type Bind[+A] <: Dsl.Keyword.Opaque =
      Dsl.Keyword.Opaque.Of[Binding[A]]

    final def Bind[A](using
        dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit
    ): Binding[A] =:= Bind[A] =
      Dsl.Keyword.Opaque.Of
    end Bind

    object Bind:
      given [A]: Dsl.IsKeyword[Bind[A], A] with {}

      inline given [A, B]: Dsl.Original[Bind[A], Binding[B], A] =
        Dsl.Original(_ flatMap _)
      end given
    end Bind

    @inline given [A]: Dsl.Lift.OneStep[A, Binding[A]] = Constant(_)

    extension [A](inline binding: Binding[A])
      /** Returns the current value of this [[Binding]] and marks the current
        * `@dom` method depend on this [[Binding]].
        *
        * Each time the value changes, in the current `@dom` method, all code
        * after the current `bind` expression will be re-evaluated if the
        * current `@dom` method is [[#watch watch]]ing. However, code in current
        * `@dom` method and before the current `bind` expression will not be
        * re-evaluated. The above rule is not applied to DOM nodes created by
        * XHTML literal. A `bind` expression under a DOM node does not affect
        * siblings and parents of that node.
        *
        * @note
        *   This method must be invoked inside a `@dom` method body or a
        *   `Binding { ... }` block..
        */
      transparent inline def bind: A =
        Dsl.shift[Binding.Bind[A], A](Binding.Bind(binding))
      end bind
    end extension
  end Companion

  object Macros:
    private def bindingFunctionBody[A: quoted.Type, B: quoted.Type](
        f: quoted.Expr[A => B]
    )(using Quotes) =
      import quoted.quotes.reflect.*
      f.asTerm match
        case inlined @ Inlined(
              call,
              bindings,
              block @ Block(
                List(
                  defDef @ DefDef(
                    name,
                    paramss @ List(
                      TermParamClause(
                        List(param @ ValDef(paramName, paramTpt, _))
                      )
                    ),
                    tpt,
                    Some(rhs)
                  )
                ),
                closureIdent
              )
            ) =>
          Inlined
            .copy(inlined)(
              call,
              bindings,
              '{ (a: A) =>
                ${
                  Block(
                    List(
                      ValDef
                        .copy(param)(paramName, paramTpt, Some('a.asTerm))
                        .changeOwner(Symbol.spliceOwner)
                    ),
                    '{
                      Binding(${
                        rhs.changeOwner(Symbol.spliceOwner).asExprOf[B]
                      })
                    }.asTerm.changeOwner(Symbol.spliceOwner)
                  )
                    .asExprOf[Binding[B]]
                }: Binding[B]
              }.asTerm
            )
            .asExprOf[A => Binding[B]]
        case _ =>
          '{ (a: A) => Binding.Constant($f(a)): Binding[B] }
      end match
    end bindingFunctionBody

    def foreach[A: quoted.Type, U: quoted.Type](
        self: quoted.Expr[BindingSeq[A]],
        f: quoted.Expr[A => U]
    )(using
        qctx: Quotes
    ): quoted.Expr[Unit] = '{
      $self.foreachBinding(${ bindingFunctionBody(f) }).bind
    }

    def map[A: quoted.Type, B: quoted.Type](
        self: quoted.Expr[BindingSeqOrWithFilter[A]],
        f: quoted.Expr[A => B]
    )(using
        qctx: Quotes
    ): quoted.Expr[BindingSeq[B]] = '{
      $self.mapBinding(${ bindingFunctionBody(f) })
    }

    def flatMap[A: quoted.Type, B: quoted.Type](
        self: quoted.Expr[BindingSeqOrWithFilter[A]],
        f: quoted.Expr[A => BindingSeq[B]]
    )(using
        qctx: Quotes
    ): quoted.Expr[BindingSeq[B]] = '{
      $self.flatMapBinding(${ bindingFunctionBody(f) })
    }

    def withFilter[A: quoted.Type](
        self: quoted.Expr[BindingSeqOrWithFilter[A]],
        f: quoted.Expr[A => Boolean]
    )(using
        qctx: Quotes
    ) = '{ $self.withFilterBinding(${ bindingFunctionBody(f) }) }

  end Macros

  object BindingSeqOrWithFilter:
    extension [A](inline bindingSeqOrWithFilter: BindingSeqOrWithFilter[A])

      /** Returns a [[BindingSeq]] that maps each element of this [[BindingSeq]]
        * via `f`
        *
        * @param f
        *   The mapper function, which may contain magic [[Binding#bind bind]]
        *   calls.
        */
      inline def map[B](inline f: A => B): BindingSeq[B] = ${
        Macros.map('bindingSeqOrWithFilter, 'f)
      }

      /** Returns a [[BindingSeq]] that flat-maps each element of this
        * [[BindingSeq]] via `f`
        *
        * @param f
        *   The mapper function, which may contain magic [[Binding#bind bind]]
        *   calls.
        */
      inline def flatMap[B](inline f: A => BindingSeq[B]): BindingSeq[B] = ${
        Macros.flatMap('bindingSeqOrWithFilter, 'f)
      }

      /** Returns a view of this [[BindingSeq]] that applied a filter of
        * `condition`
        *
        * @param f
        *   The mapper function, which may contain magic [[Binding#bind bind]]
        *   calls.
        */
      inline def withFilter(
          inline condition: A => Boolean
      ): BindingSeq.WithFilter[A] = ${
        Macros.withFilter('bindingSeqOrWithFilter, 'condition)
      }
    end extension
  end BindingSeqOrWithFilter

  trait BindingSeqOrWithFilter[+A]:
    def mapBinding[B](f: A => Binding[B]): BindingSeq[B]
    def flatMapBinding[B](f: A => Binding[BindingSeq[B]]): BindingSeq[B]
    def withFilterBinding(
        condition: A => Binding[Boolean]
    ): BindingSeq.WithFilter[A]
  end BindingSeqOrWithFilter

  object BindingSeq2Or3:
    extension [A](inline bindingSeq: BindingSeq[A])
      transparent inline def foreach[U](inline f: A => U): Unit = ${
        Macros.foreach('bindingSeq, 'f)
      }
    end extension

    trait WithFilter2Or3[+A] extends BindingSeqOrWithFilter[A]:
      this: BindingSeq.WithFilter[A] =>

    end WithFilter2Or3

  end BindingSeq2Or3

  trait BindingSeq2Or3[+A] extends BindingSeqOrWithFilter[A]:
    this: BindingSeq[A] =>

  end BindingSeq2Or3

end Binding2Or3

private[binding] trait Binding2Or3[+A]:
  this: binding.Binding[A] =>
end Binding2Or3
