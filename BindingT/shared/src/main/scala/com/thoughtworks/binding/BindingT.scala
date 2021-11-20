package com.thoughtworks.binding

import scalaz.StreamT
import scala.concurrent.Future
import scalaz.Monad
import scalaz.Nondeterminism
import scalaz.Applicative
import scalaz.StreamT.Skip
import scalaz.StreamT.Done
import scalaz.StreamT.Yield
import scalaz.StreamT.Step
import scalaz.IList
import scalaz.-\/
import scalaz.\/-
import scalaz.Monoid
import scalaz.ICons
import scalaz.INil
import scalaz.Isomorphism.IsoFunctor
import scalaz.Isomorphism.IsoFunctorTemplate
import scalaz.MonadPlus
import scalaz.IsomorphismMonadPlus
import scalaz.Functor
import scalaz.Equal

opaque type BindingT[M[_], A] <: StreamT[M, A] = StreamT[M, A]

object BindingT:
  given [M[_]](using M: Nondeterminism[M]): Monad[[A] =>> BindingT[M, A]] with
    def point[A](a: => A) = StreamT.StreamTMonadPlus.point(a)
    def bind[A, B](fa: BindingT[M, A])(f: A => BindingT[M, B]): BindingT[M, B] =
      given Equal[B] = Equal.equalA[B]
      fa.flatMapLatest(f).distinctUntilChanged
    override def map[A, B](fa: BindingT[M, A])(f: A => B): BindingT[M, B] =
      given Equal[B] = Equal.equalA[B]
      fa.map(f).distinctUntilChanged
