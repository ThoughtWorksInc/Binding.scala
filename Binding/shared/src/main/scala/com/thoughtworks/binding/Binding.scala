package com.thoughtworks.binding

import scalaz.-\/
import scalaz.Applicative
import scalaz.DList
import scalaz.Equal
import scalaz.FingerTree
import scalaz.Free
import scalaz.IList
import scalaz.Memo
import scalaz.Monad
import scalaz.MonadPlus
import scalaz.Monoid
import scalaz.Nondeterminism
import scalaz.Reducer
import scalaz.StreamT
import scalaz.StreamT.Done
import scalaz.StreamT.Skip
import scalaz.StreamT.Yield
import scalaz.UnitReducer
import scalaz.\/-

import scala.concurrent.Future
import scala.collection.IndexedSeqView
import scalaz.StreamT.Step
import scala.annotation.unchecked.uncheckedVariance

object Binding:
  type Awaitable[+A] = Future[A]

  /** The data binding expression of a sequence, essentially an asynchronous
    * stream of patches describing how the sequence is changing.
    */
  type BindingSeq[+A] = BindingSeqT[Awaitable, A]
  object BindingSeq:
    export BindingSeqT._

  export BindingT._

  opaque type Constant[+A] <: Binding[A] = Binding[A]
  object Constant:
    def apply[A](a: A)(using Applicative[Awaitable]): Binding[A] = BindingT(
      a :: StreamT.empty
    )
  opaque type Constants[+A] <: BindingSeq[A] = BindingSeq[A]
  object Constants:
    def apply[A](elements: A*)(using Applicative[Awaitable]): Constants[A] =
      BindingSeqT(
        BindingT(BindingSeqT.Patch.Splice[A](0, 0, elements) :: StreamT.empty)
      )
type Binding[+A] = BindingT[Binding.Awaitable, A]
