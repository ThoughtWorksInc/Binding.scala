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
  /** The data binding expression of a sequence, essentially an asynchronous
    * stream of patches describing how the sequence is changing.
    */
  type BindingSeq[+A] = BindingSeqT[Future, A]
  object BindingSeq:
    export BindingSeqT._

  export BindingT._

type Binding[+A] = BindingT[Future, A]
