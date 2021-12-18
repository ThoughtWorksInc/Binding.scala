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
import scala.util.Success

final case class NodeBinding[+A](value: A, eventLoop: Binding[Nothing])

object NodeBinding:
  given [A](using
      Applicative[Binding.Awaitable]
  ): BindableSeq.Aux[NodeBinding[A], A] =
    new BindableSeq[NodeBinding[A]]:
      type Value = A
      def toBindingSeq(nodeBinding: NodeBinding[A]): Binding.BindingSeq[A] =
        BindingSeqT(
          BindingT(
            BindingSeqT.Patch.Splice[A](
              0,
              0,
              collection.View.Single(nodeBinding.value)
            ) :: BindingT.apply.flip(nodeBinding.eventLoop)
          )
        )
