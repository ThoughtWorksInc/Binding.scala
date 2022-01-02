package com.thoughtworks.binding
import scalaz.-\/
import scalaz.Applicative
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
import scalaz.StreamT.Step
import scalaz.StreamT.Yield
import scalaz.UnitReducer
import scalaz.\/-

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.IndexedSeqView
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.Success
import scalajs.js

private[binding] trait JSBinding { this: Binding.type =>

  private def jsPipeWriteTail[A](pipe: Pipe[A])(using
      ExecutionContext
  ): Binding[js.Function1[A, Unit]] =
    import pipe.*
    StreamT(promise.future.map { case StreamT.Yield(_, pipe: Pipe[A]) =>
      StreamT.Yield(pipe.writeHead, () => jsPipeWriteTail(pipe))
    })
  private def jsPipeWrite[A](pipe: Pipe[A])(using
      ExecutionContext
  ): Binding[js.Function1[A, Unit]] =
    import pipe.*
    writeHead _ :: jsPipeWriteTail(pipe)

  @inline def jsPipe[A](using
      ExecutionContext
  ): (Binding[A], Binding[js.Function1[A, Unit]]) =
    val p = new Pipe[A]()
    (p.read, jsPipeWrite(p))

}
