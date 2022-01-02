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
import scalaz.ReaderT

object Binding extends JSBinding:

  /** The data binding expression of a sequence, essentially an asynchronous
    * stream of patches describing how the sequence is changing.
    */
  type BindingSeq[+A] = PatchStreamT[DefaultFuture, A]
  object BindingSeq:
    export PatchStreamT._
    type Reader[S, A] = PatchStreamT[ReaderT[S, DefaultFuture, _], A]
    type SnapshotReader[A] = PatchStreamT[ReaderT[Snapshot[A], DefaultFuture, _], A]
    type SizeReader[A] = PatchStreamT[ReaderT[Int, DefaultFuture, _], A]

  export CovariantStreamT._

  opaque type Constant[+A] <: Binding[A] = Binding[A]
  object Constant:
    def apply[A](a: A)(using Applicative[DefaultFuture]): Binding[A] =
      CovariantStreamT.pure(a)

  private[binding] final class Pipe[A] extends (() => Binding[A]):
    private[binding] val promise = Promise[StreamT.Yield[A, Binding[A]]]()
    def apply(): Binding[A] = read
    def read: Binding[A] = StreamT(promise.future)
    def writeHead(a: A): Unit =
      promise.success(StreamT.Yield(a, Pipe()))
    def writeTail(using ExecutionContext): Binding[A => Unit] =
      StreamT(promise.future.map { case StreamT.Yield(_, pipe: Pipe[A]) =>
        StreamT.Yield(pipe.writeHead, () => pipe.writeTail)
      })
    def write(using ExecutionContext): Binding[A => Unit] =
      writeHead :: writeTail

  @inline def pipe[A](using
      ExecutionContext
  ): (Binding[A], Binding[A => Unit]) =
    val p = Pipe[A]()
    (p.read, p.write)

  @deprecated("Use `pipe` or `jsPipe` instead", "13.0.0")
  opaque type Var[A] <: Binding[A] = Binding[A] {
    val step: DefaultFuture[
      StreamT.Yield[A, Binding[A]]
    ] {
      def value: Some[Success[
        StreamT.Yield[A, Binding[A]] {
          val s: Pipe[A]
        }
      ]]
    }
  }

  @deprecated("Use `pipe` or `jsPipe` instead", "13.0.0")
  object Var:
    def apply[A](a: A): Var[A] =
      val binding: Binding[A] = StreamT(
        Future.successful(StreamT.Yield(a, Pipe[A]()))
      )
      binding.asInstanceOf[Var[A]]
    extension [A](self: Var[A])
      private def value: A = ???
      def value_= : A => Unit =
        new (A => Unit) {
          var currentPipe = self.step.value.get.get.s
          def apply(a: A) =
            val nextPipe = Pipe[A]()
            val oldPipe = synchronized {
              val oldPipe = currentPipe
              currentPipe = nextPipe
              oldPipe
            }
            oldPipe.writeHead(a)
        }

  opaque type Constants[+A] <: BindingSeq[A] = BindingSeq[A]
  object Constants:
    def apply[A](elements: A*)(using Applicative[DefaultFuture]): Constants[A] =
      PatchStreamT.fromIterable(elements)

type Binding[+A] = CovariantStreamT[DefaultFuture, A]
