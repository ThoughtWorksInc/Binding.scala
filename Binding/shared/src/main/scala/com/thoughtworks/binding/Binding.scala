package com.thoughtworks.binding
import com.thoughtworks.dsl.macros.Reset
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
import StreamT.Done
import StreamT.Skip
import StreamT.Step
import StreamT.Yield
import scalaz.UnitReducer
import scalaz.\/-

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.IndexedSeqView
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.Success
import scalaz.ReaderT
import scalaz.Functor
import scala.annotation.tailrec
import scala.util.Failure

object Binding extends JSBinding:

  inline def apply[A](
      inline a: A
  )(using Monad[DefaultFuture]): Binding[A] =
    lazy val tail = {
      @inline given Equal[A] = Equal.equalA[A]
      StreamT.memoize(
        StreamT
          .noSkip(new Reset {
            type ShouldResetNestedFunctions = false
            type DontSuspend = true
          }.*[Binding](a).distinctUntilChanged)
      )
    }
    CovariantStreamT(
      Applicative[DefaultFuture].pure(
        Skip(() => tail)
      )
    ).dropHistory

  extension [A](binding: Binding[A])
    @tailrec
    private def dropHistoryStrict(latest: A)(using
        Applicative[DefaultFuture]
    ): Binding[A] =
      binding.step.value match
        case None | Some(Success(Done())) | Some(Failure(_)) =>
          latest :: binding
        case Some(Success(Yield(a, s))) =>
          s().dropHistoryStrict(a)
        case Some(Success(Skip(s))) =>
          s().dropHistoryStrict(latest)

    @tailrec
    private def dropHistoryStrict(using
        Applicative[DefaultFuture]
    ): Binding[A] =
      binding.step.value match
        case None | Some(Success(Done())) | Some(Failure(_)) =>
          binding
        case Some(Success(Yield(a, s))) =>
          s().dropHistoryStrict(a)
        case Some(Success(Skip(s))) =>
          s().dropHistoryStrict

    def dropHistory(using Applicative[DefaultFuture]): Binding[A] =
      CovariantStreamT(
        Applicative[DefaultFuture].point(Skip(new (() => Binding[A]) {
          @volatile
          private var cache: Binding[A] = binding
          def apply(): Binding[A] =
            cache = cache.dropHistoryStrict
            cache
        }))
      )

  /** The data binding expression of a sequence, essentially an asynchronous
    * stream of patches describing how the sequence is changing.
    */
  type BindingSeq[+A] = PatchStreamT[DefaultFuture, A]
  object BindingSeq:
    export PatchStreamT._

    type Mutation[A] = Snapshot[A] => Patch[A]
    type MutationStream[A] = CovariantStreamT[DefaultFuture, Mutation[A]]

    private def snapshotStream[A](
        mutationStream: MutationStream[A],
        state: Snapshot[A]
    )(using
        M: Monad[DefaultFuture]
    ): StreamT[DefaultFuture, (Snapshot[A], Patch[A])] = StreamT(
      M.bind(mutationStream.step) {
        case Yield(mutation, s) =>
          val patch = mutation(state)
          val newState = patch.applyTo(state)
          lazy val nextStep = snapshotStream(s(), newState)
          M.point(Yield((newState, patch), () => nextStep))
        case Skip(s) =>
          snapshotStream(s(), state).step
        case Done() =>
          M.point(Done())
      }
    )
    @inline def fromMutationStream[A](
        mutationStream: MutationStream[A],
        state: Snapshot[A] = Snapshot.empty
    )(using
        M: Monad[DefaultFuture]
    ): BindingSeq[A] =
      // Put the cache in an object for lazy initialization
      object Cache:
        @volatile
        private var cache = snapshotStream(mutationStream, state)
        def dropHistoryAndUpdateCache(): StreamT[DefaultFuture, Patch[A]] = {
          cache = cache.dropHistoryStrict
          cache.step.value match
            case Some(Success(Yield(a, s))) =>
              Patch.ReplaceChildren(a._1.toList) :: s().map(_._2)
            case _ =>
              cache.map(_._2)
        }

      BindingSeq(
        CovariantStreamT(
          Applicative[DefaultFuture].point(
            Skip(() => Cache.dropHistoryAndUpdateCache())
          )
        )
      )

  export CovariantStreamT._

  opaque type Constant[+A] <: Binding[A] = Binding[A]
  object Constant:
    def apply[A](a: A)(using Applicative[DefaultFuture]): Binding[A] =
      CovariantStreamT.pure(a)

  def empty[A](using Applicative[DefaultFuture]): Binding[A] =
    StreamT.empty[DefaultFuture, A]

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

  private final class VarFunction[A](private var head: A)
      extends (() => Binding[A]):
    private var tail: Promise[Yield[A, Binding[A]]] = Promise()

    def apply: Binding[A] = StreamT(Future.successful(Yield(head, this)))
    def get = head
    def set(newHead: A): Unit =
      val newTail = Promise[Yield[A, Binding[A]]]()
      val oldTail = synchronized {
        val oldTail = tail
        head = newHead
        tail = newTail
        oldTail
      }
      oldTail.success(Yield(newHead, this))

  opaque type Var[A] <: Binding[A] = Binding[A] {
    val step: DefaultFuture[StreamT.Skip[A, Binding[A]]] {
      def value: Some[Success[
        StreamT.Skip[A, Binding[A]] {
          val s: VarFunction[A]
        }
      ]]
    }
  }

  object Var:
    def apply[A](a: A): Var[A] =
      val binding: Binding[A] = StreamT(
        Future.successful(StreamT.Skip(VarFunction(a)))
      )
      binding.asInstanceOf[Var[A]]
    extension [A](self: Var[A])
      def value: A = self.step.value.get.get.s.get
      def value_=(a: A): Unit = self.step.value.get.get.s.set(a)

  opaque type Constants[+A] <: BindingSeq[A] = BindingSeq[A]
  object Constants:
    def apply[A](elements: A*)(using Applicative[DefaultFuture]): Constants[A] =
      PatchStreamT.fromIterable(elements)

type Binding[+A] = CovariantStreamT[DefaultFuture, A]
