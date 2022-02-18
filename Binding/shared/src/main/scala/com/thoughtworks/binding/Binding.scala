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
      CovariantStreamT.memoize(
        CovariantStreamT
          .noSkip(new Reset {
            type ShouldResetNestedFunctions = false
            type DontSuspend = true
          }.*[Binding](a).distinctUntilChanged)
      )
    }
    CovariantStreamT(
      StreamT(
        Applicative[DefaultFuture].pure(
          Skip(() => CovariantStreamT.apply flip tail)
        )
      )
    ).dropHistory

  inline def events[A](
      inline a: A
  )(using Monad[DefaultFuture]): Binding[A] =
    lazy val tail = {
      @inline given Equal[A] = Equal.equalA[A]
      CovariantStreamT.memoize(
        CovariantStreamT
          .noSkip(new Reset {
            type ShouldResetNestedFunctions = false
            type DontSuspend = true
          }.*[Binding](a))
      )
    }
    CovariantStreamT(
      StreamT(
        Applicative[DefaultFuture].pure(
          Skip(() => CovariantStreamT.apply flip tail)
        )
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
          CovariantStreamT(s()).dropHistoryStrict(a)
        case Some(Success(Skip(s))) =>
          CovariantStreamT(s()).dropHistoryStrict(latest)

    @tailrec
    private def dropHistoryStrict(using
        Applicative[DefaultFuture]
    ): Binding[A] =
      binding.step.value match
        case None | Some(Success(Done())) | Some(Failure(_)) =>
          binding
        case Some(Success(Yield(a, s))) =>
          CovariantStreamT(s()).dropHistoryStrict(a)
        case Some(Success(Skip(s))) =>
          CovariantStreamT(s()).dropHistoryStrict

    def dropHistory(using Applicative[DefaultFuture]): Binding[A] =
      CovariantStreamT(
        StreamT(
          Applicative[DefaultFuture].point(
            Skip(new (() => StreamT[DefaultFuture, A]) {
              @volatile
              private var cache: Binding[A] = binding
              def apply() =
                cache = cache.dropHistoryStrict
                CovariantStreamT.apply.flip(cache)
              end apply
            })
          )
        )
      )

    @tailrec
    private def dropCompletedStrict: Binding[A] =
      binding.step.value match
        case None | Some(Success(Done())) | Some(Failure(_)) =>
          binding
        case Some(Success(Yield(_, s))) =>
          CovariantStreamT(s()).dropCompletedStrict
        case Some(Success(Skip(s))) =>
          CovariantStreamT(s()).dropCompletedStrict

    def dropCompleted(using Applicative[DefaultFuture]): Binding[A] =
      CovariantStreamT(
        StreamT(
          Applicative[DefaultFuture].point(
            Skip(new (() => StreamT[DefaultFuture, A]) {
              @volatile
              private var cache: Binding[A] = binding
              def apply() =
                cache = cache.dropCompletedStrict
                CovariantStreamT.apply.flip(cache)
            })
          )
        )
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
    ): Binding[(Snapshot[A], Patch[A])] = CovariantStreamT apply StreamT(
      M.bind(mutationStream.step) {
        case Yield(mutation, s) =>
          val patch = mutation(state)
          val newState = patch.applyTo(state)
          lazy val nextStep = snapshotStream(CovariantStreamT(s()), newState)
          M.point(
            Yield((newState, patch), () => CovariantStreamT.apply flip nextStep)
          )
        case Skip(s) =>
          snapshotStream(CovariantStreamT(s()), state).step
        case Done() =>
          M.point(Done())
      }
    )
    @inline def fromMutationStream[A](
        mutationStream: => MutationStream[A],
        state: Snapshot[A] = Snapshot.empty
    )(using
        M: Monad[DefaultFuture]
    ): BindingSeq[A] =
      // Put the cache in an object for lazy initialization
      object Cache:
        @volatile
        private var cache = snapshotStream(mutationStream, state)
        def dropHistoryAndUpdateCache(): Binding[Patch[A]] = {
          val newCache = cache.dropHistoryStrict
          cache = newCache
          newCache.step.value match
            case Some(Success(Yield(a, s))) =>
              Patch.ReplaceChildren(new Iterable[A] {
                def iterator = a._1.iterator
              }) :: CovariantStreamT(s()).map(_._2)
            case _ =>
              Patch.ReplaceChildren(new Iterable[A] {
                def iterator = state.iterator
              }) :: newCache.map(_._2)
        }

      BindingSeq(
        CovariantStreamT(
          StreamT(
            Applicative[DefaultFuture].point(
              Skip(() =>
                CovariantStreamT.apply flip Cache.dropHistoryAndUpdateCache()
              )
            )
          )
        )
      )

  export CovariantStreamT.{apply => _, empty => _, _}

  opaque type Constant[+A] <: Binding[A] = Binding[A]
  object Constant:
    def apply[A](a: A)(using Applicative[DefaultFuture]): Binding[A] =
      CovariantStreamT.pure(a)

  def empty[A](using Applicative[DefaultFuture]): Binding[A] =
    CovariantStreamT.empty[DefaultFuture, A]

  private[binding] final class Pipe[A]
      extends (() => StreamT[DefaultFuture, A]):
    private[binding] val promise =
      Promise[StreamT.Yield[A, StreamT[DefaultFuture, A]]]()
    def apply() = CovariantStreamT.apply.flip(read)
    def read: Binding[A] = CovariantStreamT(StreamT(promise.future))
    def writeHead(a: A): Unit =
      promise.success(StreamT.Yield(a, Pipe()))
    def writeTail(using ExecutionContext): Binding[A => Unit] =
      CovariantStreamT apply StreamT(promise.future.map {
        case StreamT.Yield(_, pipe: Pipe[A]) =>
          StreamT.Yield(
            pipe.writeHead,
            () => CovariantStreamT.apply flip pipe.writeTail
          )
      })
    def write(using ExecutionContext): Binding[A => Unit] =
      writeHead :: writeTail

  @inline def pipe[A](using
      ExecutionContext
  ): (Binding[A], Binding[A => Unit]) =
    val p = Pipe[A]()
    (p.read, p.write)

  private final class VarFunction[A](private var head: A)
      extends (() => StreamT[DefaultFuture, A]):
    private var tail: Promise[Yield[A, StreamT[DefaultFuture, A]]] = Promise()

    def apply() =
      val (head, tail) = synchronized {
        (this.head, this.tail)
      }
      StreamT(
        Future.successful(Yield(head, StreamT(tail.future)))
      )
    end apply
    def get = head
    def set(newHead: A): Unit =
      val newTail = Promise[Yield[A, StreamT[DefaultFuture, A]]]()
      val oldTail = synchronized {
        val oldTail = tail
        head = newHead
        tail = newTail
        oldTail
      }
      oldTail.success(Yield(newHead, this))
    end set

  opaque type Var[A] <: Binding[A] = Binding[A]

  object Var:
    def apply[A](a: A): Var[A] =
      CovariantStreamT(
        StreamT(
          Future.successful(StreamT.Skip(VarFunction(a)))
        )
      )
    extension [A](self: Var[A])
      private def varFunction =
        val StreamT.Skip(s: VarFunction[A]) =
          CovariantStreamT.apply.flip(self).step.value.get.get
        s
      end varFunction
      def value: A = varFunction.get
      def value_=(a: A): Unit = varFunction.set(a)

  opaque type Constants[+A] <: BindingSeq[A] = BindingSeq[A]
  object Constants:
    def apply[A](elements: A*)(using Applicative[DefaultFuture]): Constants[A] =
      PatchStreamT.fromIterable(elements)

type Binding[+A] = CovariantStreamT[DefaultFuture, A]
