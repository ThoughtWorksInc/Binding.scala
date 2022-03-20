package com.thoughtworks.binding

import scalaz.Free
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.annotation.tailrec
import scalaz.Monad
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.View

sealed trait Observable[+A]:
  @inline def flatMapLatest[B](
      mapper: A => Observable[B],
      default: Observable[B] = Observable.AsyncList.Empty
  )(using ExecutionContext): Observable.AsyncList.NonEmpty.Lazy[B] =
    flatMapLatest(
      { a =>
        mapper(a).replay
      },
      default.replay
    )
  private def flatMapLatest[B](
      mapper: A => Observable.AsyncList[B],
      default: Observable.AsyncList[B]
  )(using ExecutionContext): Observable.AsyncList.NonEmpty.Lazy[B] =
    final class FlatMapLatest extends Observable.AsyncList.NonEmpty.Lazy[B]:
      protected lazy val nextValue =
        def handleA(
            head: Iterable[A],
            tail: Observable[A]
        ): (Iterable[B], Observable.AsyncList[B]) =
          val newDefault = head.lastOption match
            case None =>
              default
            case Some(last) =>
              mapper(last)
          (View.Empty, tail.flatMapLatest(mapper, newDefault))
        def handleB(
            head: Iterable[B],
            tail: Observable.AsyncList[B]
        ): (Iterable[B], Observable.AsyncList[B]) =
          (head, Observable.this.flatMapLatest[B](mapper, tail))
        Observable.this match
          case nonEmptyA: Observable.NonEmpty[A] =>
            default match
              case Observable.AsyncList.Empty =>
                nonEmptyA.next().map(handleA)
              case nonEmptyB: Observable.AsyncList.NonEmpty[B] =>
                val handler = Future.firstCompletedOf(
                  Seq(
                    nonEmptyA
                      .next() // This is problematic because nonEmptyA.next() might not be idempotent
                      .map { case (head, tail) =>
                        () => handleA(head, tail)
                      }(using ExecutionContext.parasitic),
                    nonEmptyB
                      .next()
                      .map { case (head, tail) =>
                        () => handleB(head, tail)
                      }(using ExecutionContext.parasitic)
                  )
                )
                handler.map(_())
          case Observable.Empty =>
            default match
              case Observable.AsyncList.Empty =>
                Future.successful(View.Empty, Observable.Empty)
              case nonEmptyB: Observable.AsyncList.NonEmpty[B] =>
                nonEmptyB.next()
    new FlatMapLatest
  end flatMapLatest
  def replay: Observable.AsyncList[A] =
    this match
      case asyncList: Observable.AsyncList[A] =>
        asyncList
      case nonEmpty: Observable.NonEmpty[A] =>
        final class Replay extends Observable.AsyncList.NonEmpty.Lazy[A]:
          protected lazy val nextValue = nonEmpty
            .next()
            .map { case (head, tail) =>
              (head, tail.replay)
            }(ExecutionContext.parasitic)
        new Replay
  end replay

object Observable:
  sealed trait AsyncList[+A] extends Observable[A]
  object AsyncList:

    object Empty extends AsyncList[Nothing]
    sealed trait NonEmpty[+A] extends AsyncList[A] with Observable.NonEmpty[A]:
      final def next(): Future[(Iterable[A], AsyncList[A])] = nextValue

      /** The stable memoized value of `next()`, which must be either a
        * non-`lazy` or `lazy val`
        */
      protected def nextValue: Future[(Iterable[A], AsyncList[A])]
    end NonEmpty

    object NonEmpty:
      trait Lazy[+A] extends NonEmpty[A]:
        protected lazy val nextValue: Future[(Iterable[A], AsyncList[A])]
      end Lazy
      final case class Eager[+A](
          protected val nextValue: Future[(Iterable[A], AsyncList[A])]
      ) extends NonEmpty[A]
    end NonEmpty
  end AsyncList

  val Empty = AsyncList.Empty
  sealed trait NonEmpty[+A] extends Observable[A]:
    def next(): Future[(Iterable[A], Observable[A])]

  @FunctionalInterface
  trait Subject[+A] extends Observable.NonEmpty[A]
  object Subject:
    trait Behavior[+A] extends NonEmpty[A]:
      def updatedState(): Behavior.State[A]
      final def next() = {
        updatedState() match
          case Behavior.State.Incompleted(head, nextValue) =>
            Future.successful(
              (View.Single(head), (() => nextValue): Subject[A])
            )
          case Behavior.State.Completed(head) =>
            Future.successful((View.Single(head), Empty))
      }

    object Behavior:
      sealed trait State[+A]:
        def head: A
        def tail: Observable[A]
      object State:
        def apply[A](head: A, tail: Observable[A]) =
          tail match
            case Empty =>
              State.Completed(head)
            case nonEmptyTail: NonEmpty[A] =>
              State.Incompleted(head, nonEmptyTail.next())

        final case class Incompleted[A](
            head: A,
            nextValue: Future[(Iterable[A], Observable[A])]
        ) extends State[A]:
          def tail: Subject[A] = () => nextValue
        final case class Completed[A](head: A) extends State[A]:
          def tail = Empty

      def pure[A](a: A): Behavior[A] = (() => State.Completed(a))
      def apply[A](initialValue: A, source: Observable[A]): Behavior[A] =
        new Behavior[A]:
          private var state = State(initialValue, source)
          def updatedState() = synchronized {
            @tailrec
            def loop(state: State[A]): State[A] =
              state match
                case State.Incompleted(head, nextValue) =>
                  nextValue.value match
                    case None =>
                      state
                    case Some(tryValue) =>
                      val (batch, newTail) = tryValue.get
                      loop(
                        State(batch.lastOption.getOrElse(head), newTail)
                      )
                case _: State.Completed[?] =>
                  state
            end loop
            state = loop(state)
            state
          }
        end new
      end apply

      given (using ExecutionContext): Monad[Behavior] with
        def point[A](a: => A): Behavior[A] = Behavior.pure(a)

        def bind[A, B](
            fa: Behavior[A]
        )(mapper: A => Behavior[B]): Behavior[B] =
          val cacheA = fa.updatedState()
          val cacheB = mapper(cacheA.head).updatedState()
          Behavior(
            cacheB.head,
            cacheA.tail.flatMapLatest(mapper, cacheB.tail)
          )
        end bind
      end given
    end Behavior
  end Subject
