package com.thoughtworks.binding

import scalaz.Free
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.annotation.tailrec
import scalaz.Monad
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.View

sealed trait Observable[+A]:
  def flatMapLatest[B](
      mapper: A => Observable[B],
      default: Observable[B] = Observable.Empty
  )(using ExecutionContext): Observable.NonEmpty[B] = { () =>
    def handleA(
        head: Iterable[A],
        tail: Observable[A]
    ): (Iterable[B], Observable[B]) =
      val newDefault = head.lastOption match
        case None =>
          default
        case Some(last) =>
          mapper(last)
      (View.Empty, tail.flatMapLatest(mapper, newDefault))
    def handleB(
        head: Iterable[B],
        tail: Observable[B]
    ): (Iterable[B], Observable[B]) =
      (head, this.flatMapLatest(mapper, tail))
    this match
      case nonEmptyA: Observable.NonEmpty[A] =>
        default match
          case nonEmptyB: Observable.NonEmpty[B] =>
            val handler = Future.firstCompletedOf(
              Seq(
                nonEmptyA.next().map { case (head, tail) =>
                  () => handleA(head, tail)
                },
                nonEmptyB.next().map { case (head, tail) =>
                  () => handleB(head, tail)
                }
              )
            )
            handler.map(_())
          case Observable.Empty =>
            nonEmptyA.next().map(handleA)
      case Observable.Empty =>
        default match
          case nonEmptyB: Observable.NonEmpty[B] =>
            nonEmptyB.next()
          case Observable.Empty =>
            Future.successful(View.Empty, Observable.Empty)
  }

object Observable:
  object Empty extends Observable[Nothing]
  @FunctionalInterface
  trait NonEmpty[+A] extends Observable[A]:
    def next(): Future[(Iterable[A], Observable[A])]

  trait BehaviorSubject[+A] extends NonEmpty[A]:
    def updatedState(): BehaviorSubject.State[A]
    final def next() = {
      updatedState() match
        case BehaviorSubject.State.Incompleted(head, future) =>
          Future.successful((View.Single(head), (() => future): NonEmpty[A]))
        case BehaviorSubject.State.Completed(head) =>
          Future.successful((View.Single(head), Empty))
    }

  object BehaviorSubject:
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
          future: Future[(Iterable[A], Observable[A])]
      ) extends State[A]:
        def tail: NonEmpty[A] = () => future
      final case class Completed[A](head: A) extends State[A]:
        def tail = Empty

    def pure[A](a: A): BehaviorSubject[A] = (() => State.Completed(a))
    def apply[A](initialValue: A, source: Observable[A]): BehaviorSubject[A] =
      new BehaviorSubject[A]:
        private var state = State(initialValue, source)
        def updatedState() = synchronized {
          @tailrec
          def loop(state: State[A]): State[A] =
            state match
              case State.Incompleted(head, future) =>
                future.value match
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

    given (using ExecutionContext): Monad[BehaviorSubject] with
      def point[A](a: => A): BehaviorSubject[A] = BehaviorSubject.pure(a)

      def bind[A, B](
          fa: BehaviorSubject[A]
      )(mapper: A => BehaviorSubject[B]): BehaviorSubject[B] =
        val cacheA = fa.updatedState()
        val cacheB = mapper(cacheA.head).updatedState()
        BehaviorSubject(
          cacheB.head,
          cacheA.tail.flatMapLatest(mapper, cacheB.tail)
        )
      end bind
    end given
