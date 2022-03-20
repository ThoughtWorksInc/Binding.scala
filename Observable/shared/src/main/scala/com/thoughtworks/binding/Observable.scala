package com.thoughtworks.binding

import scalaz.Free
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.annotation.tailrec
import scalaz.Monad
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.View

sealed trait Observable[+A]:
  @inline final def flatMapLatest[B](
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
  final def replay: Observable.AsyncList[A] =
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
      trait Eager[+A] extends NonEmpty[A]:
        protected val nextValue: Future[(Iterable[A], AsyncList[A])]
      end Eager
    end NonEmpty
  end AsyncList

  val Empty = AsyncList.Empty
  trait NonEmpty[+A] extends Observable[A]:
    /** Returns the next batch of data, which includes a tuple of finished items
      * and the next observable of further batches.
      *
      * @note
      *   This method must be idempotent for both side effects and CPU cost.
      *   Given `callNextOnce` and `callNextTwice` calling [[next]] once and
      *   twice, respectively,
      *   {{{
      *   def callNextOnce[A](observable: Observable[A])(using
      *       ExecutionContext
      *   ): Future[Int] =
      *     val startTime = System.nanoTime()
      *     for
      *       _ <- observable.next()
      *       endTime = System.nanoTime()
      *     yield
      *       endTime - startTime
      *   end
      *   def callNextTwice[A](observable: Observable[A])(using
      *       ExecutionContext
      *   ): Future[Int] =
      *     val startTime = System.nanoTime()
      *     for
      *       _ <- observable.next()
      *       _ <- observable.next()
      *       endTime = System.nanoTime()
      *     yield
      *     endTime - startTime
      *   end
      *   }}}
      *
      * Then the execution time should not be significant different and the side
      * effects performed, if any, should not show any noticeable difference.
      */
    def next(): Future[(Iterable[A], Observable[A])]

  trait BehaviorSubject[+A] extends Observable.NonEmpty[A]:
    def updatedState(): BehaviorSubject.State[A]
    final def next() = {
      updatedState() match
        case BehaviorSubject.State.Incompleted(head, nextValue) =>
          Future.successful(
            (View.Single(head), (() => nextValue): Observable.NonEmpty[A])
          )
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
          case nonEmptyTail: Observable.NonEmpty[A] =>
            State.Incompleted(head, nonEmptyTail.next())

      final case class Incompleted[A](
          head: A,
          nextValue: Future[(Iterable[A], Observable[A])]
      ) extends State[A]:
        def tail: Observable.NonEmpty[A] = () => nextValue
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
  end BehaviorSubject
