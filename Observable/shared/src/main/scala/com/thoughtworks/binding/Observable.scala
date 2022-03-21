package com.thoughtworks.binding

import scalaz.Free
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.annotation.tailrec
import scalaz.Monad
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.View
import com.thoughtworks.binding.Observable.Operator

sealed trait Observable[+A]:
  final def flatMapLatest[B](
      mapper: A => Observable[B],
      default: Observable[B] = Observable.Operator.Empty
  )(using ExecutionContext): Observable.Operator[B] =
    Observable.this match
      case Observable.Empty =>
        default.replay
      case nonEmptyA: Observable.NonEmpty[A] =>
        def handleA(
            head: Iterable[A],
            tail: Observable[A]
        ): (Iterable[B], Observable.Operator[B]) =
          val newDefault = head.lastOption match
            case None =>
              default
            case Some(last) =>
              mapper(last)
          (View.Empty, tail.flatMapLatest(mapper, newDefault))
        def handleB(
            head: Iterable[B],
            tail: Observable[B]
        ): (Iterable[B], Observable.Operator[B]) =
          (head, nonEmptyA.flatMapLatest[B](mapper, tail))
        final class FlatMapLatest extends Observable.Operator.NonEmpty.Lazy[B]:
          lazy val nextValue =
            default match
              case Observable.Empty =>
                nonEmptyA.next().map(handleA)
              case nonEmptyB: Observable.NonEmpty[B] =>
                val handler = Future.firstCompletedOf(
                  Seq(
                    nonEmptyA
                      .next()
                      .map { case (head, tail) =>
                        () => handleA(head, tail)
                      }(using ExecutionContext.parasitic),
                    nonEmptyB
                      .next()
                      .map { case (head, tail) =>
                        () => handleB(head, tail)
                      }(using ExecutionContext.parasitic)
                  )
                )(using ExecutionContext.parasitic)
                handler.map(_())
            end match
        new FlatMapLatest
  end flatMapLatest
  final def replay: Observable.Operator[A] =
    this match
      case asyncList: Observable.Operator[A] =>
        asyncList
      case nonEmpty: Observable.NonEmpty[A] =>
        final class Replay extends Observable.Operator.NonEmpty.Lazy[A]:
          protected lazy val nextValue = nonEmpty
            .next()
            .map { case (head, tail) =>
              (head, tail.replay)
            }(ExecutionContext.parasitic)
        new Replay
  end replay

object Observable:
  sealed trait Operator[+A] extends Observable[A]
  object Operator:

    object Empty extends Operator[Nothing]
    sealed trait NonEmpty[+A] extends Operator[A] with Observable.NonEmpty[A]:
      final def next(): Future[(Iterable[A], Operator[A])] = nextValue

      /** The stable memoized value of `next()`, which must be either a
        * non-`lazy` or `lazy val`
        */
      protected def nextValue: Future[(Iterable[A], Operator[A])]
    end NonEmpty

    object NonEmpty:
      trait Lazy[+A] extends NonEmpty[A]:
        protected lazy val nextValue: Future[(Iterable[A], Operator[A])]
      end Lazy
      trait Eager[+A] extends NonEmpty[A]:
        protected val nextValue: Future[(Iterable[A], Operator[A])]
      end Eager
    end NonEmpty
  end Operator

  val Empty = Operator.Empty
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
