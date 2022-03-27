package com.thoughtworks.binding

import scalaz.Free
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.annotation.tailrec
import scalaz.Monad
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.View
import com.thoughtworks.binding.Observable.Operator
import scala.util.control.NonFatal
import scala.collection.immutable.ArraySeq
sealed trait Observable[+A]:
  private[binding] final def postScanLeft[B](z: B)(
      op: (B, A) => B
  )(using ExecutionContext): Observable.Operator[B] =
    this match
      case Observable.Empty =>
        Observable.Empty
      case nonEmptyA: Observable.NonEmpty[A] =>
        locally[Observable.Operator.NonEmpty.Lazy[B]] { () =>
          nonEmptyA.next().map { case (head, tail) =>
            val builder = head.iterableFactory.newBuilder[B]
            val headIterator = head.iterator
            var acc = z
            while headIterator.hasNext do
              acc = op(acc, headIterator.next())
              builder += acc
            (builder.result, tail.postScanLeft(acc)(op))
          }
        }
    end match
  end postScanLeft
  final def scanLeft[B](z: B)(op: (B, A) => B)(using
      ExecutionContext
  ): Observable.Operator[B] =
    z +: postScanLeft(z)(op)
  end scanLeft
  final def +:[B >: A](a: B): Observable.Operator.NonEmpty.Eager[B] =
    Observable.Operator.NonEmpty.Eager[B](
      Future.successful((View.Single(a), replay))
    )
  end +:
  final def map[B](mapper: A => B)(using
      ExecutionContext
  ): Observable.Operator[B] =
    this match
      case Observable.Empty =>
        Observable.Empty
      case nonEmptyA: Observable.NonEmpty[A] =>
        locally[Observable.Operator.NonEmpty.Lazy[B]] { () =>
          nonEmptyA.next().map { case (head, tail) =>
            (head.map(mapper), tail.map(mapper))
          }
        }
    end match
  end map
  final def flatMapLatest[B](
      mapper: A => Observable[B],
      default: Observable[B] = Observable.Operator.Empty
  )(using ExecutionContext): Observable.Operator[B] =
    this match
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
        end handleA
        def handleB(
            head: Iterable[B],
            tail: Observable[B]
        ): (Iterable[B], Observable.Operator[B]) =
          (head, nonEmptyA.flatMapLatest[B](mapper, tail))
        end handleB
        locally[Observable.Operator.NonEmpty.Lazy[B]] { () =>
          default match
            case Observable.Empty =>
              nonEmptyA.next().map(handleA)
            case nonEmptyB: Observable.NonEmpty[B] =>
              Future
                .firstCompletedOf(
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
                .map(_())
        }
    end match
  end flatMapLatest
  final def replay: Observable.Operator[A] =
    this match
      case operator: Observable.Operator[A] =>
        operator
      case nonEmpty: Observable.NonEmpty[A] =>
        locally[Observable.Operator.NonEmpty.Lazy[A]] { () =>
          nonEmpty
            .next()
            .map { case (head, tail) =>
              (head, tail.replay)
            }(ExecutionContext.parasitic)
        }
    end match
  end replay
end Observable
object Observable:
  sealed trait Operator[+A] extends Observable[A]
  object Operator:

    object Empty extends Operator[Nothing]
    sealed trait NonEmpty[+A] extends Operator[A] with Observable.NonEmpty[A]:
      /** The stable memoized value of `next()`, which must be either a
        * non-`lazy` or `lazy val`
        */
      def next(): Future[(Iterable[A], Operator[A])]

    end NonEmpty

    object NonEmpty:
      trait Lazy[+A] extends NonEmpty[A]:
        def computeNext(): Future[(Iterable[A], Operator[A])]
        private lazy val nextLazyVal: Future[(Iterable[A], Operator[A])] =
          try {
            computeNext()
          } catch {
            case NonFatal(e) =>
              Future.failed(e)
          }
        final def next() = nextLazyVal
      end Lazy
      final case class Eager[+A](nextVal: Future[(Iterable[A], Operator[A])])
          extends NonEmpty[A]:
        def next() = nextVal
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
end Observable
