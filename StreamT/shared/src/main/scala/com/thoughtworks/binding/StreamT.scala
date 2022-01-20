package com.thoughtworks.binding

import scalaz.-\/
import scalaz.Applicative
import scalaz.Equal
import scalaz.FingerTree
import scalaz.Functor
import scalaz.ICons
import scalaz.IList
import scalaz.INil
import scalaz.Isomorphism.IsoFunctor
import scalaz.Isomorphism.IsoFunctorTemplate
import scalaz.IsomorphismMonadPlus
import scalaz.Monad
import scalaz.MonadPlus
import scalaz.Monoid
import scalaz.Nondeterminism
import scalaz.StreamT
import scalaz.StreamT.Done
import scalaz.StreamT.Skip
import scalaz.StreamT.Step
import scalaz.StreamT.Yield
import scalaz.\/-
import scalaz.ReaderT
import scalaz.Reducer
import scalaz.EphemeralStream

import scala.concurrent.Future
private[binding] type StreamT[M[_], A] = scalaz.StreamT[M, A]
private[binding] object StreamT:
  export scalaz.StreamT.*

  extension [M[_], A](fa: StreamT[M, A])
    private[binding] def stepMap[B](
        f: Step[A, StreamT[M, A]] => Step[B, StreamT[M, B]]
    )(implicit
        M: Functor[M]
    ): StreamT[M, B] = StreamT(M.map(fa.step)(f))

    def memoize(implicit m: Functor[M]): StreamT[M, A] = stepMap {
      case Yield(a, s) => Yield(a, EphemeralStream.weakMemo(s()))
      case Skip(s)     => Skip(EphemeralStream.weakMemo(s()))
      case Done()      => Done()
    }

    def noSkip(implicit M: Monad[M]): StreamT[M, A] =
      StreamT(M.bind(fa.step) {
        case Yield(a, s) =>
          M.point(Yield(a, s().noSkip))
        case Skip(s) =>
          s().noSkip.step
        case Done() =>
          M.point(Done())
      })
  end extension
