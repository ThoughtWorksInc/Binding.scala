package com.thoughtworks.binding.async

import cats.effect.{Effect, IO, SyncIO}
import com.thoughtworks.binding.Binding
import com.thoughtworks.binding.Binding.Var


class AsyncBinding[T, F[_] : Effect](task: F[T]) extends Binding[AsyncState[T]] {

  private val state: Var[AsyncState[T]] = Var(AsyncState.NotStarted)

  def refresh(): Unit = {
    val start = Effect[F].runAsync(task) {
      case Left(err) => IO(state.value = AsyncState.Failure(err))
      case Right(value) => IO(state.value = AsyncState.Success(value))
    }
    SyncIO(state.value = AsyncState.Pending)
      .flatMap(_ => start)
      .unsafeRunSync()
  }


  override private[binding] def value: AsyncState[T] = state.value

  override private[binding] def removeChangedListener(listener: Binding.ChangedListener[AsyncState[T]]): Unit = state.removeChangedListener(listener)

  override private[binding] def addChangedListener(listener: Binding.ChangedListener[AsyncState[T]]): Unit = state.addChangedListener(listener)
}