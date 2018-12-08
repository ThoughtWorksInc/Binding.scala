package com.thoughtworks.binding.async

sealed trait AsyncState[+T] {
  def get: Option[T]
}

object AsyncState {

  //TODO naming
  case object NotStarted extends AsyncState[Nothing] {
    override def get: Option[Nothing] = None
  }

  case object Pending extends AsyncState[Nothing] {
    override def get: Option[Nothing] = None
  }

  case class Success[T](value: T) extends AsyncState[T] {
    override val get: Option[T] = Some(value)
  }

  case class Failure(error: Throwable) extends AsyncState[Nothing] {
    override def get: Option[Nothing] = None
  }

}