package com.thoughtworks.binding

import scala.concurrent.*
import scalaz.*

/** A [[scala.concurrent.Future]] whose scalaz instances are not orphan
  * @todo
  *   Replace `Future` with `Coyoneda[F, _]` once
  *   https://github.com/scalaz/scalaz/pull/2269 is merged
  */
opaque type DefaultFuture[+A] >: Future[A] <: Future[A] = Future[A]
object DefaultFuture:
  export scalaz.std.scalaFuture.*
