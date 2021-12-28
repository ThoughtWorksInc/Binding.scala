package com.thoughtworks.binding

import scala.concurrent.Future

/** A [[scala.concurrent.Future]] whose scalaz instances are not orphan */
opaque type DefaultFuture[+A] >: Future[A] <: Future[A] = Future[A]
object DefaultFuture:
  export scalaz.std.scalaFuture.*
