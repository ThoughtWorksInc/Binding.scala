package com.thoughtworks.binding

private object SafeBufferJvmOrJs {

  @inline
  def newBuffer[A] = new scalajs.js.Array[A]

  @inline
  implicit final class ReduceToSizeOps[A] @inline() (array: scalajs.js.Array[A]) {
    @inline def reduceToSize(newSize: Int) = array.length = newSize
  }

}
