package com.thoughtworks.binding

private object SafeBufferJvmOrJs {
  def newBuffer[A] = collection.mutable.ArrayBuffer.empty[A]

  // Used for Scala 2.13
  @inline
  implicit final class ReduceToSizeOps[A] @inline() (buffer: collection.mutable.ArrayBuffer[A]) {
    @inline def reduceToSize(newSize: Int) = {
      buffer.remove(newSize, buffer.size - newSize)
    }
  }
}
