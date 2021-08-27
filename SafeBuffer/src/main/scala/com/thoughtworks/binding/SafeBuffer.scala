package com.thoughtworks.binding

import com.thoughtworks.{enableIf, enableMembersIf}

import scala.annotation.tailrec
import scala.collection.mutable

private[binding] object SafeBuffer {

  @enableMembersIf(c => !c.compilerSettings.exists(_.matches("""^-Xplugin:.*scalajs-compiler_[0-9\.\-]*\.jar$""")))
  private[SafeBuffer] object Jvm {
    def newBuffer[A] = collection.mutable.ArrayBuffer.empty[A]

    // Used for Scala 2.13
    @inline
    implicit final class ReduceToSizeOps[A] @inline() (buffer: collection.mutable.ArrayBuffer[A]) {
      @inline def reduceToSize(newSize: Int) = {
        buffer.remove(newSize, buffer.size - newSize)
      }
    }
  }

  @enableMembersIf(c => c.compilerSettings.exists(_.matches("""^-Xplugin:.*scalajs-compiler_[0-9\.\-]*\.jar$""")))
  private[SafeBuffer] object Js {

    @inline
    def newBuffer[A] = new scalajs.js.Array[A]

    @inline
    implicit final class ReduceToSizeOps[A] @inline() (array: scalajs.js.Array[A]) {
      @inline def reduceToSize(newSize: Int) = array.length = newSize
    }

  }

  private[SafeBuffer] sealed trait State

  case object Idle extends State

  case object CleanForeach extends State

  case object DirtyForeach extends State

  final val Hole = new AnyRef

}

/** Similar to [[scala.collection.mutable.ArrayBuffer]], except that this [[SafeBuffer]] allows adding or removing
  * elements via [[+=]] and [[-=]] inside a [[foreach]] block.
  *
  * @note
  *   A [[java.lang.IllegalStateException]] will be thrown when invoking methods other than [[+=]] and [[-=]] in a
  *   [[foreach]] block.
  */
final class SafeBuffer[A] extends mutable.Buffer[A] {

  import SafeBuffer._

  import Js._
  import Jvm._
  private val data = newBuffer[Any]

  @volatile
  private var state: State = Idle

  @inline
  override def isEmpty = data.forall(_ == Hole)

  override def foreach[U](f: A => U): Unit = {
    state match {
      case Idle =>
        state = CleanForeach
        data.withFilter(_ != Hole).foreach(f.asInstanceOf[Any => U])
        state match {
          case DirtyForeach => {
            @tailrec
            def compact(i: Int, j: Int): Unit = {
              if (i < data.length) {
                val x = data(i)
                if (x == Hole) {
                  compact(i + 1, j)
                } else {
                  data(j) = x
                  compact(i + 1, j + 1)
                }
              } else {
                data.reduceToSize(j)
              }
            }
            compact(0, 0)
            state = Idle
          }
          case CleanForeach =>
            state = Idle
          case Idle =>
            throw new IllegalStateException("Expect CleanForeach or DirtyForeach")
        }
      case CleanForeach | DirtyForeach =>
        data.withFilter(_ != Hole).foreach(f.asInstanceOf[Any => U])
    }
  }

  def addOne(x: A): this.type = {
    data += x
    this
  }

  @inline
  override def subtractOne(x: A): this.type = {
    state match {
      case Idle =>
        data -= x
      case CleanForeach =>
        data(data.indexOf(x)) = Hole
        state = DirtyForeach
      case DirtyForeach =>
        data(data.indexOf(x)) = Hole
    }
    this
  }

  private def checkIdle() = {
    if (Idle != state)
      throw new IllegalStateException(
        "Not allowed to invoke methods other than `+=` and `-=` when `foreach` is running."
      )
  }

  def apply(n: Int): A = {
    checkIdle()
    data(n).asInstanceOf[A]
  }

  def clear(): Unit = {
    checkIdle()
    data.clear()
  }

  def insert(idx: Int, elem: A): Unit = {
    checkIdle()
    data.insert(idx, elem)
  }

  def insertAll(idx: Int, elems: scala.collection.IterableOnce[A]): Unit = {
    checkIdle()
    data.insertAll(idx, elems)
  }

  def patchInPlace(from: Int, patch: scala.collection.IterableOnce[A], replaced: Int): this.type = {
    checkIdle()
    data.patchInPlace(from, patch, replaced)
    this
  }

  def prepend(elem: A): this.type = {
    checkIdle()
    data.prepend(elem)
    this
  }

  def remove(idx: Int, count: Int): Unit = {
    checkIdle()
    data.remove(idx, count)
  }

  def insertAll(n: Int, elems: Iterable[A]): Unit = {
    checkIdle()
    data.insertAll(n, elems)
  }

  def length: Int = {
    checkIdle()
    data.length
  }

  def remove(n: Int): A = {
    checkIdle()
    data.remove(n).asInstanceOf[A]
  }

  def update(n: Int, newelem: A): Unit = {
    checkIdle()
    data.update(n, newelem)
  }

  def iterator: Iterator[A] = {
    checkIdle()
    data.iterator.asInstanceOf[Iterator[A]]
  }
}
