package com.thoughtworks.binding

import scalaz.-\/
import scalaz.Applicative
import scalaz.DList
import scalaz.Equal
import scalaz.FingerTree
import scalaz.Free
import scalaz.IList
import scalaz.Memo
import scalaz.Monad
import scalaz.MonadPlus
import scalaz.Monoid
import scalaz.Nondeterminism
import scalaz.Reducer
import scalaz.StreamT
import scalaz.StreamT.Done
import scalaz.StreamT.Skip
import scalaz.StreamT.Yield
import scalaz.UnitReducer
import scalaz.\/-

import scala.concurrent.Future
import scala.collection.IndexedSeqView
import scalaz.Functor
import scalaz.StreamT.Step
import scala.annotation.unchecked.uncheckedVariance
import scala.util.Success
import org.scalajs.dom.Node
import BindingSeqT.Patch
import scala.annotation.tailrec
import org.scalajs.dom.Element

final case class NodeBinding[+A](value: A, eventLoop: Binding[Nothing])

object NodeBinding:
  def mount[A](a: A, events: Binding[A => Unit])(using
      N: Functor[Binding.Awaitable]
  ): Binding[Nothing] =
    val stream = BindingT.apply.flip(events)
    def mapEvents(
        stream: StreamT[Binding.Awaitable, A => Unit]
    ): StreamT[Binding.Awaitable, Nothing] =
      StreamT(N.map(stream.step) {
        case Yield(f, s) =>
          f(a)
          Skip(() => mapEvents(s()))
        case Skip(s) =>
          Skip(() => mapEvents(s()))
        case Done() =>
          Done()
      })
    mapEvents(stream)

  def mountChildNodes(
      parent: Node,
      childNodes: Binding.BindingSeq[Node]
  )(using N: Functor[Binding.Awaitable]): Binding[Nothing] =
    val patchStream = BindingT.apply.flip(BindingSeqT.apply.flip(childNodes))
    def mapEvents(
        patchStream: StreamT[Binding.Awaitable, Patch[Node]]
    ): StreamT[Binding.Awaitable, Nothing] =
      StreamT(N.map(patchStream.step) {
        case Yield(
              Patch.Splice(
                index,
                deleteCount,
                newItems
              ),
              s
            ) =>
          val childNodes = parent.childNodes
          if (index < childNodes.length) then
            val refChild = parent.childNodes(index)
            for child <- newItems do parent.insertBefore(child, refChild)
            @tailrec
            def delete(remaining: Int, refChild: Node): Unit =
              if remaining > 0 then
                val nextSibling = refChild.nextSibling
                parent.removeChild(refChild)
                delete(remaining - 1, nextSibling)
            delete(deleteCount, refChild)
          else if (index == childNodes.length && deleteCount == 0)
            for child <- newItems do parent.appendChild(child)
          else
            throw new IllegalStateException
          Skip(() => mapEvents(s()))
        case Skip(s) =>
          Skip(() => mapEvents(s()))
        case Done() =>
          Done()
      })
    mapEvents(patchStream)

  given [Element](using
      Applicative[Binding.Awaitable]
  ): BindableSeq[NodeBinding[Element], Element] = BindableSeq { nodeBinding =>
    BindingSeqT(
      BindingT(
        BindingSeqT.Patch.Splice[Element](
          0,
          0,
          collection.View.Single(nodeBinding.value)
        ) :: BindingT.apply.flip(nodeBinding.eventLoop)
      )
    )
  }
