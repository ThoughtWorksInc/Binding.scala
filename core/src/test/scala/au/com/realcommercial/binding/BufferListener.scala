package au.com.realcommercial.binding

import au.com.realcommercial.binding.Binding.{PatchedEvent, ChangedEvent, PatchedListener, ChangedListener}

import scala.collection.mutable.ArrayBuffer

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class BufferListener extends ArrayBuffer[Any] {
  val listener = new ChangedListener[Seq[Any]] with PatchedListener[Any] {
    override private[binding] def changed(event: ChangedEvent[Seq[Any]]): Unit = {
      BufferListener.this += event
    }

    override private[binding] def patched(event: PatchedEvent[Any]): Unit = {
      BufferListener.this += event
    }
  }
}