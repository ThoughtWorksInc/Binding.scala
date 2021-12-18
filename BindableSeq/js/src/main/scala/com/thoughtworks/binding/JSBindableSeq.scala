package com.thoughtworks.binding
import com.thoughtworks.dsl.Dsl
import Binding.BindingSeq
import org.scalajs.dom.document
import org.scalajs.dom.Text
import scalaz.Functor
private[binding] trait JSBindableSeq:
  given [Keyword, Value](using
      Dsl.IsKeyword[Keyword, Value],
      Dsl.Run[Keyword, BindingSeq[String], Value],
      Functor[BindingSeq]
  ): BindableSeq[Keyword, Text] = BindableSeq { keyword =>
    Functor[BindingSeq].map(
      summon[Dsl.Run[Keyword, BindingSeq[String], Value]](keyword)
    )(document.createTextNode)
  }
