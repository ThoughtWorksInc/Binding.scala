package com.thoughtworks.binding

private[binding] object BindingJvmOrJs {

  type ConstantsData[+A] = scalajs.js.Array[_ <: A]

  @inline
  def toConstantsData[A](seq: IterableOnce[A]) = {
    import scalajs.js.JSConverters._
    seq.toJSArray
  }

  @inline
  def toCacheData[A](seq: collection.Iterable[A]) = {
    import scalajs.js.JSConverters._
    seq.toJSArray
  }

  @inline
  def emptyCacheData[A]: HasCache[A]#Cache = scalajs.js.Array()

  trait HasCache[A] {

    private[binding] type Cache = scalajs.js.Array[A]

    private[binding] def cacheData: Cache

    @inline
    private[binding] final def getCache(n: Int): A = cacheData(n)

    @inline
    private[binding] final def updateCache(n: Int, newelem: A): Unit = {
      cacheData(n) = newelem
    }

    @inline
    private[binding] final def cacheLength: Int = cacheData.length

    @inline
    private[binding] final def clearCache(): Unit = {
      cacheData.length = 0
    }

    @inline
    private[binding] final def removeCache(n: Int): A = {
      cacheData.remove(n)
    }

    private[binding] final def removeCache(idx: Int, count: Int): Unit = {
      cacheData.remove(idx, count)
    }

    @inline
    private[binding] final def appendCache(elements: IterableOnce[A]): Seq[A] = {
      val seq = Seq.from(elements)
      cacheData ++= seq
      seq
    }

    @inline
    private[binding] final def appendCache(elem: A): Unit = {
      cacheData += elem
    }

    @inline
    private[binding] final def prependCache(elem: A): Unit = {
      cacheData.unshift(elem)
    }

    @inline
    private[binding] final def insertOneCache(n: Int, elem: A): Seq[A] = {
      cacheData.insert(n, elem)
      Seq(elem)
    }

    @inline
    private[binding] final def insertCache(n: Int, elems: IterableOnce[A]): Seq[A] = {
      val seq = Seq.from(elems)
      cacheData.insertAll(n, elems)
      seq
    }

    @inline
    private[binding] final def cacheIterator: Iterator[A] = {
      cacheData.iterator
    }

    @inline
    private[binding] final def spliceCache(from: Int, mappedNewChildren: Cache, replaced: Int) = {
      cacheData.splice(from, replaced, scalajs.runtime.toScalaVarArgs(mappedNewChildren): _*)
    }

    @inline
    private[binding] final def spliceCache(from: Int, mappedNewChildren: IterableOnce[A], replaced: Int) = {
      cacheData.splice(from, replaced, Seq.from(mappedNewChildren): _*)
    }

    @inline
    private[binding] final def indexOfCache(a: A): Int = {
      cacheData.indexOf(a)
    }

  }
}
