package com.thoughtworks.binding

private[binding] object BindingJvmOrJs {

  type ConstantsData[+A] = Seq[A]

  @inline
  def toConstantsData[A](seq: IterableOnce[A]) = Seq.from(seq)

  def toCacheData[A](seq: collection.Iterable[A]) = Vector.from(seq)

  def emptyCacheData[A]: HasCache[A]#Cache = Vector.empty

  trait HasCache[A] {

    private[binding] type Cache = Vector[A]

    private[binding] var cacheData: Cache

    @inline
    private[binding] final def getCache(n: Int): A = cacheData(n)

    @inline
    private[binding] final def updateCache(n: Int, newelem: A): Unit = {
      cacheData = cacheData.updated(n, newelem)
    }

    @inline
    private[binding] final def cacheLength: Int = cacheData.length

    @inline
    private[binding] final def clearCache(): Unit = {
      cacheData = Vector.empty
    }

    private[binding] final def removeCache(n: Int): A = {
      val result = cacheData(n)
      cacheData = cacheData.patch(n, Nil, 1)
      result
    }

    private[binding] final def removeCache(idx: Int, count: Int): Unit = {
      cacheData = cacheData.patch(idx, Nil, count)
    }

    private[binding] final def appendCache(
        elements: IterableOnce[A]
    ): Seq[A] = {
      val seq = Seq.from(elements)
      cacheData = cacheData ++ seq
      seq
    }

    private[binding] final def appendCache(elem: A): Unit = {
      cacheData = cacheData :+ elem
    }

    private[binding] final def prependCache(elem: A): Unit = {
      cacheData = elem +: cacheData
    }

    private[binding] final def insertCache(
        n: Int,
        elems: IterableOnce[A]
    ): Seq[A] = {
      val seq = Seq.from(elems)
      cacheData = cacheData.patch(n, seq, 0)
      seq
    }

    private[binding] final def insertOneCache(n: Int, elem: A): Seq[A] = {
      val seq = Seq(elem)
      cacheData = cacheData.patch(n, seq, 0)
      seq
    }

    private[binding] final def cacheIterator: Iterator[A] = {
      cacheData.iterator
    }

    private[binding] final def spliceCache(
        from: Int,
        mappedNewChildren: IterableOnce[A],
        replaced: Int
    ) = {
      val oldCache = cacheData
      if (from == 0) {
        cacheData = mappedNewChildren ++: oldCache.drop(replaced)
      } else {
        cacheData = oldCache.patch(from, mappedNewChildren, replaced)
      }
      oldCache.view.slice(from, from + replaced)
    }

    private[binding] final def indexOfCache[B >: A](a: B): Int = {
      cacheData.indexOf(a)
    }

  }

}
