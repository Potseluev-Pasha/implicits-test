import java.lang.ref.{ReferenceQueue, WeakReference}

import WeakSet._

import scala.annotation.tailrec

class WeakSet[T] extends MySet[T] {
  private[this] val initialCapacity = DEFAULT_SIZE
  private[this] val loadFactor = DEFAULT_LOAD_FACTOR
  private[this] val queue: ReferenceQueue[Any] = new ReferenceQueue[Any]
  private[this] var buckets = Array.fill[Entry](initialCapacity)(Empty)
  private[this] var size: Int = 0
  private[this] var usedBuckets: Int = 0

  private[this] def resizeIfNeed() = {
    if (usedBuckets >= buckets.length * loadFactor) {
      usedBuckets = 0
      val entries = buckets.flatMap(toList)
      buckets = Array.fill[Entry](buckets.length * 2)(Empty)
      entries.foreach(entry => addEntry(entry withBucket getBucket(entry.hash)))
    }
  }

  override def add(t: T): Unit = {
    cleanUp()
    resizeIfNeed()
    require(t != null, "value can't be null")
    val (hash, bucket) = getHashAndBucket(t)
    if (!contains(t, hash, bucket)) {
      val entry = NonEmpty(hash, bucket)
      addEntry(entry withValue weakWrap(t, entry))
      size += 1
    }
  }

  private[this] def addEntry(entry: NonEmpty): Unit = {
    val entryInBucket = buckets(entry.bucket)
    buckets(entry.bucket) = entry withNext entryInBucket
    if (entryInBucket == Empty)
      usedBuckets += 1
  }

  override def contains(t: T): Boolean = {
    cleanUp()
    require(t != null, "value can't be null")
    val (hash, bucket) = getHashAndBucket(t)
    contains(t, hash, bucket)
  }

  private[this] def getHashAndBucket(t: T): (Int, Int) = {
    val hash = t.hashCode()
    (hash, getBucket(hash))
  }

  private[this] def getBucket(hashCode: Int): Int = math.abs(hashCode % buckets.length)

  private[this] def contains(t: T, hashCode: Int, bucket: Int): Boolean = {
    @tailrec
    def contains(t: T, cur: Entry): Boolean = cur match {
      case Empty => false
      case NonEmpty(hash, b, next, value) => hash == hashCode && value == t || contains(t, next)
    }

    contains(t, buckets(bucket))
  }

  private[this] def remove(bucket: Int)(p: NonEmpty => Boolean): Unit = {
    @tailrec
    def remove(prev: NonEmpty, cur: Entry): Unit = cur match {
      case Empty =>
      case entry: NonEmpty => if (p(entry)) {
        prev.next = entry.next
        size -= 1
      } else remove(entry, entry.next)
    }

    buckets(bucket) match {
      case Empty =>
      case e: NonEmpty => if (p(e)) {
        buckets(bucket) = e.next
        if (e.next == Empty)
          usedBuckets -= 1
        size -= 1
      } else remove(e, e.next)
    }
  }

  private[this] def weakWrap(input: Any, entry: NonEmpty): Any = input match {
    case (x, y) => (weakWrap(x, entry), weakWrap(y, entry))
    case x => new MyWeakRef(x, queue, entry)
  }

  @tailrec
  private[this] def cleanUp(): Unit = {
    val ref = queue.poll().asInstanceOf[MyWeakRef[Any]]
    if (ref != null) {
      remove(ref.entry.bucket)(ref.entry eq _)
      cleanUp()
    }
  }
}

object WeakSet {
  private val DEFAULT_SIZE = 10
  private val DEFAULT_LOAD_FACTOR = 0.75

  private def toList(entry: Entry): List[NonEmpty] = {
    @tailrec
    def loop(entry: Entry, acc: List[NonEmpty]): List[NonEmpty] = entry match {
      case Empty => acc
      case e: NonEmpty => loop(e.next, e :: acc)
    }

    loop(entry, List())
  }

  private class Entry

  private case class NonEmpty(hash: Int, var bucket: Int, var next: Entry = Empty, var value: Any = null) extends Entry {
    private[WeakSet] def withValue(value: Any) = {
      this.value = value
      this
    }

    private[WeakSet] def withNext(next: Entry) = {
      this.next = next
      this
    }

    private[WeakSet] def withBucket(bucket: Int) = {
      this.bucket = bucket
      this
    }
  }

  private object Empty extends Entry

  private class MyWeakRef[T](t: T, queue: ReferenceQueue[T], val entry: NonEmpty) extends WeakReference[T](t, queue) {
    /**
      * Not symmetric!! Only for internal usage
      */
    override def equals(obj: scala.Any): Boolean = obj == get()
  }

}