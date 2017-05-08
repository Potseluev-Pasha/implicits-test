import java.lang.ref.{ReferenceQueue, WeakReference}

import WeakSet._

import scala.annotation.tailrec

class WeakSet[T](initialCapacity: Int = DEFAULT_CAPACITY, loadFactor: Double = DEFAULT_LOAD_FACTOR) extends MySet[T] {
  require(initialCapacity >= 0, "initial capacity can't be negative")
  require(loadFactor > 0 && loadFactor <= 1, "load factor should be between 0 and 1")

  private[this] val queue: ReferenceQueue[Any] = new ReferenceQueue
  private[this] var buckets = new Array[Entry](calculateCapacity)
  private[this] var size: Int = 0
  private[this] var usedBuckets: Int = 0
  private[this] var threshold: Int = calculateThreshold

  private[this] def calculateCapacity: Int = {
    var capacity = 1
    while (capacity < initialCapacity) capacity *= 2
    capacity
  }

  private[this] def calculateThreshold: Int = (buckets.length * loadFactor).toInt

  private[this] def resizeIfNeed() = if (usedBuckets >= threshold) {
    usedBuckets = 0
    val entries = buckets.flatMap(toList)
    buckets = new Array[Entry](buckets.length * 2)
    threshold = calculateThreshold
    entries.foreach(entry => addEntry(entry, getBucket(entry.hash)))
  }

  override def add(t: T): Unit = {
    cleanUp()
    requireNonNull(t)
    val (hash, bucket) = getHashAndBucket(t)
    if (!contains(t, hash, bucket)) {
      val entry = new Entry(hash)
      addEntry(entry withValue weakWrap(t, entry), bucket)
      size += 1
      resizeIfNeed()
    }
  }

  private[this] def addEntry(entry: Entry, bucket: Int): Unit = {
    val entryInBucket = buckets(bucket)
    buckets(bucket) = entry withNext entryInBucket
    if (entryInBucket == null) usedBuckets += 1
  }

  override def contains(t: T): Boolean = {
    cleanUp()
    requireNonNull(t)
    val (hash, bucket) = getHashAndBucket(t)
    contains(t, hash, bucket)
  }

  private[this] def getHashAndBucket(t: T): (Int, Int) = {
    val hash = calcHash(t.hashCode())
    (hash, getBucket(hash))
  }

  //applies a supplemental hash function to a given hashCode (copied from java.util.HashMap)
  private[this] def calcHash(hashCode: Int) = {
    var hash = hashCode
    hash ^= hash >>> 20 ^ hash >>> 12
    hash ^= hash >>> 7 ^ hash >>> 4
    hash
  }

  private[this] def getBucket(hash: Int): Int = {
    hash & (buckets.length - 1)
  }

  private[this] def contains(t: T, hash: Int, bucket: Int): Boolean = {
    @tailrec
    def contains(t: T, cur: Entry): Boolean = cur match {
      case null => false
      case e: Entry => e.hash == hash && e.value == t || contains(t, e.next)
    }

    contains(t, buckets(bucket))
  }

  private[this] def remove(bucket: Int)(p: Entry => Boolean): Unit = {
    @tailrec
    def remove(prev: Entry, cur: Entry): Unit = cur match {
      case null => ()
      case entry: Entry => if (p(entry)) {
        prev.next = entry.next
        size -= 1
      } else remove(entry, entry.next)
    }

    buckets(bucket) match {
      case null => ()
      case e: Entry => if (p(e)) {
        buckets(bucket) = e.next
        size -= 1
        if (e.next == null) usedBuckets -= 1
      } else remove(e, e.next)
    }
  }

  private[this] def weakWrap(input: Any, entry: Entry): Any = input match {
    case (x, y) => (weakWrap(x, entry), weakWrap(y, entry))
    case x => new MyWeakRef(x, queue, entry)
  }

  @tailrec
  private[this] def cleanUp(): Unit = Option(queue.poll().asInstanceOf[MyWeakRef[Any]]).map(_.entry) match {
    case None => ()
    case Some(entry) if entry.value == null => cleanUp()
    case Some(entry) => remove(getBucket(entry.hash))(entry eq _)
      entry.value = null
      cleanUp()
  }
}

object WeakSet {
  private val DEFAULT_CAPACITY = 16
  private val DEFAULT_LOAD_FACTOR = 0.75

  private def toList(entry: Entry): List[Entry] = {
    @tailrec
    def loop(entry: Entry, acc: List[Entry]): List[Entry] = entry match {
      case null => acc
      case e: Entry => loop(e.next, e :: acc)
    }

    loop(entry, List())
  }

  private def requireNonNull(value: Any) = if (value == null) throw new NullPointerException("value can't be null")

  private class Entry(val hash: Int) {
    var next: Entry = _
    var value: Any = _

    private[WeakSet] def withValue(value: Any) = {
      this.value = value
      this
    }

    private[WeakSet] def withNext(next: Entry) = {
      this.next = next
      this
    }
  }

  private class MyWeakRef[T](t: T, queue: ReferenceQueue[T], val entry: Entry) extends WeakReference[T](t, queue) {
    /**
      * Not symmetric!! Only for internal usage
      */
    override def equals(obj: scala.Any): Boolean = obj == get()
  }

}