package task1.modifyme

/**
 * A priority queue of priorities `K` to values `V`.
 */
trait PriorityQueue[K, V] {
  // returns `true` if nothing is enqueued in the queue
  def isEmpty: Boolean
  // removes the element of the lowest priority
  def deleteMin(): Unit
  // retrieves the priority of the lowest element
  def priority: K
  // retrieves the element of the lowest priority
  def min: V
  // adds the element `v` with priority `k`
  def enqueue(k: K, v: V): Unit
}

object PriorityQueue {
  def intPriority[V]: PriorityQueue[Int, V] = {
    new JavaQueue[V]()
  }
}

class JavaQueue[V] extends PriorityQueue[Int, V] {
  type Entry = (Int, V)
  val pq = new java.util.PriorityQueue[Entry]((o1: (Int, V), o2: (Int, V)) => Integer.compare(o1._1, o2._1))

  override def isEmpty: Boolean = pq.isEmpty
  override def deleteMin(): Unit = {
    pq.poll()
    ()
  }

  override def priority: Int = pq.peek()._1
  override def min: V = pq.peek()._2
  override def enqueue(k: Int, v: V): Unit = {
    pq.add((k, v))
    ()
  }
}

class Queue[V] private (
    underlying: scala.collection.mutable.PriorityQueue[Queue.Entry[V]])
  extends PriorityQueue[Int, V] {

  private[this] var entry: Queue.Entry[V] = null

  @inline private[this] def maybeUpdateEntry(): Unit = {
    if (entry == null) {
      entry = underlying.dequeue()
    }
  }

  def isEmpty = entry == null && underlying.isEmpty

  def deleteMin(): Unit = {
    if (underlying.isEmpty) {
      entry = null
    } else {
      entry = underlying.dequeue()
    }

    ()
  }


  def priority: Int = {
    maybeUpdateEntry()
    entry.priority
  }
  
  def min: V = {
    maybeUpdateEntry()
    entry.value
  }

  def enqueue(k: Int, v: V): Unit = {
    val queued = Queue.Entry(k, v)

    if (entry == null) {
      entry = queued
    } else if (entry.priority > k) {
      underlying.enqueue(entry)
      entry = queued
    } else {
      underlying.enqueue(queued)
    }

    ()
  }
}

object Queue {
  def empty[V]: Queue[V] = {
    new Queue[V](new scala.collection.mutable.PriorityQueue)
  }

  case class Entry[V](priority: Int, value: V) extends Ordered[Entry[V]] {
    def compare(that: Entry[V]): Int = - this.priority.compare(that.priority)
  }
}
