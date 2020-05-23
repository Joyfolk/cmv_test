package task2
import java.util.concurrent.atomic.{AtomicInteger, AtomicIntegerArray}

import scala.annotation.tailrec
import scala.util.Random

class MultiQueue[A >: Null] private (
  // nQueues is guaranteed to be at least 2
  nQueues: Int,
  // other arguments here if needed
  rng: Random = new Random() // would be thread contention point in real app, replace with threadlocal instances or something like that
  )(implicit ordering: Ordering[A]) {

  private val queues: Array[Queue[A]] = Array.fill(nQueues)(Queue.empty)
  private val locks: AtomicIntegerArray = new AtomicIntegerArray(nQueues)
  private val _size: AtomicInteger = new AtomicInteger(0)

  def isEmpty: Boolean = size == 0
  def size: Int        = _size.get()

  def insert(element: A): Unit = {
    withRandomLock { n =>
      queues(n).enqueue(element)
      _size.incrementAndGet()
      ()
    }
  }

  @tailrec
  private def withRandomLock[T](f: Int => T): T =
    acquireAndDo(rng.nextInt(nQueues))(f) match {
      case Some(x) => x
      case None    => withRandomLock(f)
    }

  private def acquireAndDo[T](n: Int)(f: Int => T): Option[T] =
    if (locks.compareAndSet(n, 0, 1))
      try Some(f(n)) finally locks.set(n, 0)
    else
      None

  @tailrec
  private def with2RandomLocks[T](f: (Int, Int) => T): T = {
    val n = (rng.nextInt(nQueues), rng.nextInt(nQueues))
    if (n._1 == n._2)
      with2RandomLocks(f)
    else {
      val m = if (n._1 < n._2) n else n.swap
      acquireAndDo(m._1) { n1 =>
        acquireAndDo(m._2) { n2 =>
          val r = f(n1, n2)
          if (r != null)
            _size.decrementAndGet()
          r
        }
      }.flatten match {
        case Some(v) => v
        case None => with2RandomLocks(f)
      }
    }
  }

  /*
   * Smallest elements (non-strictly) first.
   */
  def deleteMin(): A = with2RandomLocks { (n1, n2) =>
    if (queues(n1).peekMin != null && (queues(n2).peekMin == null
      || queues(n2).peekMin != null && ordering.compare(queues(n1).peekMin, queues(n2).peekMin) <= 0))
      queues(n1).deleteMin()
    else
      queues(n2).deleteMin()
  }
}

object MultiQueue {
  // You can ignore the scaling factor and the actuall amount of processors just use the given nQueues.
  def empty[A >: Null](nQueues: Int)(implicit ordering: Ordering[A]): MultiQueue[A] = {
    new MultiQueue(math.max(2, nQueues))
  }
}
