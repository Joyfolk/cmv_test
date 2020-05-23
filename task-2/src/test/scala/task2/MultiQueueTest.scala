package task2

import org.scalatest.{FreeSpec, MustMatchers}

import scala.annotation.tailrec

class MultiQueueTest extends FreeSpec with MustMatchers {
  "empty queue isEmpty" in {
    val q = MultiQueue.empty[Integer](2)
    q.isEmpty mustBe true
  }

  "empty queue deleteMin must return nulls" in {
    val q = MultiQueue.empty[Integer](2)
    q.deleteMin() mustBe null
    q.deleteMin() mustBe null
  }

  "queue insert should work and deleteMin with single value must return that value" in {
    val q = MultiQueue.empty[Integer](2)
    q.insert(5)
    q.deleteMin() mustBe 5
  }

  "queue with nQueue number two should always return minimum value" in {
    val q = MultiQueue.empty[Integer](2)
    q.insert(5)
    q.insert(3)
    q.insert(1)
    q.insert(7)
    q.insert(9)
    q.insert(11)
    q.insert(13)
    q.deleteMin() mustBe 1
  }

  "size should always be correct" in {
    val q = MultiQueue.empty[Integer](2)
    q.size mustBe 0
    q.insert(5)
    q.insert(3)
    q.size mustBe 2
    q.insert(1)
    q.insert(7)
    q.insert(9)
    q.size mustBe 5
    q.insert(11)
    q.insert(13)
    q.size mustBe 7
    q.deleteMin()
    q.size mustBe 6
    q.deleteMin()
    q.deleteMin()
    q.deleteMin()
    q.deleteMin()
    q.deleteMin()
    q.size mustBe 1
    q.deleteMin()
    q.size mustBe 0
    q.deleteMin()
    q.deleteMin()
    q.deleteMin()
    q.size mustBe 0
  }

  "queue should work with more than 2 queues" in {
    val q = MultiQueue.empty[Integer](5)
    val toAdd = Range(0, 10)
    @tailrec
    def takeN(n: Int, s: Set[Int] = Set()): Set[Int] =
      if (n == 0)
        s
      else if (q.isEmpty)
        throw new IllegalStateException("empty queue")
      else {
        val e = q.deleteMin()
        if (e != null)
          takeN(n - 1, s + e)
        else
          takeN(n, s)
      }
    toAdd.foreach(q.insert(_))
    q.size mustBe 10
    var r = takeN(5)
    q.size mustBe 5
    r ++= takeN(3)
    q.size mustBe 2
    q.isEmpty mustBe false
    r ++= takeN(2)
    q.size mustBe 0
    q.isEmpty mustBe true
    r mustBe toAdd.toSet
  }
}
