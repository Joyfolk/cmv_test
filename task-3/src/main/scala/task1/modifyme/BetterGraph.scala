package task1.modifyme
import task1.GenericGraph

import scala.collection.mutable

object BetterGraph {
  def apply(graph: GenericGraph[Int, Int]): Graph[Int, Int] = {
    val m = new mutable.LongMap[Array[Int]]()
    graph.points.foreach { case (v, n) =>
      val b = new mutable.ArrayBuilder.ofInt()
      n.foreach((l, w) => { b += l; b += w })
      m += v.toLong -> b.result()
    }
    new IntGraph(m)
  }

  private class IntGraph(internal: mutable.LongMap[Array[Int]]) extends Graph[Int, Int] {
    override def neighbours(label: Int): Neighbours[Int, Int] =
      internal.get(label.toLong).map(mkNeighbours).getOrElse(emptyNeighbours)
  }

  private val emptyNeighbours = new Neighbours[Int, Int] {
    override def foreach(f: (Int, Int) => Unit): Unit = {}
  }

  private def mkNeighbours(a: Array[Int]): Neighbours[Int, Int] =
    (f: (Int, Int) => Unit) => {
      val iter = a.iterator
      var l = -1
      var w = -1
      var i = 0
      while (iter.hasNext) {
        if (i % 2 == 0)
          l = iter.next()
        else {
          w = iter.next()
          f(l, w)
        }
        i += 1
      }
    }
}
