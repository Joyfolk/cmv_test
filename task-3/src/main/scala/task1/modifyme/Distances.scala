package task1.modifyme
import scala.collection.mutable

/**
 * Stores the distances (of type `W) to the labels `A`.
 */
abstract class Distances[A, W] {
  /*
   * Retrieves the stored distance `W` at label `W` or Infinity[W] if not reached.
   */
  def distanceAt(a: A): W
  /*
   * Updates the distance for label `A` with the value `W`
   */
  def updated(label: A, value: W): Unit
}

object Distances {
  def empty(implicit ev: Infinity[Int]): Distances[Int, Int] = new MapDistances
}

class MapDistances(implicit ev: Infinity[Int]) extends Distances[Int, Int] {
  var underlying = new mutable.LongMap[Int]
  val inf: Int = Infinity[Int]

  def distanceAt(a: Int): Int = {
    underlying.getOrElse(a.toLong, inf)
  }

  def updated(label: Int, value: Int): Unit = {
    underlying.update(label.toLong, value)
  }
}

