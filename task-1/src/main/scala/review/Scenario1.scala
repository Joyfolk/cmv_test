package review

import donotmodifyme.Scenario1._
import _root_.cats.effect.Sync
import cats.syntax.all._
import eu.timepit.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.types.numeric.NonNegInt

object Scenario1 {
  /*
   * Given a `blackBoxPositiveInt` 
   *  - a black box procedure returning `Int` that's outside of our control (i.e an external library call
   * or a call to a service)
   *
   * @return the amount of calls to `blackBoxPositiveInt` needed, so that the sum of all returned values from
   * `blackBoxPositiveInt` would be equal to @input `total` 
   *
   * blackBoxPositiveInt:
   *  - can side effect
   *  - is delivered by a third party, we don't know how it operates
   */
  /*
    Added refined types to make clear, that function can only accept and return non-negative integers
    Also, because blackBoxPositiveInt can have side effects it is wrapped in Sync
    Runtime checks against NonNegative refinement should prevent non-expected behavior inside helper.
   */
  def process[F[_]: Sync](total: NonNegInt): F[NonNegInt] = {
    helper(total, 0)
  }

  private def nonNegInt[F[_]: Sync](i: Int): F[NonNegInt] =
    implicitly[Sync[F]].fromEither(refineV[NonNegative](i).left.map(new RuntimeException(_))) // better to use custom exception

  private def helper[F[_]: Sync](total: NonNegInt, n: NonNegInt): F[NonNegInt] = {
    val s = implicitly[Sync[F]]
    if (total == refineMV[NonNegative](0))
      s.pure(n)
    else {
      for {
        r <- nonNegInt(blackBoxPositiveInt)
        newTotal <- nonNegInt(total - r)
        newN <- nonNegInt(n + 1)
        res <- helper(newTotal, newN)
      } yield res
    }
  }
}
