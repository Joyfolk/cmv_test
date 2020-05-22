package design
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.W
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined._

object Scenario3 {
  /**
   * Write a class to represent and create a user
   * 
   * - User has a name/surname
   * - User has a username
   * - Username can only consist of characters "[a-z][A-Z][0-9]-._"
   * - User has a level
   * - User starts from level 0 and can only increase.
   * - User has experience
   * - User gets experience each time he posts or is reposted. 
   * - The experience transfers to levels on midnight each day
   * - The experience can't ever be negative
   * - An user is either a free user or a paid user.
   * - A free user has a limit to the amount of posts he can write per day.
   * - A paid user has a counter of the remaining paid days
   */
  type UsernameSpec = MatchesRegex[W.`"""^[[a-z][A-Z][0-9]\\-\\._]+$"""`.T]
  type Username = String Refined UsernameSpec
  type Level = NonNegInt
  type Experience = NonNegInt
  type Name = NonEmptyString

  sealed trait User {
    def username: Username
    def name: Name
    def surname: Name
    def level: Level
    def experience: Experience
  }

  case class FreeUser (
    username: Username,
    name: Name,
    surname: Name,
    level: Level,
    experience: Experience,
    postsLeft: NonNegInt
  ) extends User

  case class PaidUser (
    username: Username,
    name: Name,
    surname: Name,
    level: Level,
    experience: Experience,
    daysRemaining: NonNegInt
  ) extends User

  object UserLogic {
    /*
     * This logic will be run each midnight every day. It should:
     *   1) give a level for each 1000exp, the remaining experience goes to the next day
     *   2) if a free user is under 3 posts refresh the number of posts he can publish to 3
     *   3) paid users reduce their days remaining count
     *
     * Other functions (that you don't need to write) modify the amount of posts a user can 
     * still post when they post, give experience for posts, might increase or decrease a free users limit
     * etc.
     */
    def runAtMidnight(user: User): User = {
      val refLevel = refineV[NonNegative](user.level.value + user.experience.value / 1000)
      val refExperience = refineV[NonNegative](user.experience.value % 1000)
      val refUser = user match {
        case u@FreeUser(_, _, _, _, _, postsLeft) =>
          val newPostsLeft = if (postsLeft.value < 3) refineMV[NonNegative](3) else postsLeft
          for {
            newLevel <- refLevel
            newExperience <- refExperience
          } yield u.copy(level = newLevel, experience = newExperience, postsLeft = newPostsLeft)
        case u@PaidUser(_, _, _, _, _, daysRemaining) =>
          val refDaysRemaining = refineV[NonNegative](Math.max(daysRemaining.value - 1, 0))
          for {
            newLevel <- refLevel
            newExperience <- refExperience
            newDaysRemaining <- refDaysRemaining
          } yield u.copy(level = newLevel, experience = newExperience, daysRemaining = newDaysRemaining)
      }
      refUser.right.get  // the only way to have refinement errors is to get int overflow, highly unlikely (
    }
  }
}
