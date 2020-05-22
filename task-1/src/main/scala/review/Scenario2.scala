package review

import cats.effect.Sync
import donotmodifyme.Scenario2._
import database._

/*
 * We need to store `donotmodifyme.Scenario2.Datum` in an efficient manner. We found a very efficient database
 * implementation (donotmodifyme.Scenario2.database.*` now we need to provide a well behaved wrapper for a
 * java the libary `database`.
 */
object Scenario2 {
  trait ConnectionIO[F[_]] {
    def put(datum: Datum): F[Unit]
    def getAll: F[Seq[Datum]]
  }

  object DatabaseUser {
    def execute[F[_]: Sync, A](credentials: DatabaseCredentials)(f: ConnectionIO[F] => F[A]): F[A] = {
      val s = implicitly[Sync[F]]
      s.bracket(s.delay(DatabaseConnection.open(credentials)))(con => f(interp(con)))(con => s.delay(con.close()))
    }

    private def interp[F[_]: Sync](connection: DatabaseConnection): ConnectionIO[F] = {
      val s = implicitly[Sync[F]]
      new ConnectionIO[F] {
        override def put(datum: Datum): F[Unit] =
          s.delay(connection.put(datum.key, datum.serializeContent))

        override def getAll: F[Seq[Datum]] =
          s.delay {
            val keys = connection.keys.toList
            val builder = Seq.newBuilder[Datum]
            keys.foreach { key =>
              val bytes = connection.fetch(key)
              Datum.deserialize(bytes) match {
                case Left(error) =>
                  // should we really ignore deserialization errors?
                case Right(datum) =>
                  builder += datum
              }
            }
            builder.result
          }
      }
    }
  }
}
