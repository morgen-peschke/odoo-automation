package peschke.odoo.models

import cats.syntax.all._
import com.monovore.decline.Argument
import io.circe.{Decoder, Json}
import io.circe.syntax._
import org.http4s.Uri

object authentication {
  object Database extends NonEmptyString("Database")
  type Database = Database.Type

  object Username extends NonEmptyString("Username")
  type Username = Username.Type

  object ServerUri extends supertagged.NewType[Uri] {
    def fromString(raw: String): Either[String, Type] = Uri.fromString(raw).bimap(_.message, apply(_))

    implicit val decoder: Decoder[Type] = Decoder[String].emap(fromString)

    implicit val argument: Argument[Type] = Argument.from("uri")(fromString(_).toValidatedNel)
  }
  type ServerUri = ServerUri.Type

  object ApiKey extends NonEmptyString("Api Key")
  type ApiKey = ApiKey.Type

  object LoginUid extends PosInt("Logged in Uid")
  type LoginUid = LoginUid.Type

  final case class LoggedIn(database: Database, loginUid: LoginUid, apiKey: ApiKey) {
    def addToArgs(args: List[Json]): List[Json] =
      Database.raw(database).asJson ::
        LoginUid.raw(loginUid).asJson ::
        ApiKey.raw(apiKey).asJson ::
        args
  }
}
