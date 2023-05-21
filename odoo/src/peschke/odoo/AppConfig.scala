package peschke.odoo

import cats.Eq
import cats.data.NonEmptySet
import cats.syntax.all._
import com.monovore.decline.Argument
import io.circe.Decoder
import peschke.odoo.AppConfig.{AppCommand, AuthConfig, DryRun, LoginCache}
import peschke.odoo.models.Action
import peschke.odoo.models.Template.TimeOfDay
import peschke.odoo.models.authentication.{ApiKey, Database, ServerUri, Username}
import peschke.odoo.utils.Circe._

import java.time.LocalDate

final case class AppConfig(auth: AuthConfig, command: AppCommand, dryRun: DryRun, loginCache: LoginCache)
object AppConfig {
  final case class AuthConfig(serverUrl: ServerUri,
                              username: Username,
                              database: Database,
                              apiKey: ApiKey)

  sealed abstract class DryRun
  object DryRun {
    case object Disabled extends DryRun
    case object Enabled extends DryRun
    case object Verbose extends DryRun

    def fromString(raw: String): Either[String, DryRun] = raw.toUpperCase match {
      case "DISABLED" => Disabled.asRight
      case "ENABLED" => Enabled.asRight
      case "VERBOSE" => Verbose.asRight
      case _ => s"Expected one of: Disabled,Enabled,Verbose (was: $raw)".asLeft
    }

    implicit val eq: Eq[DryRun] = Eq.fromUniversalEquals
    implicit val decoder: Decoder[DryRun] = Decoder[String].emap(fromString)
    implicit val argument: Argument[DryRun] =
      Argument.from("enabled|disabled|verbose")(fromString(_).toValidatedNel)
  }

  object LoginCache extends supertagged.NewType[fs2.io.file.Path] {
    def fromString(raw: String): Either[String, Type] =
      Either.catchNonFatal(fs2.io.file.Path(raw)).bimap(_.getMessage, apply(_))

    implicit val argument: Argument[Type] = Argument.from("path")(fromString(_).toValidatedNel)
    implicit val decoder: Decoder[Type] = Decoder[String].emap(fromString)
  }
  type LoginCache = LoginCache.Type

  sealed trait AppCommand
  object AppCommand {
    final case class DoAction(action: Action) extends AppCommand

    final case class CreatePickings(template: JsonLoader.Source,
                                    knownIdsOpt: Option[JsonLoader.Source],
                                    times: Option[NonEmptySet[TimeOfDay]],
                                    dateOverrideOpt: Option[LocalDate]
                                   ) extends AppCommand
    object CreatePickings {
      implicit val decoder: Decoder[CreatePickings] = accumulatingDecoder { c =>
        (
          c.downField("entries").asAcc[JsonLoader.Source],
          c.downField("knownIds").asAcc[Option[JsonLoader.Source]],
          c.downField("times").asAcc[Option[NonEmptySet[TimeOfDay]]],
          c.downField("dateOverride").asAcc[Option[LocalDate]]
        ).mapN(CreatePickings.apply)
      }
    }

    case object ReloadKnownIds extends AppCommand

    implicit val decoder: Decoder[AppCommand] = anyOf[AppCommand](
      Decoder[Action].map(DoAction),
      Decoder[CreatePickings].at("pickings").widen,
      fixed("reload-known-ids").as(ReloadKnownIds)
    )
  }

  implicit val authConfigDecoder: Decoder[AuthConfig] = accumulatingDecoder { c =>
    (
      c.downField("server").asAcc[ServerUri],
      c.downField("username").asAcc[Username],
      c.downField("database").asAcc[Database],
      c.downField("apiKey").asAcc[ApiKey]
    ).mapN(AuthConfig.apply)
  }

  implicit val decoder: Decoder[AppConfig] = accumulatingDecoder { c =>
    (
      c.downField("auth").asAcc[AuthConfig],
      c.downField("command").asAcc[AppCommand],
      c.downField("dry-run").asAcc[Option[DryRun]].map(_.getOrElse(DryRun.Disabled)),
      c.downField("login-cache").asAcc[LoginCache]
    ).mapN(AppConfig.apply)
  }
}
