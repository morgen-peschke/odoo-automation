package peschke.odoo

import cats.Show
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.syntax.all._
import com.monovore.decline.Argument
import io.circe.Decoder
import io.circe.Encoder
import peschke.odoo.AppConfig.AppCommand
import peschke.odoo.AppConfig.AuthConfig
import peschke.odoo.AppConfig.DryRun
import peschke.odoo.AppConfig.LoginCache
import peschke.odoo.AppConfig.Verbose
import peschke.odoo.models.Template.PickingNameTemplate
import peschke.odoo.models.Template.TimeOfDay
import peschke.odoo.models.Template.TimeOfDay.ScheduleAtOverrides
import peschke.odoo.models._
import peschke.odoo.models.authentication.ApiKey
import peschke.odoo.models.authentication.Database
import peschke.odoo.models.authentication.ServerUri
import peschke.odoo.models.authentication.Username
import peschke.odoo.utils.Circe._

import scala.concurrent.duration.Duration
import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

final case class AppConfig
  (auth: AuthConfig,
   command: AppCommand,
   dryRun: DryRun,
   verbose: Verbose,
   loginCache: LoginCache,
   globalNamePrefixOpt: Option[PickingNameTemplate],
   globalNameSuffixOpt: Option[PickingNameTemplate],
   minIntervalBetweenRequests: FiniteDuration
  )
object AppConfig {
  final case class AuthConfig(serverUrl: ServerUri, username: Username, database: Database, apiKey: ApiKey)

  sealed abstract class DryRun(val name: String)
  object DryRun {
    case object Enabled  extends DryRun("TRUE")
    case object Disabled extends DryRun("FALSE")
    case object ReadOnly extends DryRun("READ-ONLY")

    def fromString(raw: String): Either[String, DryRun] = raw.toUpperCase match {
      case "T" | Enabled.name   => Enabled.asRight
      case "F" | Disabled.name  => Disabled.asRight
      case "RO" | ReadOnly.name => ReadOnly.asRight
      case _                    => "Expected one of: 'true', 't', 'false', 'f', 'ro', or 'read-only'".asLeft
    }

    implicit val show: Show[DryRun] = Show.show(_.name)

    implicit val decoder: Decoder[DryRun] = Decoder[String].emap(fromString)
    implicit val encoder: Encoder[DryRun] = Encoder[String].contramap(_.name)

    implicit val argument: Argument[DryRun] = Argument.from("t|f|ro")(fromString(_).toValidatedNel)
  }

  object Verbose extends NewBoolean
  type Verbose = Verbose.Type

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

    final case class CreatePickings
      (template: JsonLoader.Source,
       knownIdsOpt: Option[JsonLoader.Source],
       times: Option[NonEmptySet[TimeOfDay]],
       dateOverridesOpt: Option[NonEmptySet[DateOverride]],
       scheduleAtOverrides: ScheduleAtOverrides,
       labelFilter: LabelFilter,
       tagFilter: TagFilter
      ) extends AppCommand
    object CreatePickings {
      implicit val decoder: Decoder[CreatePickings] = accumulatingDecoder { c =>
        (
          c.downField("entries").asAcc[JsonLoader.Source],
          c.downField("knownIds").asAcc[Option[JsonLoader.Source]],
          c.downField("times").asAcc[Option[NonEmptySet[TimeOfDay]]],
          c.downField("dateOverrides").asAcc[Option[NonEmptyList[DateOverride]]].map(_.map(_.toNes)),
          (
            c.downField("am-time").asAcc[Option[TimeOfDay.MorningTime]],
            c.downField("pm-time").asAcc[Option[TimeOfDay.NightTime]]
          ).tupled.map(ScheduleAtOverrides(_)),
          LabelFilter(TextFilter.truthy).valid,
          TagFilter.Exists(TextFilter.truthy).valid
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

  implicit val finiteDurationDecoder: Decoder[FiniteDuration] = Decoder[String].emap { raw =>
    Either
      .catchOnly[NumberFormatException](Duration(raw))
      .leftMap(_ => s"Invalid duration: $raw")
      .flatMap {
        case duration: FiniteDuration => duration.asRight
        case _                        => "Expected finite duration".asLeft
      }
  }

  implicit val decoder: Decoder[AppConfig] = accumulatingDecoder { c =>
    (
      c.downField("auth").asAcc[AuthConfig],
      c.downField("command").asAcc[AppCommand],
      c.downField("dry-run").asAcc[Option[DryRun]].map(_.getOrElse(DryRun.Disabled)),
      c.downField("verbose").asAcc[Option[Verbose]].map(_.getOrElse(Verbose.Disabled)),
      c.downField("login-cache").asAcc[LoginCache],
      c.downField("global-name-prefix").asAcc[Option[PickingNameTemplate]],
      c.downField("global-name-suffix").asAcc[Option[PickingNameTemplate]],
      c.downField("min-request-interval").asAcc[Option[FiniteDuration]].map(_.getOrElse(1.second))
    ).mapN(AppConfig.apply)
  }
}
