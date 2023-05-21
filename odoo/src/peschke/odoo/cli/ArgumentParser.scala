package peschke.odoo.cli

import peschke.odoo.{AppConfig, JsonLoader}
import peschke.odoo.JsonLoader.Source
import peschke.odoo.JsonLoader.Source.StdIn
import cats.{Monad, Order}
import cats.data.{NonEmptyList, NonEmptySet, Validated}
import cats.syntax.all._
import com.monovore.decline.{Argument, Command, Help, Opts}
import fs2.io.file.Path
import io.circe.Json
import peschke.odoo.AppConfig.{AppCommand, AuthConfig, DryRun, LoginCache}
import peschke.odoo.models.Action
import peschke.odoo.models.Action.{Fields, Read, Search, Write}
import peschke.odoo.models.RpcServiceCall.ObjectService.{FieldName, Id, ModelName}
import peschke.odoo.models.Template.TimeOfDay
import peschke.odoo.models.authentication.{ApiKey, Database, ServerUri, Username}

import java.nio.file.InvalidPathException
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.collection.immutable.SortedSet

trait ArgumentParser[F[_]] {
  def parse(args: Seq[String]): F[Either[Help,AppConfig]]
}
object ArgumentParser {

  private implicit val settingsArg: Argument[Source] =
    Argument.from("json:config-json|file:path|-")(_.split(':').toList match {
      case "-" :: Nil => StdIn.validNel
      case "json" :: rest => JsonLoader.Source.RawJson(rest.mkString(":")).validNel
      case "file" :: rest =>
        val path = rest.mkString(":")
        Validated.catchOnly[InvalidPathException](Path(path))
          .bimap(
            e => s"Invalid path <$path>: ${e.getMessage}".pure[NonEmptyList],
            JsonLoader.Source.JsonFile
          )
      case _ => """Expected Json prefixed with "json:" or a path prefixed with "file:" or "-" for stdin""".invalidNel
    })

  private val appConfigJsonOpt: Opts[JsonLoader.Source] =
    Opts.option[JsonLoader.Source](
      long = "config",
      help = """Configuration, as JSON, as a literal string or a path to a file or "-" to read from stdin"""
    )

  private val authFromEnv: Opts[AuthConfig] =
    (
      Opts.env[ServerUri]("ODOO_SERVER_URI", help = "Uri of server, often <...>.odoo.com"),
      Opts.env[Username]("ODOO_USERNAME", help = "Username on odoo server"),
      Opts.env[Database]("ODOO_DATABASE", help = "Database on odoo server"),
      Opts.env[ApiKey]("ODOO_API_KEY", help = "API key on odoo server")
    ).mapN(AuthConfig.apply)

  private val modelNameOpts: Opts[ModelName] =
    Opts.option[ModelName]("model", help = "Model name (ex: stock.picking)")

  private val fieldNamesOpt: Opts[List[FieldName]] =
    Opts.option[List[FieldName]]("fields", help = "Fields to return, pass an empty string for all fields")

  private val serverInfoOpt: Opts[Action] =
    Opts.subcommand("server-info", help = "Retrieve server metadata")(Action.ServerInfo.pure[Opts])

  private val loginOpt: Opts[Action] =
    Opts.subcommand("login", help = "Log in to server and cache result")(Action.Login.pure[Opts])

  private val fieldsOpts: Opts[Action] =
    Opts.subcommand("fields", help = "Retrieve model fields") {
      val attributes =
        Opts.option[Either[List[Fields.Attribute], Fields.DefaultAttributes]](
          short = "a",
          long = "attribute",
          help = "Attributes to include, if omitted all will be returned, which can be overwhelming"
        )

      (modelNameOpts, attributes).mapN(Action.Fields.apply)
    }

  private val searchOpts: Opts[Action] =
    Opts.subcommand("search", help = "Search and read records") {
      (
        modelNameOpts,
        fieldNamesOpt,
        Opts.options[Search.Condition](
          short = "c",
          long = "condition",
          help = "Search condition, as a json-encoded array of strings"
        ).orEmpty,
        Opts.option[Search.Limit](
          long = "limit",
          help = "Limit the number of values returned"
        ).orNone
      ).mapN(Search.apply)
    }

  implicit val parameterArgument: Argument[(FieldName, Json)] = Argument.from("<field>=<json>") { raw =>
    raw.split('=').toList match {
      case key :: v0 :: vN =>
        (
          FieldName.fromString(key).toValidatedNel,
          io.circe.parser.parse((v0 :: vN).mkString("=")).leftMap(_.message).toValidatedNel
        ).tupled
      case _ => "Expected <field>=<json>".invalidNel
    }
  }

  implicit val timeOfDaySetArgument: Argument[Option[NonEmptySet[TimeOfDay]]] =
    Argument.from("time0,time1,...timeN") { raw =>
      raw.split(',').toList.traverse(TimeOfDay.fromString)
        .map(SortedSet.from(_)(Order[TimeOfDay].toOrdering))
        .map(NonEmptySet.fromSet)
        .toValidatedNel
    }

  implicit val localDateArgument: Argument[LocalDate] =
    Argument.from("yyyy-MM-dd") { raw =>
      Either.catchNonFatal(LocalDate.parse(raw, DateTimeFormatter.ISO_LOCAL_DATE))
        .leftMap(_.getMessage)
        .toValidatedNel
    }

  private val readOpts: Opts[Action] =
    Opts.subcommand("read", help = "Read a record") {
      (
        modelNameOpts,
        Opts.options[Id](
          short = "i",
          long = "id",
          help = "Id of record to read"
        ),
        fieldNamesOpt
      ).mapN(Read.apply)
    }


  private val writeOpts: Opts[Action] =
    Opts.subcommand("write", help = "Update a record") {
      (
        modelNameOpts,
        Opts.options[Id](
          short = "i",
          long = "id",
          help = "Id of records to update"
        ),
        Opts.options[(FieldName, Json)](
          short = "u",
          long = "update",
          help = "Field name and new value to be updated"
        )
      ).mapN(Write.apply)
    }


  private val createOpts: Opts[Action] =
    Opts.subcommand("create", help = "Create a record") {
      (
        modelNameOpts,
        Opts.options[(FieldName, Json)](
          short = "p",
          long = "parameter",
          help = "Parameter used to initialize the record"
        )
      ).mapN(Action.Create.apply)
    }

  private val createPickingOpts: Opts[AppCommand] =
    Opts.subcommand("pickings", help = "Create pickings and moves from a template") {
      (
        Opts.option[Source]("template", help = "Template with pickings and moves"),
        Opts.option[Source]("known-ids", help = "File with mappings from strings to known ids").orNone,
        Opts.option[Option[NonEmptySet[TimeOfDay]]](
          "time-of-day",
          help = "Only generate pickings for a specific time of day"
        ).orNone.map(_.flatten),
        Opts.option[LocalDate]("override-date", help = "Pretend today is this date").orNone
      ).mapN(AppCommand.CreatePickings.apply)
    }

  private val generateKnownIds: Opts[AppCommand] =
    Opts.subcommand("generate-known-ids", help = "Generate a the JSON for a known ids file") {
      AppCommand.ReloadKnownIds.pure[Opts]
    }

  private val appCommandOpt: Opts[AppCommand] =
    serverInfoOpt
      .orElse(loginOpt)
      .orElse(fieldsOpts)
      .orElse(searchOpts)
      .orElse(readOpts)
      .orElse(writeOpts)
      .orElse(createOpts)
      .map(AppCommand.DoAction)
      .orElse(createPickingOpts)
      .orElse(generateKnownIds)

  private val appConfigViaParametersOpt: Opts[AppConfig] =
    (
      authFromEnv,
      appCommandOpt,
      Opts
        .env[DryRun]("ODOO_DRY_RUN", help = "Don't make live calls to servers")
        .orElse(DryRun.Disabled.pure[Opts]),
      Opts.env[LoginCache]("ODOO_LOGIN_CACHE", help = "File used to cache login session")
    ).mapN(AppConfig.apply)

  private val appConfigOpt: Opts[Either[JsonLoader.Source, AppConfig]] =
    appConfigJsonOpt.sum(appConfigViaParametersOpt)

  private val command: Command[Either[JsonLoader.Source, AppConfig]] =
    Command(name = "odoo", header = "Automation for odoo")(appConfigOpt)

  def default[F[_]: Monad](configLoader: JsonLoader[F]): ArgumentParser[F] =
    command.parse(_, sys.env)
      .fold(
        _.asLeft[AppConfig].pure[F],
        _.fold(
          configLoader.load[AppConfig](_).map(_.asRight[Help]),
          _.asRight[Help].pure[F]
        )
      )
}