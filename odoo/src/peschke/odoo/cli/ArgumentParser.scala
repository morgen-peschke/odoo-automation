package peschke.odoo.cli

import cats.Monad
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.data.Validated
import cats.data.ValidatedNel
import cats.syntax.all._
import com.monovore.decline.Argument
import com.monovore.decline.Command
import com.monovore.decline.Help
import com.monovore.decline.Opts
import fs2.io.file.Path
import io.circe.Json
import peschke.odoo.AppConfig
import peschke.odoo.AppConfig.AppCommand
import peschke.odoo.AppConfig.AuthConfig
import peschke.odoo.AppConfig.DryRun
import peschke.odoo.AppConfig.LoginCache
import peschke.odoo.AppConfig.Verbose
import peschke.odoo.JsonLoader
import peschke.odoo.JsonLoader.Source
import peschke.odoo.JsonLoader.Source.StdIn
import peschke.odoo.algebras.TextFilterParser
import peschke.odoo.models.Action.Fields
import peschke.odoo.models.Action.Read
import peschke.odoo.models.Action.Search
import peschke.odoo.models.Action.Write
import peschke.odoo.models.RpcServiceCall.ObjectService.FieldName
import peschke.odoo.models.RpcServiceCall.ObjectService.Id
import peschke.odoo.models.RpcServiceCall.ObjectService.ModelName
import peschke.odoo.models.Template.PickingNameTemplate
import peschke.odoo.models.Template.TimeOfDay
import peschke.odoo.models.Template.TimeOfDay.ScheduleAtOverrides
import peschke.odoo.models._
import peschke.odoo.models.authentication.ApiKey
import peschke.odoo.models.authentication.Database
import peschke.odoo.models.authentication.ServerUri
import peschke.odoo.models.authentication.Username

import java.nio.file.InvalidPathException
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

trait ArgumentParser[F[_]] {
  def parse(args: Seq[String]): F[Either[Help, AppConfig]]
}
object ArgumentParser      {

  private implicit val regexArg: Argument[Regex] =
    Argument.from("regular-expression") { raw =>
      Validated.catchNonFatal(raw.r).leftMap(_.getMessage).toValidatedNel
    }

  private implicit val settingsArg: Argument[Source] =
    Argument.from("json:config-json|file:path|-")(_.split(':').toList match {
      case "-" :: Nil     => StdIn.validNel
      case "json" :: rest => JsonLoader.Source.RawJson(rest.mkString(":")).validNel
      case "file" :: rest =>
        val path = rest.mkString(":")
        Validated
          .catchOnly[InvalidPathException](Path(path))
          .bimap(
            e => s"Invalid path <$path>: ${e.getMessage}".pure[NonEmptyList],
            JsonLoader.Source.JsonFile
          )
      case _              => """Expected Json prefixed with "json:" or a path prefixed with "file:" or "-" for stdin""".invalidNel
    })

  private implicit val textFilterArg: Argument[TextFilter] = {
    val parser = TextFilterParser.default[Either[String, *], String](_.show)
    Argument.from("filter-exp") { raw =>
      parser
        .parse(raw)
        .leftMap { error =>
          s"""Expected filter expression
             |Not a strictly valid grammar, but here's the gist:
             |
             |QUOTED      := '"' [^"]* '"'
             |UNQUOTED    := [A-Za-z0-9_.-]+
             |WS          := ' ' | '\\t'
             |TEXT        := QUOTED | UNQUOTED
             |
             |FALSE       := 'false'
             |TRUE        := 'true'
             |EXACT       := 'is:' TEXT
             |PREFIX      := 'starts:' TEXT
             |SUFFIX      := 'ends:' TEXT
             |CONTAINS    := 'contains:' TEXT
             |
             |NOT_ALPHA   := 'not:' FILTER
             |NOT_SYMBOL  := '!' FILTER
             |NOT         := NOT_ALPHA | NOT_SYMBOL
             |
             |AND_ALPHA   := FILTER 'and' FILTER
             |AND_SYMBOL  := FILTER '&' FILTER
             |AND         := AND_ALPHA | AND_SYMBOL
             |
             |OR_ALPHA    := FILTER 'or' FILTER
             |OR_SYMBOL   := FILTER '|' FILTER
             |OR          := OR_ALPHA | OR_SYMBOL
             |
             |NO_PARENS   := FALSE | TRUE | EXACT | PREFIX | SUFFIX | CONTAINS | NOT | AND | OR
             |WITH_PARENS := '(' NO_PARENS ')'
             |FILTER      := WITH_PARENS | NO_PARENS
             |$error""".stripMargin
        }
        .toValidatedNel
    }
  }

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
        Opts
          .options[Search.Condition](
            short = "c",
            long = "condition",
            help = "Search condition, as a json-encoded array of strings"
          ).orEmpty,
        Opts
          .option[Search.Limit](
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
      case _               => "Expected <field>=<json>".invalidNel
    }
  }

  private def csvOpt[A]
    (entryFormat: String, metaVar: String, parser: String => ValidatedNel[String, A])
    : Argument[NonEmptyList[A]] =
    Argument.from(s"${metaVar}0,${metaVar}1,...${metaVar}N $metaVar:$entryFormat") { raw =>
      NonEmptyList
        .fromList(raw.split(',').toList)
        .toValidNel("Expected non-empty list of comma-separated entries")
        .andThen(_.traverse(parser))
    }

  implicit val timeOfDaySetArgument: Argument[NonEmptySet[TimeOfDay]] = {
    val entryNames = TimeOfDay.values.map(t => s"${t.shortName}|${t.fullName}").mkString("|")
    csvOpt(entryNames, "time", TimeOfDay.parse(_).toValidatedNel)
      .map(_.toNes)
  }

  implicit val dayOfWeekArgument: Argument[NonEmptyList[DayOfWeek]] = {
    val entryNames = DayOfWeek.values.map(t => t.fullName).mkString("unique prefix of \"", "\", \"", "\"")
    csvOpt(entryNames, "dayOfWeek", Argument[DayOfWeek].read(_))
  }

  implicit val intList: Argument[NonEmptyList[Int]] =
    csvOpt("[0-9]", "i", s => s.toIntOption.filter(_ >= 0).toValidNel(s"$s is not a positive integer"))

  implicit val localDateArgument: Argument[NonEmptyList[LocalDate]] = {
    def parse(s: String) =
      Either.catchNonFatal(LocalDate.parse(s, DateTimeFormatter.ISO_LOCAL_DATE)).leftMap(_.getMessage)

    csvOpt("yyyy-MM-dd", "date", parse(_).toValidatedNel)
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

  private val dateOverridesOpt: Opts[Option[NonEmptySet[DateOverride]]] = {
    val today: Opts[NonEmptyList[DateOverride]] =
      Opts
        .flag("today", help = "Create pickets for today (default)")
        .as(DateOverride.Today.pure[NonEmptyList])

    val yesterday: Opts[NonEmptyList[DateOverride]] =
      Opts
        .flag("yesterday", help = "Create pickings for yesterday")
        .as(DateOverride.DaysAgo(1).pure[NonEmptyList])

    val daysAgo: Opts[NonEmptyList[DateOverride]] =
      Opts
        .options[NonEmptyList[Int]]("days-ago", help = "Create pickings for a date N days ago")
        .map(_.flatten.map(DateOverride.DaysAgo))

    val last: Opts[NonEmptyList[DateOverride]] =
      Opts
        .options[NonEmptyList[DayOfWeek]]("last", help = "Create pickings for the immediately previous day of the week")
        .map(_.flatten.map(DateOverride.Last))

    val exactly: Opts[NonEmptyList[DateOverride]] =
      Opts
        .options[NonEmptyList[LocalDate]]("use-date", help = "Create pickings for this date")
        .map(_.flatten.map(DateOverride.Exactly))

    (
      today.orNone,
      yesterday.orNone,
      daysAgo.orNone,
      last.orNone,
      exactly.orNone
    ).mapN(_ :: _ :: _ :: _ :: _ :: Nil).map(_.flatten).map(NonEmptyList.fromList).map(_.map(_.flatten.toNes))
  }

  private val labelFilterOpts: Opts[LabelFilter] = {
    val parseAbleFilter =
      Opts
        .option[TextFilter](
          long = "label",
          help = "Only create pickings that match this filter"
        )

    val regexFilter =
      Opts
        .option[Regex](
          long = "label:regex",
          help = "Only create pickings that match this filter (for when --label is insufficient)"
        )
        .map(TextFilter.matches)

    parseAbleFilter
      .orElse(regexFilter)
      .map(LabelFilter)
      .withDefault(LabelFilter(TextFilter.truthy))
  }

  private val tagFilterOpts: Opts[TagFilter] = {
    val exists =
      Opts
        .option[TextFilter](
          long = "tag:exists",
          help = "Only create pickings when at least one tag that matches this filter"
        )
        .map(TagFilter.Exists)

    val existsRegex =
      Opts
        .option[Regex](
          long = "tag:exists:regex",
          help = "Only create pickings when at least one tag that matches this regex"
        )
        .map(TextFilter.matches)
        .map(TagFilter.Exists)

    val forAll =
      Opts
        .option[TextFilter](
          long = "tag:forAll",
          help = "Only create pickings when all tags match this filter"
        )
        .map(TagFilter.ForAll)

    val forAllRegex =
      Opts
        .option[Regex](
          long = "tag:forAll:regex",
          help = "Only create pickings when all tags match this regex"
        )
        .map(TextFilter.matches)
        .map(TagFilter.ForAll)

    val skip = TagFilter.Exists(TextFilter.truthy)

    exists.orElse(forAll).orElse(existsRegex).orElse(forAllRegex).withDefault(skip)
  }

  private val createPickingOpts: Opts[AppCommand] =
    Opts.subcommand("pickings", help = "Create pickings and moves from a template") {
      (
        Opts.option[Source]("template", help = "Template with pickings and moves"),
        Opts.option[Source]("known-ids", help = "File with mappings from strings to known ids").orNone,
        Opts
          .option[NonEmptySet[TimeOfDay]](
            "time-of-day",
            help = "Only generate pickings for a specific time of day"
          ).orNone,
        dateOverridesOpt,
        (
          Opts.option[TimeOfDay.MorningTime]("am-time", help = "Time of day to schedule AM pickings").orNone,
          Opts.option[TimeOfDay.NightTime]("pm-time", help = "Time of day to schedule PM pickings").orNone
        ).tupled.map(ScheduleAtOverrides(_)),
        labelFilterOpts,
        tagFilterOpts
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
      Opts
        .env[Verbose](
          "ODOO_VERBOSE",
          help = "Enable verbose logging for calls. This includes the full curl, so only use this for local testing"
        )
        .orElse(Verbose.Disabled.pure[Opts]),
      Opts.env[LoginCache]("ODOO_LOGIN_CACHE", help = "File used to cache login session"),
      Opts
        .env[PickingNameTemplate]("ODOO_PICKING_NAME_PREFIX", help = "Add a prefix to generated picking names")
        .orNone,
      Opts
        .env[PickingNameTemplate]("ODOO_PICKING_NAME_SUFFIX", help = "Add a suffix to generated picking names")
        .orNone,
      Opts
        .env[FiniteDuration]("ODOO_MIN_INTERVAL_BETWEEN_REQUESTS", help = "Minimum delay between API requests")
        .orElse(1.second.pure[Opts])
    ).mapN(AppConfig.apply)

  private val appConfigOpt: Opts[Either[JsonLoader.Source, AppConfig]] =
    appConfigJsonOpt.sum(appConfigViaParametersOpt)

  private val command: Command[Either[JsonLoader.Source, AppConfig]] =
    Command(name = "odoo", header = "Automation for odoo")(appConfigOpt)

  def default[F[_]: Monad](configLoader: JsonLoader[F]): ArgumentParser[F] =
    command
      .parse(_, sys.env)
      .fold(
        _.asLeft[AppConfig].pure[F],
        _.fold(
          configLoader.load[AppConfig](_).map(_.asRight[Help]),
          _.asRight[Help].pure[F]
        )
      )
}
