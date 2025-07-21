package peschke.odoo.algebras

import cats.MonadThrow
import cats.Show
import cats.syntax.all._
import com.github.mustachejava.DefaultMustacheFactory
import com.github.mustachejava.Mustache
import org.typelevel.log4cats.LoggerFactory
import peschke.odoo.algebras.PickingNameGenerator.EntryIndex
import peschke.odoo.algebras.PickingNameGenerator.PickingIndex
import peschke.odoo.models.CheckedTemplate.PickingName
import peschke.odoo.models.DayOfWeek
import peschke.odoo.models.NewString
import peschke.odoo.models.PosInt
import peschke.odoo.models.Template.PickingNameTemplate
import peschke.odoo.models.Template.PickingTemplate
import peschke.odoo.models.Template.TimeOfDay

import java.io.StringReader
import java.io.StringWriter
import java.time.LocalDate
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatterBuilder
import java.time.temporal.ChronoField
import scala.jdk.CollectionConverters._

trait PickingNameGenerator[F[_]] {
  def generate
    (picking: PickingTemplate, today: LocalDate, index: (EntryIndex, PickingIndex), timestamp: ZonedDateTime)
    : F[PickingName]
}
object PickingNameGenerator      {
  def apply[F[_]](implicit PNG: PickingNameGenerator[F]): PNG.type = PNG

  object EntryIndex extends PosInt("entry index")
  type EntryIndex = EntryIndex.Type

  object PickingIndex extends PosInt("picking index")
  type PickingIndex = PickingIndex.Type

  object IndexStr extends NewString {
    override def fromString(raw: String): Either[String, Type] = apply(raw).asRight
  }
  type IndexStr = IndexStr.Type

  def default[F[_]: MonadThrow: LoggerFactory]
    (globalNamePrefixOpt: Option[PickingNameTemplate], globalNameSuffixOpt: Option[PickingNameTemplate])
    : PickingNameGenerator[F] =
    new PickingNameGenerator[F] {
      private val logger = LoggerFactory[F].getLoggerFromClass(classOf[PickingNameGenerator[F]])
      private implicit val localDateShow: Show[LocalDate] = {
        val formatter = DateTimeFormatter.ISO_LOCAL_DATE
        Show.show(_.format(formatter))
      }
      private val timestampDateFormatter =
        new DateTimeFormatterBuilder()
          .appendValue(ChronoField.YEAR, 4)
          .appendLiteral('-')
          .appendValue(ChronoField.MONTH_OF_YEAR, 2)
          .appendLiteral('-')
          .appendValue(ChronoField.DAY_OF_MONTH, 2)
          .toFormatter
      private val timestampFullTime24Formatter =
        new DateTimeFormatterBuilder()
          .appendValue(ChronoField.HOUR_OF_DAY, 2)
          .appendLiteral(':')
          .appendValue(ChronoField.MINUTE_OF_HOUR, 2)
          .appendLiteral(':')
          .appendValue(ChronoField.SECOND_OF_MINUTE, 2)
          .toFormatter
      private val timestampFullTime12Formatter =
        new DateTimeFormatterBuilder()
          .appendValue(ChronoField.HOUR_OF_AMPM, 2)
          .appendLiteral(':')
          .appendValue(ChronoField.MINUTE_OF_HOUR, 2)
          .appendLiteral(':')
          .appendValue(ChronoField.SECOND_OF_MINUTE, 2)
          .appendText(ChronoField.AMPM_OF_DAY)
          .toFormatter
      private val timestampShortTime24Formatter =
        new DateTimeFormatterBuilder()
          .appendValue(ChronoField.HOUR_OF_DAY, 2)
          .appendLiteral(':')
          .appendValue(ChronoField.MINUTE_OF_HOUR, 2)
          .toFormatter
      private val timestampShortTime12Formatter =
        new DateTimeFormatterBuilder()
          .appendValue(ChronoField.HOUR_OF_AMPM, 2)
          .appendLiteral(':')
          .appendValue(ChronoField.MINUTE_OF_HOUR, 2)
          .appendText(ChronoField.AMPM_OF_DAY)
          .toFormatter

      private implicit val stringWriterShow: Show[StringWriter] = Show.fromToString
      private val mf = new DefaultMustacheFactory()

      private val namePrefix = globalNamePrefixOpt.fold("")(pnt => s"${PickingNameTemplate.raw(pnt)}/")
      private val nameSuffix = globalNameSuffixOpt.fold("")(pnt => s"/${PickingNameTemplate.raw(pnt)}")

      def buildMustache(picking: PickingTemplate, index: IndexStr): F[Mustache] =
        MonadThrow[F]
          .catchNonFatal {
            mf.compile(
              new StringReader(s"$namePrefix${PickingNameTemplate.raw(picking.pickingName)}$nameSuffix"),
              show"${picking.pickingName}/$index"
            )
          }

      def runMustache
        (mustache: Mustache, today: LocalDate, timeOfDay: TimeOfDay, index: IndexStr, timestamp: ZonedDateTime)
        : F[String] =
        MonadThrow[F].catchNonFatal {
          val dayOfWeek = DayOfWeek.ofDay(today)
          val out = new StringWriter()
          mustache.execute(
            out,
            Map(
              "today" -> today.show,
              "timeOfDay" -> timeOfDay.shortName,
              "timeOfDayShort" -> timeOfDay.shortName,
              "timeOfDayFull" -> timeOfDay.fullName,
              "index" -> index.show,
              "dayOfWeek" -> dayOfWeek.shortName,
              "dayOfWeekShort" -> dayOfWeek.shortName,
              "dayOfWeekFull" -> dayOfWeek.fullName,
              "dayOfWeekIndex" -> dayOfWeek.index.show,
              "ts-date" -> timestampDateFormatter.format(timestamp),
              "ts-HH:MM:SS" -> timestampFullTime24Formatter.format(timestamp),
              "ts-12HH:MM:SS" -> timestampFullTime12Formatter.format(timestamp),
              "ts-HH:MM" -> timestampShortTime24Formatter.format(timestamp),
              "ts-12HH:MM" -> timestampShortTime12Formatter.format(timestamp)
            ).asJava
          )
          out.show
        }

      override def generate
        (picking: PickingTemplate, today: LocalDate, rawIndex: (EntryIndex, PickingIndex), timestamp: ZonedDateTime)
        : F[PickingName] = {
        val index = IndexStr(s"${rawIndex._1}.${rawIndex._2}")

        def fallBackToSimpleName: PickingName = PickingName(show"MOVE/$today/$index/${picking.timeOfDay.shortName}")

        buildMustache(picking, index).redeemWith(
          ex => {
            val name = fallBackToSimpleName
            logger.warn(ex)(show"Unable to compile picking name template for $name").as(name)
          },
          runMustache(_, today, picking.timeOfDay, index, timestamp).redeemWith(
            ex => {
              val name = fallBackToSimpleName
              logger.warn(ex)(show"Unable to execute picking name template for $name").as(name)
            },
            PickingName(_).pure[F]
          )
        )
      }
    }
}
