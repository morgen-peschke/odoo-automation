package peschke.odoo.algebras

import cats.syntax.all._
import cats.{MonadThrow, Show}
import com.github.mustachejava.{DefaultMustacheFactory, Mustache}
import org.typelevel.log4cats.LoggerFactory
import peschke.odoo.algebras.PickingNameGenerator.{EntryIndex, PickingIndex}
import peschke.odoo.models.CheckedTemplate.PickingName
import peschke.odoo.models.Template.{PickingNameTemplate, PickingTemplate, TimeOfDay}
import peschke.odoo.models.{DayOfWeek, NewString, PosInt}

import java.io.{StringReader, StringWriter}
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._

trait PickingNameGenerator[F[_]] {
  def generate(picking: PickingTemplate, today: LocalDate, index: (EntryIndex, PickingIndex)): F[PickingName]
}
object PickingNameGenerator {
  def apply[F[_]](implicit PNG: PickingNameGenerator[F]): PNG.type = PNG

  object EntryIndex extends PosInt("entry index")
  type EntryIndex = EntryIndex.Type

  object PickingIndex extends PosInt("picking index")
  type PickingIndex = PickingIndex.Type

  object IndexStr extends NewString {
    override def fromString(raw: String): Either[String, Type] = apply(raw).asRight
  }
  type IndexStr = IndexStr.Type

  def default[F[_]: MonadThrow: LoggerFactory](globalNamePrefixOpt: Option[PickingNameTemplate],
                                               globalNameSuffixOpt: Option[PickingNameTemplate]): PickingNameGenerator[F] =
    new PickingNameGenerator[F] {
      private val logger = LoggerFactory[F].getLogger
      private implicit val localDateShow: Show[LocalDate] = {
        val formatter = DateTimeFormatter.ISO_LOCAL_DATE
        Show.show(_.format(formatter))
      }
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

      def runMustache(mustache: Mustache, today: LocalDate, timeOfDay: TimeOfDay, index: IndexStr): F[String] =
        MonadThrow[F].catchNonFatal {
          val dayOfWeek = DayOfWeek.ofDay(today)
          val out = new StringWriter()
          mustache.execute(out, Map(
            "today" -> today.show,
            "timeOfDay" -> timeOfDay.shortName,
            "timeOfDayShort" -> timeOfDay.shortName,
            "timeOfDayFull" -> timeOfDay.fullName,
            "index" -> index.show,
            "dayOfWeek" -> dayOfWeek.shortName,
            "dayOfWeekShort" -> dayOfWeek.shortName,
            "dayOfWeekFull" -> dayOfWeek.longName
          ).asJava)
          out.show
        }

      override def generate(picking: PickingTemplate,
                            today: LocalDate,
                            rawIndex: (EntryIndex, PickingIndex)): F[PickingName] = {
        val index = IndexStr(s"${rawIndex._1}.${rawIndex._2}")

        def fallBackToSimpleName: PickingName = PickingName(show"MOVE/$today/$index/${picking.timeOfDay.shortName}")

        buildMustache(picking, index).redeemWith(
          ex => {
            val name = fallBackToSimpleName
            logger.warn(ex)(show"Unable to compile picking name template for $name").as(name)
          },
          runMustache(_, today, picking.timeOfDay, index).redeemWith(
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
