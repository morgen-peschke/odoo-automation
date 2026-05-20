package peschke.odoo.algebras

import cats.MonadThrow
import cats.Show
import cats.syntax.all._
import com.github.mustachejava.DefaultMustacheFactory
import com.github.mustachejava.Mustache
import org.typelevel.log4cats.LoggerFactory
import peschke.odoo.models.DayOfWeek
import peschke.odoo.models.Template.TimeOfDay

import java.io.StringReader
import java.io.StringWriter
import scala.jdk.CollectionConverters._

trait LocationNameExpander[F[_]] {
  def generate(template: String, dayOfWeek: DayOfWeek, timeOfDay: TimeOfDay): F[String]
}
object LocationNameExpander      {
  def apply[F[_]](implicit LNE: LocationNameExpander[F]): LNE.type = LNE

  def default[F[_]: MonadThrow: LoggerFactory]: LocationNameExpander[F] =
    new LocationNameExpander[F] {
      private val logger = LoggerFactory[F].getLoggerFromClass(classOf[LocationNameExpander[F]])
      private val mf = new DefaultMustacheFactory()
      private implicit val stringWriterShow: Show[StringWriter] = Show.fromToString

      def buildMustache(template: String): F[Mustache] =
        MonadThrow[F].catchNonFatal {
          mf.compile(new StringReader(template), "Location name")
        }

      def runMustache(mustache: Mustache, dayOfWeek: DayOfWeek, timeOfDay: TimeOfDay): F[String] =
        MonadThrow[F].catchNonFatal {
          val out = new StringWriter()
          mustache.execute(
            out,
            Map(
              "timeOfDay" -> timeOfDay.regularName,
              "timeOfDayShort" -> timeOfDay.shortName,
              "timeOfDayFull" -> timeOfDay.fullName,
              "dayOfWeek" -> dayOfWeek.shortName,
              "dayOfWeekShort" -> dayOfWeek.shortName,
              "dayOfWeekFull" -> dayOfWeek.fullName,
              "dayOfWeekIndex" -> dayOfWeek.index.show
            ).asJava
          )
          out.show
        }

      override def generate(template: String, dayOfWeek: DayOfWeek, timeOfDay: TimeOfDay): F[String] =
        if (!template.contains("{{")) template.pure[F]
        else
          buildMustache(template).redeemWith(
            logger.warn(_)(s"Unable to compile location as template: $template").as(template),
            runMustache(_, dayOfWeek, timeOfDay).redeemWith(
              logger.warn(_)(s"Unable to execute location as template: $template").as(template),
              _.pure[F]
            )
          )
    }
}
