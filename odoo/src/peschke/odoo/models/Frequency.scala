package peschke.odoo.models

import cats.data.{NonEmptyList, NonEmptySet}
import cats.syntax.all._
import io.circe.Decoder
import peschke.odoo.utils.Circe._

sealed trait Frequency {
  def expanded: NonEmptyList[DayOfWeek]
  def narrow(dayOfWeek: DayOfWeek): Option[Frequency]
  def isMocked: Boolean = false
}
object Frequency       {
  case object Daily                                            extends Frequency {
    override val expanded: NonEmptyList[DayOfWeek] = DayOfWeek.valuesNel

    override def narrow(dayOfWeek: DayOfWeek): Option[Frequency] =
      Weekly(NonEmptySet.one(dayOfWeek)).some
  }
  final case class Weekly(days: NonEmptySet[DayOfWeek])        extends Frequency {
    override def expanded: NonEmptyList[DayOfWeek] = days.toNonEmptyList.sortBy(_.index)

    override def narrow(dayOfWeek: DayOfWeek): Option[Frequency] =
      Option.when(days.contains(dayOfWeek))(Weekly(NonEmptySet.one(dayOfWeek)))
  }
  final case class Mocked(daysOfWeek: NonEmptyList[DayOfWeek]) extends Frequency {
    override def isMocked: Boolean = true
    override def expanded: NonEmptyList[DayOfWeek] = daysOfWeek
    override def narrow(dayOfWeek: DayOfWeek): Option[Frequency] =
      Option.when(daysOfWeek.exists(_ === dayOfWeek))(Mocked(NonEmptyList.one(dayOfWeek)))
  }

  implicit val decoder: Decoder[Frequency] = {
    val direct = anyOf[Frequency](
      fixed("daily").as(Daily),
      fixed("weekdays").as(Weekly(DayOfWeek.WeekDays)),
      fixed("weekends").as(Weekly(DayOfWeek.WeekEnd)),
      Decoder[NonEmptySet[DayOfWeek]].map(Weekly),
      Decoder[DayOfWeek].map(NonEmptySet.one(_)).map(Weekly)
    )
    anyOf[Frequency](direct, direct.at("mocked").map(_.expanded).map(Mocked))
  }
}
