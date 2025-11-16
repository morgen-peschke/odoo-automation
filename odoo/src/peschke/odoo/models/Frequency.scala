package peschke.odoo.models

import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.syntax.all._
import io.circe.Decoder
import peschke.odoo.utils.Circe._

import java.time.LocalDate

sealed trait Frequency {
  def expanded: NonEmptyList[DayOfWeek]
  def includes(day: LocalDate): Boolean
}
object Frequency       {
  case object Daily                                     extends Frequency {
    override def expanded: NonEmptyList[DayOfWeek] = DayOfWeek.valuesNel

    override def includes(day: LocalDate): Boolean = true
  }
  final case class Weekly(days: NonEmptySet[DayOfWeek]) extends Frequency {
    override def expanded: NonEmptyList[DayOfWeek] = days.toNonEmptyList.sortBy(_.index)

    override def includes(day: LocalDate): Boolean = days.contains(DayOfWeek.ofDay(day))
  }

  implicit val decoder: Decoder[Frequency] =
    anyOf[Frequency](
      exactly("daily").as(Daily),
      exactly("weekdays").as(Weekly(DayOfWeek.WeekDays)),
      exactly("weekends").as(Weekly(DayOfWeek.WeekEnd)),
      Decoder[NonEmptySet[DayOfWeek]].map(Weekly),
      Decoder[DayOfWeek].map(NonEmptySet.one(_)).map(Weekly)
    )
}
