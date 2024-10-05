package peschke.odoo.models

import cats.data.NonEmptySet
import cats.syntax.all._
import io.circe.Decoder
import peschke.odoo.utils.Circe._

sealed trait Frequency
object Frequency {
  case object Daily                                     extends Frequency
  final case class Weekly(days: NonEmptySet[DayOfWeek]) extends Frequency

  implicit val decoder: Decoder[Frequency] = anyOf[Frequency](
    fixed("daily").as(Daily),
    fixed("weekdays").as(Weekly(DayOfWeek.WeekDays)),
    fixed("weekends").as(Weekly(DayOfWeek.WeekEnd)),
    Decoder[NonEmptySet[DayOfWeek]].map(Weekly),
    Decoder[DayOfWeek].map(NonEmptySet.one(_)).map(Weekly)
  )
}
