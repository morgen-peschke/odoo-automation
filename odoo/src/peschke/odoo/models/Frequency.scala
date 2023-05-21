package peschke.odoo.models

import cats.Order
import cats.data.NonEmptySet
import cats.syntax.all._
import io.circe.Decoder
import peschke.odoo.utils.Circe._

import java.time.DayOfWeek
import java.time.DayOfWeek.{FRIDAY, MONDAY, SATURDAY, SUNDAY, THURSDAY, TUESDAY, WEDNESDAY}

sealed trait Frequency
object Frequency {
  case object Daily extends Frequency
  final case class Weekly(days: NonEmptySet[DayOfWeek]) extends Frequency

  implicit val dayOfWeekOrder: Order[DayOfWeek] = Order.by(_.getValue)

  implicit val dayOfWeekDecoder: Decoder[DayOfWeek] = Decoder[String].map(_.toUpperCase).emap {
    case "MON" => MONDAY.asRight
    case "TUE" => TUESDAY.asRight
    case "WED" => WEDNESDAY.asRight
    case "THU" => THURSDAY.asRight
    case "FRI" => FRIDAY.asRight
    case "SAT" => SATURDAY.asRight
    case "SUN" => SUNDAY.asRight
    case other => Either.catchOnly[IllegalArgumentException](DayOfWeek.valueOf(other)).leftMap(_.getMessage)
  }

  implicit val decoder: Decoder[Frequency] = anyOf[Frequency](
    fixed("daily").as(Daily),
    fixed("weekdays").as(Weekly(NonEmptySet.of(MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY))),
    fixed("weekends").as(Weekly(NonEmptySet.of(SATURDAY, SUNDAY))),
    Decoder[NonEmptySet[DayOfWeek]].map(Weekly)
  )
}
