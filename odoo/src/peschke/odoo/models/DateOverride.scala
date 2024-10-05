package peschke.odoo.models

import cats.Order
import cats.syntax.all._
import io.circe.Decoder
import peschke.odoo.utils.Circe._

import java.time.LocalDate

sealed trait DateOverride
object DateOverride {
  case object Today                           extends DateOverride
  final case class Exactly(date: LocalDate)   extends DateOverride
  final case class DaysAgo(delta: Int)        extends DateOverride
  final case class Last(dayOfWeek: DayOfWeek) extends DateOverride

  implicit val decoder: Decoder[DateOverride] = anyOf[DateOverride](
    fixed("today").as(Today),
    fixed("yesterday").as(DaysAgo(1)),
    Decoder[Int].at("days-ago").map(DaysAgo),
    Decoder[DayOfWeek].at("last").map(Last),
    Decoder[LocalDate].at("on").map(Exactly)
  )

  implicit val order: Order[DateOverride] = Order.by {
    case Today           => (0, 0, DayOfWeek.Monday)
    case DaysAgo(n)      => (2, n, DayOfWeek.Monday)
    case Last(dayOfWeek) => (3, 0, dayOfWeek)
    case Exactly(date)   => (4, date.getDayOfYear, DayOfWeek.Monday)
  }
}
