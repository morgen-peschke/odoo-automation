package peschke.odoo.models

import cats.Order
import cats.data.NonEmptySet
import cats.syntax.all._
import io.circe.Decoder

import java.time.{LocalDate, DayOfWeek => JDayOfWeek}

sealed abstract class DayOfWeek(val shortName: String, val longName: String) extends Product with Serializable {
  def unapply(str: String): Boolean = str.equalsIgnoreCase(shortName) || str.equalsIgnoreCase(longName)
}
object DayOfWeek {
  case object Monday extends DayOfWeek("Mon", "Monday")
  case object Tuesday extends DayOfWeek("Tue", "Tuesday")
  case object Wednesday extends DayOfWeek("Wed", "Wednesday")
  case object Thursday extends DayOfWeek("Thu", "Thursday")
  case object Friday extends DayOfWeek("Fri", "Friday")
  case object Saturday extends DayOfWeek("Sat", "Saturday")
  case object Sunday extends DayOfWeek("Sun", "Sunday")

  def ofDay(day: LocalDate): DayOfWeek = day.getDayOfWeek match {
    case JDayOfWeek.MONDAY => Monday
    case JDayOfWeek.TUESDAY => Tuesday
    case JDayOfWeek.WEDNESDAY => Wednesday
    case JDayOfWeek.THURSDAY => Thursday
    case JDayOfWeek.FRIDAY => Friday
    case JDayOfWeek.SATURDAY => Saturday
    case JDayOfWeek.SUNDAY => Sunday
  }

  implicit val dayOfWeekOrder: Order[DayOfWeek] = Order.by {
    case Monday => 1
    case Tuesday => 2
    case Wednesday => 3
    case Thursday => 4
    case Friday => 5
    case Saturday => 6
    case Sunday => 7
  }

  val WeekDays: NonEmptySet[DayOfWeek] = NonEmptySet.of(Monday, Tuesday, Wednesday, Thursday, Friday)
  val WeekEnd: NonEmptySet[DayOfWeek] = NonEmptySet.of(Saturday, Sunday)

  implicit val dayOfWeekDecoder: Decoder[DayOfWeek] = Decoder[String].map(_.toUpperCase).emap {
    case Monday() => Monday.asRight
    case Tuesday() => Tuesday.asRight
    case Wednesday() => Wednesday.asRight
    case Thursday() => Thursday.asRight
    case Friday() => Friday.asRight
    case Saturday() => Saturday.asRight
    case Sunday() => Sunday.asRight
    case _ => "Expected one of Mon, Tue, Wed, Thu, Fri, Sat, or Sun".asLeft
  }
}
