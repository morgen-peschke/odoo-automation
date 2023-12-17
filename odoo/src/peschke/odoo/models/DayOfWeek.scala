package peschke.odoo.models

import cats.Order
import cats.data.NonEmptySet
import cats.syntax.all._
import io.circe.Decoder

import java.time.{LocalDate, DayOfWeek => JDayOfWeek}

sealed abstract class DayOfWeek(val shortName: String)
  extends enumeratum.EnumEntry
    with Product
    with Serializable {

  def fullName: String = entryName

  private lazy val normalized: List[String] = List(fullName.toUpperCase, shortName.toUpperCase)

  def unapply(str: String): Boolean =
    normalized.contains(str) || ((str.length > 1) && normalized.exists(_.startsWith(str)))
}
object DayOfWeek extends enumeratum.Enum[DayOfWeek] {

  case object Monday extends DayOfWeek("Mon")
  case object Tuesday extends DayOfWeek("Tue")
  case object Wednesday extends DayOfWeek("Wed")
  case object Thursday extends DayOfWeek("Thu")
  case object Friday extends DayOfWeek("Fri")
  case object Saturday extends DayOfWeek("Sat")
  case object Sunday extends DayOfWeek("Sun")

  def ofDay(day: LocalDate): DayOfWeek = day.getDayOfWeek match {
    case JDayOfWeek.MONDAY => Monday
    case JDayOfWeek.TUESDAY => Tuesday
    case JDayOfWeek.WEDNESDAY => Wednesday
    case JDayOfWeek.THURSDAY => Thursday
    case JDayOfWeek.FRIDAY => Friday
    case JDayOfWeek.SATURDAY => Saturday
    case JDayOfWeek.SUNDAY => Sunday
  }

  def parse(string: String): Either[String, DayOfWeek] = string.toUpperCase match {
    case Monday() => Monday.asRight
    case Tuesday() => Tuesday.asRight
    case Wednesday() => Wednesday.asRight
    case Thursday() => Thursday.asRight
    case Friday() => Friday.asRight
    case Saturday() => Saturday.asRight
    case Sunday() => Sunday.asRight
    case _ => "Expected unique prefix of Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, or Sunday".asLeft
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

  implicit val dayOfWeekDecoder: Decoder[DayOfWeek] = Decoder[String].map(_.toUpperCase).emap(parse)

  override val values: IndexedSeq[DayOfWeek] = findValues.sorted(dayOfWeekOrder.toOrdering)
}
