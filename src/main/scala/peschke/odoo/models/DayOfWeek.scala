package peschke.odoo.models

import cats.Order
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.syntax.all._
import com.monovore.decline.Argument
import io.circe.Decoder
import peschke.odoo.utils.ArgumentHelpers

import java.time.LocalDate
import java.time.{DayOfWeek => JDayOfWeek}

sealed abstract class DayOfWeek(val shortName: String, val index: Int)
    extends enumeratum.EnumEntry
    with Product
    with Serializable {

  def fullName: String = entryName
}
object DayOfWeek extends enumeratum.Enum[DayOfWeek] {

  case object Monday    extends DayOfWeek("Mon", 1)
  case object Tuesday   extends DayOfWeek("Tue", 2)
  case object Wednesday extends DayOfWeek("Wed", 3)
  case object Thursday  extends DayOfWeek("Thu", 4)
  case object Friday    extends DayOfWeek("Fri", 5)
  case object Saturday  extends DayOfWeek("Sat", 6)
  case object Sunday    extends DayOfWeek("Sun", 7)

  def ofDay(day: LocalDate): DayOfWeek = day.getDayOfWeek match {
    case JDayOfWeek.MONDAY    => Monday
    case JDayOfWeek.TUESDAY   => Tuesday
    case JDayOfWeek.WEDNESDAY => Wednesday
    case JDayOfWeek.THURSDAY  => Thursday
    case JDayOfWeek.FRIDAY    => Friday
    case JDayOfWeek.SATURDAY  => Saturday
    case JDayOfWeek.SUNDAY    => Sunday
  }

  def parse(string: String): Either[String, DayOfWeek] =
    Argument[DayOfWeek].read(string).toEither.leftMap(_.mkString_("\n"))

  implicit val dayOfWeekOrder: Order[DayOfWeek] = Order.by(_.index)

  val WeekDays: NonEmptySet[DayOfWeek] = NonEmptySet.of(Monday, Tuesday, Wednesday, Thursday, Friday)
  val WeekEnd: NonEmptySet[DayOfWeek] = NonEmptySet.of(Saturday, Sunday)

  override val values: IndexedSeq[DayOfWeek] = findValues.sorted(dayOfWeekOrder.toOrdering)
  val valuesNel: NonEmptyList[DayOfWeek] =
    NonEmptyList.fromList(values.toList).map(_.sortBy(_.index)).getOrElse {
      throw new IllegalStateException("DayOfWeek has no values")
    }

  implicit val argument: Argument[DayOfWeek] = ArgumentHelpers.enumArgument[DayOfWeek]
  implicit val dayOfWeekDecoder: Decoder[DayOfWeek] = Decoder[String].emap(parse)
}
