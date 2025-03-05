package peschke.odoo.models

import cats.Order
import cats.syntax.all._
import io.circe.Decoder
import peschke.odoo.utils.Circe._

import java.time.LocalDate

sealed trait DateOverride
object DateOverride {
  object DateInThePast extends NewLocalDate("past-date") {
    override def fromLocalDate(t: LocalDate): Either[String, DateInThePast] =
      Either
        .catchNonFatal(LocalDate.now)
        .leftMap(e => s"unable to determine today's date (${e.getMessage})")
        .flatMap { today =>
          def pretty = t.format(NewLocalDate.formatter)
          if (t.isEqual(today)) s"$pretty is not in the past (it is today)".asLeft
          else if (t.isAfter(today)) s"$pretty is not in the past (it in the future)".asLeft
          else apply(t).asRight
        }
  }
  type DateInThePast = DateInThePast.Type

  object Delta extends PosInt("delta") {
    val MaxSinceDays = 14
    override def fromInt(i: Int): Either[String, Delta] =
      super.fromInt(i).flatMap { delta =>
        if (i > MaxSinceDays)
          s"value must be less than or equal to $MaxSinceDays, use an explicit date for further in the past".asLeft
        else delta.asRight
      }
  }
  type Delta = Delta.Type

  case object Today                                  extends DateOverride
  final case class OnExactly(date: DateInThePast)    extends DateOverride
  final case class SinceExactly(date: DateInThePast) extends DateOverride
  final case class OnDaysAgo(delta: Delta)           extends DateOverride
  final case class SinceDaysAgo(delta: Delta)        extends DateOverride
  final case class OnLast(dayOfWeek: DayOfWeek)      extends DateOverride
  final case class SinceLast(dayOfWeek: DayOfWeek)   extends DateOverride

  implicit val decoder: Decoder[DateOverride] = anyOf[DateOverride](
    fixed("today").as(Today),
    fixed("yesterday").as(OnDaysAgo(Delta(1))),
    Decoder[DateInThePast].at("on:exactly").map(OnExactly),
    Decoder[DateInThePast].at("since:exactly").map(SinceExactly),
    Decoder[Delta].at("on:days-ago").map(OnDaysAgo),
    Decoder[Delta].at("since:days-ago").map(SinceDaysAgo),
    Decoder[DayOfWeek].at("on:last").map(OnLast),
    Decoder[DayOfWeek].at("since:last").map(SinceLast)
  )

  implicit val order: Order[DateOverride] = Order.by {
    case Today                => (0, 0, DayOfWeek.Monday)
    case OnDaysAgo(n)         => (2, Delta.raw(n), DayOfWeek.Monday)
    case OnLast(dayOfWeek)    => (3, 0, dayOfWeek)
    case OnExactly(date)      => (4, DateInThePast.raw(date).getDayOfYear, DayOfWeek.Monday)
    case SinceDaysAgo(n)      => (5, Delta.raw(n), DayOfWeek.Monday)
    case SinceLast(dayOfWeek) => (6, 0, dayOfWeek)
    case SinceExactly(date)   => (7, DateInThePast.raw(date).getDayOfYear, DayOfWeek.Monday)
  }
}
