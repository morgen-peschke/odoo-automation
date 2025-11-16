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
          else if (t.isAfter(today)) s"$pretty is not in the past (it is in the future)".asLeft
          else apply(t).asRight
        }
  }
  type DateInThePast = DateInThePast.Type

  object AnyDayButToday extends NewLocalDate("any-day-but-today") {
    override def fromLocalDate(t: LocalDate): Either[String, AnyDayButToday] =
      Either
        .catchNonFatal(LocalDate.now)
        .leftMap(e => s"unable to determine today's date (${e.getMessage})")
        .flatMap { today =>
          def pretty = t.format(NewLocalDate.formatter)
          if (t.isEqual(today)) s"$pretty is today, use --today instead".asLeft
          else apply(t).asRight
        }
  }
  type AnyDayButToday = AnyDayButToday.Type

  object DateInTheFuture extends NewLocalDate("future-date") {
    override def fromLocalDate(t: LocalDate): Either[String, DateInTheFuture] =
      Either
        .catchNonFatal(LocalDate.now)
        .leftMap(e => s"unable to determine today's date (${e.getMessage})")
        .flatMap { today =>
          def pretty = t.format(NewLocalDate.formatter)
          if (t.isEqual(today)) s"$pretty is not in the future (it is today)".asLeft
          else if (t.isBefore(today)) s"$pretty is not in the future (it is in the past)".asLeft
          else apply(t).asRight
        }
  }
  type DateInTheFuture = DateInTheFuture.Type

  object Delta extends PosInt("delta") {
    val MaxSinceDays = 14
    val MaxUntilDays = 14
    override def fromInt(i: Int): Either[String, Delta] =
      super.fromInt(i).flatMap { delta =>
        if (i > MaxSinceDays)
          s"value must be less than or equal to $MaxSinceDays, use an explicit date for further in the past".asLeft
        else delta.asRight
      }
  }
  type Delta = Delta.Type

  case object Today extends DateOverride

  final case class OnDate(date: AnyDayButToday) extends DateOverride
  final case class OnDaysAgo(delta: Delta)      extends DateOverride
  final case class OnDaysHence(delta: Delta)    extends DateOverride
  final case class OnLast(dayOfWeek: DayOfWeek) extends DateOverride
  final case class OnNext(dayOfWeek: DayOfWeek) extends DateOverride

  final case class SinceDate(date: DateInThePast)  extends DateOverride
  final case class SinceDaysAgo(delta: Delta)      extends DateOverride
  final case class SinceLast(dayOfWeek: DayOfWeek) extends DateOverride

  final case class UntilDate(date: DateInTheFuture) extends DateOverride
  final case class UntilDaysHence(delta: Delta)     extends DateOverride
  final case class UntilNext(dayOfWeek: DayOfWeek)  extends DateOverride

  implicit val decoder: Decoder[DateOverride] = anyOf[DateOverride](
    exactly("today").as(Today),
    exactly("yesterday").as(OnDaysAgo(Delta(1))),
    exactly("tomorrow").as(OnDaysAgo(Delta(1))),
    Decoder[AnyDayButToday].at("on:date").map(OnDate),
    Decoder[Delta].at("on:days-ago").map(OnDaysAgo),
    Decoder[Delta].at("on:days-hence").map(OnDaysHence),
    Decoder[DayOfWeek].at("on:last").map(OnLast),
    Decoder[DayOfWeek].at("on:next").map(OnNext),
    Decoder[DateInThePast].at("since:date").map(SinceDate),
    Decoder[Delta].at("since:days-ago").map(SinceDaysAgo),
    Decoder[DayOfWeek].at("since:last").map(SinceLast),
    Decoder[DateInTheFuture].at("until:date").map(UntilDate),
    Decoder[Delta].at("util:days-hence").map(UntilDaysHence),
    Decoder[DayOfWeek].at("until:next").map(UntilNext)
  )

  implicit val order: Order[DateOverride] = Order.by {
    case Today => (0, 0, DayOfWeek.Monday)

    case OnDate(date)       => (1, AnyDayButToday.raw(date).getDayOfYear, DayOfWeek.Monday)
    case OnDaysAgo(delta)   => (2, Delta.raw(delta), DayOfWeek.Monday)
    case OnDaysHence(delta) => (3, Delta.raw(delta), DayOfWeek.Monday)
    case OnLast(dayOfWeek)  => (4, 0, dayOfWeek)
    case OnNext(dayOfWeek)  => (5, 0, dayOfWeek)

    case SinceDate(date)      => (6, DateInThePast.raw(date).getDayOfYear, DayOfWeek.Monday)
    case SinceDaysAgo(delta)  => (7, Delta.raw(delta), DayOfWeek.Monday)
    case SinceLast(dayOfWeek) => (8, 0, dayOfWeek)

    case UntilDate(date)       => (9, DateInTheFuture.raw(date).getDayOfYear, DayOfWeek.Monday)
    case UntilDaysHence(delta) => (10, Delta.raw(delta), DayOfWeek.Monday)
    case UntilNext(dayOfWeek)  => (11, 0, dayOfWeek)
  }
}
