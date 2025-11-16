package peschke.odoo.algebras

import cats.MonadThrow
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.effect.kernel.Clock
import cats.syntax.all._
import peschke.odoo.models.DateOverride
import peschke.odoo.models.DayOfWeek
import peschke.odoo.utils.JavaTime._

import java.time.LocalDate
import java.time.ZoneId
import java.time.ZonedDateTime

trait DateOverrideResolver[F[_]] {
  def resolve(dateOverride: DateOverride): F[NonEmptyList[LocalDate]]
  def resolveAll(dateOverrides: Option[NonEmptySet[DateOverride]]): F[NonEmptyList[LocalDate]]
}
object DateOverrideResolver      {
  def apply[F[_]](implicit DOR: DateOverrideResolver[F]): DOR.type = DOR

  def default[F[_]: MonadThrow: Clock](zoneId: ZoneId): DateOverrideResolver[F] =
    new DateOverrideResolver[F] {
      def todayF: F[LocalDate] = Clock[F].realTimeInstant.map(ZonedDateTime.ofInstant(_, zoneId).toLocalDate)

      override def resolve(dateOverride: DateOverride): F[NonEmptyList[LocalDate]] = dateOverride match {
        case DateOverride.Today => todayF.map(_.pure[NonEmptyList])

        case DateOverride.OnDate(date)    => DateOverride.AnyDayButToday.raw(date).pure[NonEmptyList].pure[F]
        case DateOverride.SinceDate(date) =>
          val target = DateOverride.DateInThePast.raw(date)
          todayF
            .map { today =>
              Iterator
                .iterate(today)(_.minusDays(1L))
                .slice(1, DateOverride.Delta.MaxSinceDays)
                .takeWhile(date => date.isAfter(target) || date.isEqual(target))
                .toList
            }
            .flatMap(NonEmptyList.fromList(_).liftTo[F] {
              new IllegalStateException(s"Unable to calculate dates since $target")
            })
        case DateOverride.UntilDate(date) =>
          val target = DateOverride.DateInTheFuture.raw(date)
          todayF
            .map { today =>
              Iterator
                .iterate(today)(_.plusDays(1L))
                .slice(1, DateOverride.Delta.MaxUntilDays)
                .takeWhile(date => date.isBefore(target) || date.isEqual(target))
                .toList
            }
            .flatMap(NonEmptyList.fromList(_).liftTo[F] {
              new IllegalStateException(s"Unable to calculate dates until $target")
            })

        case DateOverride.OnDaysAgo(delta) =>
          todayF.map(_.minusDays(DateOverride.Delta.raw(delta).toLong).pure[NonEmptyList])

        case DateOverride.OnDaysHence(delta) =>
          todayF.map(_.plusDays(DateOverride.Delta.raw(delta).toLong).pure[NonEmptyList])

        case DateOverride.SinceDaysAgo(delta) =>
          val target = DateOverride.Delta.raw(delta)
          todayF
            .map { today =>
              Iterator
                .iterate(today)(_.minusDays(1L))
                .take(DateOverride.Delta.MaxSinceDays)
                .take(target)
                .toList
            }
            .flatMap(NonEmptyList.fromList(_).liftTo[F] {
              new IllegalStateException(s"Unable to calculate the last $target dates")
            })

        case DateOverride.UntilDaysHence(delta) =>
          val target = DateOverride.Delta.raw(delta)
          todayF
            .map { today =>
              Iterator
                .iterate(today)(_.plusDays(1L))
                .take(DateOverride.Delta.MaxUntilDays)
                .take(target)
                .toList
            }
            .flatMap(NonEmptyList.fromList(_).liftTo[F] {
              new IllegalStateException(s"Unable to calculate the next $target dates")
            })

        case DateOverride.OnLast(target) =>
          todayF
            .flatMap { today =>
              Iterator
                .iterate(today)(_.minusDays(1L))
                .slice(1, 8)
                .dropWhile(DayOfWeek.ofDay(_) =!= target)
                .nextOption()
                .liftTo[F](
                  new IllegalStateException(
                    s"Unable to calculate the date of the last $target before $today"
                  )
                )
            }
            .map(_.pure[NonEmptyList])

        case DateOverride.OnNext(target) =>
          todayF
            .flatMap { today =>
              Iterator
                .iterate(today)(_.plusDays(1L))
                .slice(1, 8)
                .dropWhile(DayOfWeek.ofDay(_) =!= target)
                .nextOption()
                .liftTo[F](
                  new IllegalStateException(
                    s"Unable to calculate the date of the next $target after $today"
                  )
                )
            }
            .map(_.pure[NonEmptyList])

        case DateOverride.SinceLast(target) =>
          todayF
            .map { today =>
              def daysInPast = Iterator.iterate(today)(_.minusDays(1L))

              daysInPast
                .zip(daysInPast.drop(1))
                .take(8)
                .map { case (dayBefore, dayOf) =>
                  (dayOf, DayOfWeek.ofDay(dayBefore))
                }
                .takeWhile(_._2 =!= target)
                .map(_._1)
                .toList
            }
            .flatMap(NonEmptyList.fromList(_).liftTo[F] {
              new IllegalStateException(s"Unable to calculate dates since the last $target")
            })

        case DateOverride.UntilNext(target) =>
          todayF
            .map { today =>
              def daysInFuture = Iterator.iterate(today)(_.plusDays(1L))

              daysInFuture
                .zip(daysInFuture.drop(1))
                .take(8)
                .map { case (dayOf, dayAfter) =>
                  (dayOf, DayOfWeek.ofDay(dayAfter))
                }
                .takeWhile(_._2 =!= target)
                .map(_._1)
                .toList
            }
            .flatMap(NonEmptyList.fromList(_).liftTo[F] {
              new IllegalStateException(s"Unable to calculate dates until the next $target")
            })
      }

      override def resolveAll(dateOverrides: Option[NonEmptySet[DateOverride]]): F[NonEmptyList[LocalDate]] =
        dateOverrides.fold(todayF.map(NonEmptyList.one))(_.toNonEmptyList.flatTraverse(resolve).map(_.distinct.sorted))
    }
}
