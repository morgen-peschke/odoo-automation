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

        case DateOverride.OnExactly(date)    => DateOverride.DateInThePast.raw(date).pure[NonEmptyList].pure[F]
        case DateOverride.SinceExactly(date) =>
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

        case DateOverride.OnDaysAgo(delta)    =>
          todayF.map(_.minusDays(DateOverride.Delta.raw(delta).toLong).pure[NonEmptyList])
        case DateOverride.SinceDaysAgo(delta) =>
          val target = DateOverride.Delta.raw(delta)
          todayF
            .map { today =>
              Iterator
                .iterate(today)(_.minusDays(1L))
                .slice(1, DateOverride.Delta.MaxSinceDays)
                .take(target)
                .toList
            }
            .flatMap(NonEmptyList.fromList(_).liftTo[F] {
              new IllegalStateException(s"Unable to calculate the last $target dates")
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

        case DateOverride.SinceLast(target) =>
          todayF
            .map { today =>
              def daysInPast = Iterator.iterate(today)(_.minusDays(1L))

              daysInPast
                .zip(daysInPast.drop(1))
                .slice(1, 8)
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
      }

      override def resolveAll(dateOverrides: Option[NonEmptySet[DateOverride]]): F[NonEmptyList[LocalDate]] =
        dateOverrides.fold(todayF.map(NonEmptyList.one))(_.toNonEmptyList.flatTraverse(resolve).map(_.distinct.sorted))

    }
}
