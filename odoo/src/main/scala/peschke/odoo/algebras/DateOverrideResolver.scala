package peschke.odoo.algebras

import cats.MonadThrow
import cats.data.{NonEmptyList, NonEmptySet}
import cats.effect.kernel.Clock
import cats.syntax.all._
import peschke.odoo.models.{DateOverride, DayOfWeek}
import peschke.odoo.utils.JavaTime._

import java.time.{LocalDate, ZoneId, ZonedDateTime}

trait DateOverrideResolver[F[_]] {
  def resolve(dateOverride: DateOverride): F[NonEmptyList[LocalDate]]
  def resolveAll(dateOverrides: Option[NonEmptySet[DateOverride]]): F[NonEmptyList[LocalDate]]
}
object DateOverrideResolver      {
  def apply[F[_]](implicit DOR: DateOverrideResolver[F]): DOR.type = DOR

  def default[F[_]: MonadThrow: Clock](zoneId: ZoneId): DateOverrideResolver[F] =
    new DateOverrideResolver[F] {
      def todayF: F[LocalDate] = Clock[F].realTimeInstant.map(ZonedDateTime.ofInstant(_, zoneId).toLocalDate)

      def pastDaysF: F[Iterator[LocalDate]] = todayF.map(Iterator.iterate(_)(_.minusDays(1L)).drop(1))

      def futureDaysF: F[Iterator[LocalDate]] = todayF.map(Iterator.iterate(_)(_.plusDays(1L)).drop(1))

      def lastF(target: DayOfWeek): F[(Iterator[LocalDate], Option[LocalDate])] =
        pastDaysF
          .map { pastDays =>
            val (daysSince, daysBefore) = pastDays.take(8).span(DayOfWeek.ofDay(_) =!= target)

            (daysSince, daysBefore.nextOption())
          }

      def nextF(target: DayOfWeek): F[(Iterator[LocalDate], Option[LocalDate])] =
        futureDaysF
          .map { futureDays =>
            val (daysUntil, daysAfter) = futureDays.take(8).span(DayOfWeek.ofDay(_) =!= target)

            (daysUntil, daysAfter.nextOption())
          }

      override def resolve(dateOverride: DateOverride): F[NonEmptyList[LocalDate]] = dateOverride match {
        case DateOverride.Today => todayF.map(_.pure[NonEmptyList])

        case DateOverride.OnDate(date) => DateOverride.AnyDayButToday.raw(date).pure[NonEmptyList].pure[F]

        case DateOverride.SinceDate(date) =>
          val target = DateOverride.DateInThePast.raw(date)
          pastDaysF
            .map(
              _.take(DateOverride.Delta.MaxSinceDays)
                .takeWhile(!_.isBefore(target))
                .toList
            )
            .flatMap(NonEmptyList.fromList(_).liftTo[F] {
              new IllegalStateException(s"Unable to calculate dates since $target")
            })

        case DateOverride.UntilDate(date) =>
          val target = DateOverride.DateInTheFuture.raw(date)
          futureDaysF
            .map(
              _.take(DateOverride.Delta.MaxUntilDays)
                .takeWhile(!_.isAfter(target))
                .toList
            )
            .flatMap(NonEmptyList.fromList(_).liftTo[F] {
              new IllegalStateException(s"Unable to calculate dates until $target")
            })

        case DateOverride.OnDaysAgo(delta) =>
          todayF.map(_.minusDays(DateOverride.Delta.raw(delta).toLong).pure[NonEmptyList])

        case DateOverride.OnDaysHence(delta) =>
          todayF.map(_.plusDays(DateOverride.Delta.raw(delta).toLong).pure[NonEmptyList])

        case DateOverride.SinceDaysAgo(delta) =>
          val target = DateOverride.Delta.raw(delta)
          pastDaysF
            .map(
              _.take(DateOverride.Delta.MaxSinceDays)
                .take(target)
                .toList
            )
            .flatMap(NonEmptyList.fromList(_).liftTo[F] {
              new IllegalStateException(s"Unable to calculate the last $target dates")
            })

        case DateOverride.UntilDaysHence(delta) =>
          val target = DateOverride.Delta.raw(delta)
          futureDaysF
            .map(
              _.take(DateOverride.Delta.MaxUntilDays)
                .take(target)
                .toList
            )
            .flatMap(NonEmptyList.fromList(_).liftTo[F] {
              new IllegalStateException(s"Unable to calculate the next $target dates")
            })

        case DateOverride.OnLast(target) =>
          lastF(target)
            .map(_._2.toList)
            .flatMap(NonEmptyList.fromList(_).liftTo[F] {
              new IllegalStateException(s"Unable to calculate the date of the last $target before today")
            })

        case DateOverride.OnNext(target) =>
          nextF(target)
            .map(_._2.toList)
            .flatMap(NonEmptyList.fromList(_).liftTo[F] {
              new IllegalStateException(s"Unable to calculate the date of the next $target after today")
            })

        case DateOverride.SinceLast(target) =>
          lastF(target)
            .map { case (daysSince, lastOpt) =>
              List.newBuilder.addAll(daysSince).addAll(lastOpt).result()
            }
            .flatMap(NonEmptyList.fromList(_).liftTo[F] {
              new IllegalStateException(s"Unable to calculate dates since the last $target")
            })

        case DateOverride.UntilNext(target) =>
          nextF(target)
            .map { case (daysUntil, nextOpt) =>
              List.newBuilder.addAll(daysUntil).addAll(nextOpt).result()
            }
            .flatMap(NonEmptyList.fromList(_).liftTo[F] {
              new IllegalStateException(s"Unable to calculate dates until the next $target")
            })
      }

      override def resolveAll(dateOverrides: Option[NonEmptySet[DateOverride]]): F[NonEmptyList[LocalDate]] =
        dateOverrides.fold(todayF.map(NonEmptyList.one))(_.toNonEmptyList.flatTraverse(resolve).map(_.distinct.sorted))
    }
}
