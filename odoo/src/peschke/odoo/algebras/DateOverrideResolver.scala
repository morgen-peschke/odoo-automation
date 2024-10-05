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
  def resolve(dateOverride: DateOverride): F[LocalDate]
  def resolveAll(dateOverrides: Option[NonEmptySet[DateOverride]]): F[NonEmptyList[LocalDate]]
}
object DateOverrideResolver      {
  def apply[F[_]](implicit DOR: DateOverrideResolver[F]): DOR.type = DOR

  def default[F[_]: MonadThrow: Clock](zoneId: ZoneId): DateOverrideResolver[F] =
    new DateOverrideResolver[F] {
      def todayF: F[LocalDate] = Clock[F].realTimeInstant.map(ZonedDateTime.ofInstant(_, zoneId).toLocalDate)

      override def resolve(dateOverride: DateOverride): F[LocalDate] = dateOverride match {
        case DateOverride.Exactly(date)  => date.pure[F]
        case DateOverride.Today          => todayF
        case DateOverride.DaysAgo(delta) => todayF.map(_.minusDays(delta.toLong))
        case DateOverride.Last(target)   =>
          todayF
            .flatMap { today =>
              Iterator
                .iterate(today)(_.minusDays(1L))
                .slice(1, 8)
                .dropWhile(DayOfWeek.ofDay(_) =!= target)
                .nextOption()
                .liftTo[F](
                  new IllegalStateException(
                    s"Unable to calculate last $target before $today"
                  )
                )
            }
      }

      override def resolveAll(dateOverrides: Option[NonEmptySet[DateOverride]]): F[NonEmptyList[LocalDate]] =
        dateOverrides.fold(todayF.map(NonEmptyList.one))(_.toNonEmptyList.traverse(resolve).map(_.distinct.sorted))

    }
}
