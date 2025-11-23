package peschke.odoo.algebras

import cats.Monad
import cats.data.{NonEmptyList, NonEmptySet}
import cats.syntax.all._
import peschke.odoo.models.{DateOverride, DayOfWeek, Frequency}

import java.time.LocalDate

trait DatesGenerator  {
  def generate(frequency: Frequency): List[LocalDate]
}
object DatesGenerator {
  def default[F[_]: Monad]
    (resolver:      DateOverrideResolver[F])
    (dateOverrides: Option[NonEmptySet[DateOverride]])
    : F[DatesGenerator] =
    resolver.resolveAll(dateOverrides).map(forDateRange)

  def forDateRange(dates: NonEmptyList[LocalDate]): DatesGenerator = new DatesGenerator {
    private val augmented = dates.fproductLeft(DayOfWeek.ofDay)
    override def generate(frequency: Frequency): List[LocalDate] = frequency match {
      case Frequency.Daily        => dates.toList
      case Frequency.Weekly(days) =>
        augmented.filter(a => days.contains(a._1))._2F
    }
  }
}
