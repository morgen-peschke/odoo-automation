package peschke.odoo.algebras

import cats.Monad
import cats.data.{NonEmptyList, NonEmptySet}
import cats.syntax.all._
import peschke.odoo.models.Frequency.Cycled.{Cycle, Length}
import peschke.odoo.models.Frequency._
import peschke.odoo.models.{DateOverride, DayOfWeek, Frequency}
import peschke.odoo.utils.JavaTime._

import java.time.LocalDate
import java.time.temporal.ChronoUnit
import scala.annotation.tailrec

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
    private val startOfWindow = dates.minimum
    private val endOfWindow = dates.maximum

    override def generate(frequency: Frequency): List[LocalDate] = frequency match {
      case Never                    => Nil
      case Daily                    => dates.toList
      case Weekly(days)             => dates.filter(d => days.contains(DayOfWeek.ofDay(d)))
      case Cycled(starting, cycles) =>
        val cycleLength = cycles.map(_.length.int).sumAll.toLong
        if (starting.isAfter(endOfWindow)) Nil
        else {
          val daysSinceStart = starting.until(startOfWindow, ChronoUnit.DAYS)
          val cyclesToSkip = daysSinceStart / cycleLength
          val adjustedStart = {
            val trial = starting.plusDays(cycleLength * cyclesToSkip)
            if (trial.isBefore(startOfWindow)) trial
            else starting.plusDays(cycleLength * (cyclesToSkip - 1))
          }

          val builder = List.newBuilder[LocalDate]
          @tailrec
          def loop(cursor: LocalDate, remainingCycles: List[Cycle]): List[LocalDate] =
            if (cursor.isAfter(endOfWindow)) builder.result()
            else
              remainingCycles match {
                case Nil                                              => loop(cursor, cycles.toList)
                case Cycle(length, cycleFrequency) :: remainingCycles =>
                  builder.addAll {
                    filter(cursor, length, cycleFrequency)
                      .view
                      .filterNot(_.isBefore(startOfWindow))
                      .filterNot(_.isAfter(endOfWindow))
                      .toList
                  }
                  loop(cursor.plusDays(length.int.toLong), remainingCycles)
              }

          loop(adjustedStart, cycles.toList)
        }
    }

    private def filter(startDate: LocalDate, length: Length, frequency: Frequency.Unanchored): List[LocalDate] =
      frequency match {
        case Never                  => Nil
        case Daily                  => Iterator.iterate(startDate)(_.plusDays(1L)).take(length.int).toList
        case Weekly(days)           => dates.filter(d => days.contains(DayOfWeek.ofDay(d)))
        case FloatingCycles(cycles) =>
          val endDate = startDate.plusDays(length.int.toLong - 1)
          val builder = List.newBuilder[LocalDate]
          @tailrec
          def loop(cursor: LocalDate, remainingCycles: List[Cycle]): List[LocalDate] =
            if (cursor.isAfter(endDate)) builder.result()
            else
              remainingCycles match {
                case Nil                                              => loop(cursor, cycles.toList)
                case Cycle(length, cycleFrequency) :: remainingCycles =>
                  builder.addAll {
                    filter(cursor, length, cycleFrequency)
                      .view
                      .filterNot(_.isAfter(endDate))
                      .toList
                  }
                  loop(cursor.plusDays(length.int.toLong), remainingCycles)
              }

          loop(startDate, cycles.toList)
      }
  }
}
