package peschke.odoo.algebras

import cats.Monad
import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptySet}
import cats.syntax.all._
import peschke.odoo.models.Frequency.Cycled.{Anchor, Cycle, Length}
import peschke.odoo.models.Frequency._
import peschke.odoo.models.{DateOverride, DayOfWeek, Frequency}
import peschke.odoo.utils.JavaTime._

import java.time.{LocalDate, Year}
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
    override def generate(frequency: Frequency): List[LocalDate] =
      frequency match {
        case Never                    => Nil
        case Daily                    => dates.toList
        case Weekly(days)             => dates.filter(d => days.contains(DayOfWeek.ofDay(d)))
        case Cycled(starting, cycles) => filterCycled(dates, starting, cycles).toList
      }

    private def daysIterator(start: LocalDate, length: Length): Iterator[LocalDate] =
      Iterator.iterate(start)(_.plusDays(1L)).take(length.int)

    private def filterCycled
      (dates: NonEmptyList[LocalDate], anchor: Anchor, cycles: NonEmptyList[Cycle])
      : Chain[LocalDate] =
      anchor match {
        case Anchor.Fixed(on)      => filterCycledStarting(dates, on, cycles)
        case Anchor.Monthly(every) =>
          NonEmptyChain
            .fromNonEmptyList(dates.groupByNem(ld => (ld.getYear, ld.getMonth)).toNel)
            .toChain
            .flatMap { case ((year, month), datesInYearMonth) =>
              filterCycledStarting(datesInYearMonth, Year.of(year).atMonth(month).atDay(every.int), cycles)
            }
            .distinct
      }

    private def filterCycledStarting
      (dates: NonEmptyList[LocalDate], anchor: LocalDate, cycles: NonEmptyList[Cycle])
      : Chain[LocalDate] = {
      val cycleLength = cycles.map(_.length.int).sumAll.toLong
      val startOfWindow = dates.minimum
      val endOfWindow = dates.maximum
      if (anchor.isAfter(endOfWindow)) Chain.empty
      else {
        val daysSinceStart = anchor.until(startOfWindow, ChronoUnit.DAYS)
        val cyclesToSkip = daysSinceStart / cycleLength
        val adjustedStart = {
          val trial = anchor.plusDays(cycleLength * cyclesToSkip)
          if (trial.isBefore(startOfWindow)) trial
          else anchor.plusDays(cycleLength * (cyclesToSkip - 1))
        }

        @tailrec
        def loop(cursor: LocalDate, remainingCycles: List[Cycle], accum: Chain[LocalDate]): Chain[LocalDate] =
          if (cursor.isAfter(endOfWindow)) accum
          else
            remainingCycles match {
              case Nil                                              => loop(cursor, cycles.toList, accum)
              case Cycle(length, cycleFrequency) :: remainingCycles =>
                loop(
                  cursor.plusDays(length.int.toLong),
                  remainingCycles,
                  accum.concat(filterUnanchored(cursor, length, cycleFrequency))
                )
            }

        loop(adjustedStart, cycles.toList, Chain.empty)
          .filterNot(date => date.isBefore(startOfWindow) || date.isAfter(endOfWindow))
      }
    }

    private def filterUnanchored
      (startDate: LocalDate, length: Length, frequency: Frequency.Unanchored)
      : Chain[LocalDate] =
      frequency match {
        case Never                  => Chain.empty
        case Daily                  => Chain.fromIterableOnce(daysIterator(startDate, length))
        case Weekly(days)           =>
          Chain.fromIterableOnce {
            daysIterator(startDate, length).filter(d => days.contains(DayOfWeek.ofDay(d)))
          }
        case FloatingCycles(cycles) =>
          val endDate = startDate.plusDays(length.int.toLong - 1)
          @tailrec
          def loop(cursor: LocalDate, remainingCycles: List[Cycle], accum: Chain[LocalDate]): Chain[LocalDate] =
            if (cursor.isAfter(endDate)) accum
            else
              remainingCycles match {
                case Nil                                                   => loop(cursor, cycles.toList, accum)
                case Cycle(cycleLength, cycleFrequency) :: remainingCycles =>
                  loop(
                    cursor.plusDays(cycleLength.int.toLong),
                    remainingCycles,
                    accum.concat(filterUnanchored(cursor, cycleLength, cycleFrequency))
                  )
              }

          loop(startDate, cycles.toList, Chain.empty).filterNot(_.isAfter(endDate))
      }
  }
}
