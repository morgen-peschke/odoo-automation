package peschke.odoo.algebras

import cats.syntax.all._
import cats.data.{NonEmptyList, NonEmptySet}
import munit.{FunSuite, Location}
import peschke.odoo.models.DayOfWeek
import peschke.odoo.models.DayOfWeek.{Friday, Monday, Wednesday}
import peschke.odoo.models.Frequency.Cycled.{Cycle, Length}
import peschke.odoo.models.Frequency.{Cycled, Daily, FloatingCycles, Never, Weekly}

import java.time.{LocalDate, Month, Year}

/** Helpful reference for this test suite:
  * {{{
  *      July 2000
  * Su Mo Tu We Th Fr Sa
  *                    1
  *  2  3  4  5  6  7  8
  *  9 10 11 12 13 14 15
  * 16 17 18 19 20 21 22
  * 23 24 25 26 27 28 29
  * 30 31
  *       August 2000
  * Su Mo Tu We Th Fr Sa
  *        1  2  3  4  5
  *  6  7  8  9 10 11 12
  * 13 14 15 16 17 18 19
  * 20 21 22 23 24 25 26
  * 27 28 29 30 31
  * }}}
  */
class DatesGeneratorTest extends FunSuite {
  private val year = Year.of(2000)
  private def august(day: Int): LocalDate = year.atMonth(Month.AUGUST).atDay(day)
  private def august(days: Range)(implicit loc: Location): NonEmptyList[LocalDate] =
    NonEmptyList.fromList(days.map(august).toList).getOrElse {
      fail("august(Range) requires a non-empty range")
    }
  private val standardDateRange = NonEmptyList.of(
    august(1),
    august(2),
    august(3),
    august(4),
    august(5),
    august(6),
    august(7)
  )
  private val standardGenerator = DatesGenerator.forDateRange(standardDateRange)
  private def length(i: Int)(implicit loc: Location): Length =
    Length.fromInt(i).valueOr(fail(_))

  test("generate(Never) returns the empty list") {
    assertEquals(
      standardGenerator.generate(Never),
      Nil
    )
  }

  test("generate(Daily) returns the complete date range") {
    assertEquals(
      standardGenerator.generate(Daily),
      standardDateRange.toList
    )
  }

  test("generate(Weekly) returns only the correct days of the week") {
    assertEquals(
      standardGenerator.generate(
        Weekly(
          NonEmptySet.of(
            Monday,
            Wednesday,
            Friday
          )
        )
      ),
      List(august(2), august(4), august(7)),
      clues(standardDateRange.fproduct(DayOfWeek.ofDay))
    )
  }

  test("generate(Cycled) returns the empty list when before start date") {
    assertEquals(
      standardGenerator.generate(Cycled(august(10), NonEmptyList.one(Cycle(length(3), Daily)))),
      Nil
    )
  }

  test("generate(Cycled) gets to the start of the current window quickly") {
    assertEquals(
      standardGenerator.generate(
        Cycled(
          LocalDate.of(1, Month.JANUARY, 1),
          NonEmptyList.one(Cycle(length(1), Daily))
        )
      ),
      standardDateRange.toList
    )
  }

  test("generate(Cycled) handles out-of-sync cycles and windows") {
    assertEquals(
      standardGenerator.generate(
        Cycled(
          year.atMonth(Month.JULY).atDay(15),
          NonEmptyList.of(
            Cycle(length(7), Daily),
            Cycle(length(7), Never)
          )
        )
      ),
      List(
        august(1),
        august(2),
        august(3),
        august(4)
      )
    )
  }

  test("generate(Cycled) every other day for a week, then off one week") {
    assertEquals(
      DatesGenerator.forDateRange(august(1 to 31)).generate {
        Cycled(
          august(8),
          NonEmptyList.of(
            Cycle(
              length(7),
              FloatingCycles(
                NonEmptyList.of(
                  Cycle(length(1), Daily),
                  Cycle(length(1), Never)
                )
              )
            ),
            Cycle(length(7), Never)
          )
        )
      },
      List(
        august(8),
        august(10),
        august(12),
        august(14),
        august(22),
        august(24),
        august(26),
        august(28)
      )
    )
  }

  test("generate(Cycled) with a nested cycle truncates the inner cycle to the outer cycle") {
    assertEquals(
      DatesGenerator.forDateRange(august(1 to 14)).generate {
        Cycled(
          august(1),
          NonEmptyList.of(
            Cycle(
              length(7),
              FloatingCycles(
                NonEmptyList.one(
                  Cycle(length(10), Daily)
                )
              )
            ),
            Cycle(length(7), Never)
          )
        )
      },
      List(
        august(1),
        august(2),
        august(3),
        august(4),
        august(5),
        august(6),
        august(7)
      )
    )
  }
}
