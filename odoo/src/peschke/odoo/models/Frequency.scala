package peschke.odoo.models

import cats.Defer
import cats.data.{NonEmptyList, NonEmptySet}
import cats.syntax.all._
import io.circe.Decoder
import peschke.odoo.models.Frequency.Cycled.Length
import peschke.odoo.utils.Circe._

import java.time.LocalDate

sealed trait Frequency
object Frequency {
  sealed trait Unanchored
  case object Never                                                                extends Frequency with Unanchored
  case object Daily                                                                extends Frequency with Unanchored
  final case class Weekly(days: NonEmptySet[DayOfWeek])                            extends Frequency with Unanchored
  final case class FloatingCycles(cycles: NonEmptyList[Cycled.Cycle])              extends Unanchored
  final case class Cycled(starting: LocalDate, cycles: NonEmptyList[Cycled.Cycle]) extends Frequency
  object Cycled {
    object Length extends PosInt("length")
    type Length = Length.Type
    final case class Cycle(length: Length, frequency: Frequency.Unanchored)
  }

  private val commonDecoder: Decoder[Frequency with Unanchored] = anyOf[Frequency with Unanchored](
    exactly("never").as(Never),
    exactly("daily").as(Daily),
    exactly("weekdays").as(Weekly(DayOfWeek.WeekDays)),
    exactly("weekends").as(Weekly(DayOfWeek.WeekEnd)),
    Decoder[NonEmptySet[DayOfWeek]].map(Weekly),
    Decoder[DayOfWeek].map(NonEmptySet.one(_)).map(Weekly)
  )

  private implicit val unanchoredDecoder: Decoder[Unanchored] = Defer[Decoder].fix { recurse =>
    implicit val cycle: Decoder[Cycled.Cycle] = Decoder.accumulatingInstance { c =>
      (c.getAcc[Length]("length"), c.getAcc("frequency")(recurse)).mapN(Cycled.Cycle)
    }

    anyOf[Unanchored](
      commonDecoder.widen,
      Decoder[NonEmptyList[Cycled.Cycle]].at("cycles").map(FloatingCycles.apply).widen
    )
  }

  implicit val decoder: Decoder[Frequency] = {
    implicit val cycle: Decoder[Cycled.Cycle] =
      Decoder.forProduct2[Cycled.Cycle, Length, Unanchored]("length", "frequency")(Cycled.Cycle.apply)

    anyOf[Frequency](
      commonDecoder.widen,
      Decoder.forProduct2[Cycled, LocalDate, NonEmptyList[Cycled.Cycle]]("starting", "cycles")(Cycled.apply).widen
    )
  }
}
