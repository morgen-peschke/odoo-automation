package peschke.odoo.models

import cats.Order
import cats.data.{NonEmptyList, NonEmptySet}
import cats.syntax.all._
import io.circe.{Decoder, Encoder}
import peschke.odoo.models.Template.Entry
import supertagged.NewType

import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.{LocalDateTime, LocalTime}

final case class Template (entries: NonEmptyList[Entry])
object Template {
  object EntryLabel extends NonEmptyString("Person name") {
    implicit final class Ops(private val t: Type) extends AnyVal {
      def string: String = raw(t)
    }
  }
  type EntryLabel = EntryLabel.Type

  object PickingNameTemplate extends NonEmptyString("Picking name template") {
    override def fromString(raw: String): Either[String, Type] =
      apply(raw.stripSuffix("/").stripPrefix("/")).asRight
  }

  type PickingNameTemplate = PickingNameTemplate.Type

  final case class Entry(label: EntryLabel,
                         pickings: NonEmptyList[PickingTemplate])

  object MoveType extends NonEmptyString("move_type")
  type MoveType = MoveType.Type

  object PickingTypeId extends PosInt("picking_type_id")
  type PickingTypeId = PickingTypeId.Type

  object LocationId extends PosInt("location_id")
  type LocationId = LocationId.Type

  object LocationDestId extends PosInt("location_dest_id")
  type LocationDestId = LocationDestId.Type

  object PartnerId extends PosInt("partner_id")
  type PartnerId = PartnerId.Type

  object ScheduledDate extends supertagged.NewType[LocalDateTime] {
    private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

    implicit val encoder: Encoder[Type] = Encoder[String].contramap(raw(_).format(formatter))
    implicit val decoder: Decoder[Type] = Decoder[String].emap { s =>
      Either.catchOnly[DateTimeParseException](LocalDateTime.parse(s, formatter))
        .bimap(e => s"Invalid date string: ${e.getMessage}", apply(_))
    }
  }

  type ScheduledDate = ScheduledDate.Type

  sealed abstract class TimeOfDay(val shortName: String) extends enumeratum.EnumEntry {
    def fullName: String = entryName

    def unapply(raw: String): Boolean =
      shortName.equalsIgnoreCase(raw) || fullName.equalsIgnoreCase(raw)
  }
  object TimeOfDay extends enumeratum.Enum[TimeOfDay] {
    case object Morning extends TimeOfDay("AM")
    case object Noon extends TimeOfDay("NN")
    case object Night extends TimeOfDay("PM")
    case object AnyTime extends TimeOfDay("ANY") {
      override def entryName: String = "ANY"
    }

    def parse(raw: String): Either[String, TimeOfDay] = raw match {
      case Morning() => Morning.asRight
      case Noon() => Noon.asRight
      case Night() => Night.asRight
      case _ => "Expected on of: Morning, AM, Noon, Night, PM, or ANY".asLeft
    }

    implicit val decoder: Decoder[TimeOfDay] = Decoder[String].emap(parse)

    implicit val order: Order[TimeOfDay] = Order.by {
      case Morning => 1
      case Noon => 2
      case Night => 3
      case AnyTime => 4
    }

    val All: NonEmptySet[TimeOfDay] = NonEmptySet.of(Morning, Noon, Night, AnyTime)

    object MorningTime extends NewLocalTime("time in AM") {
      val Default: Type = apply(LocalTime.of(9, 0))

      override def fromLocalTime(t: LocalTime): Either[String, MorningTime.Type] =
        if (t.isBefore(LocalTime.NOON)) apply(t).asRight
        else s"$name must be before noon".asLeft
    }
    type MorningTime = MorningTime.Type

    object NightTime extends NewLocalTime("time in PM") {
      val Default: Type = apply(LocalTime.of(18, 0))

      override def fromLocalTime(t: LocalTime): Either[String, NightTime.Type] =
        if (t.isAfter(LocalTime.NOON)) apply(t).asRight
        else s"$name must be after noon".asLeft
    }
    type NightTime = NightTime.Type

    object ScheduleAt extends NewType[(MorningTime, NightTime)] {
      implicit final class NameOps(private val t: Type) extends AnyVal {
        def am: MorningTime = raw(t)._1
        def pm: NightTime = raw(t)._2
      }
    }
    type ScheduleAt = ScheduleAt.Type

    object ScheduleAtOverrides extends NewType[(Option[MorningTime], Option[NightTime])] {
      implicit final class ConvertOpts(private val t: Type) extends AnyVal {
        def asScheduleAt: ScheduleAt = ScheduleAt((
          raw(t)._1.getOrElse(TimeOfDay.MorningTime.Default),
          raw(t)._2.getOrElse(TimeOfDay.NightTime.Default)
        ))
      }
    }
    type ScheduleAtOverrides = ScheduleAtOverrides.Type

    override def values: IndexedSeq[TimeOfDay] = findValues
  }

  final case class PickingTemplate(frequency: Frequency,
                                   timeOfDay: TimeOfDay,
                                   pickingName: PickingNameTemplate,
                                   moveType: MoveType,
                                   pickingTypeId: PickingTypeId,
                                   locationId: LocationId,
                                   locationDestId: LocationDestId,
                                   partnerId: PartnerId,
                                   disabled: Boolean,
                                   moves: MoveTemplateSet)

  object MoveName extends NonEmptyString("Move name")
  type MoveName = MoveName.Type

  object ProductId extends PosInt("product_id")
  type ProductId = ProductId.Type

  object ProductQuantity extends PosDouble("product_uom_qty")
  type ProductQuantity = ProductQuantity.Type

  sealed trait QuantityType
  object QuantityType {
    final case class Add(quantity: ProductQuantity) extends QuantityType
    final case class Fill(maxQuantity: ProductQuantity) extends QuantityType
    case object All extends QuantityType
  }

  final case class MoveTemplate(name: MoveName, productId: ProductId, quantityType: QuantityType)

  sealed trait MoveTemplateSet
  object MoveTemplateSet {
    final case class Explicit(moves: NonEmptyList[MoveTemplate]) extends MoveTemplateSet
    case object All extends MoveTemplateSet
  }
}