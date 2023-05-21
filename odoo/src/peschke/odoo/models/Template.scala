package peschke.odoo.models

import cats.{Order, Show}
import cats.syntax.all._
import cats.data.{NonEmptyList, NonEmptySet}
import io.circe.Decoder
import peschke.odoo.models
import peschke.odoo.models.Template.Entry

final case class Template (entries: NonEmptyList[Entry])
object Template {
  object PersonName extends NonEmptyString("Person name")
  type PersonName = PersonName.Type

  object PickingNamePrefix extends NonEmptyString("Picking name prefix") {
    override def fromString(raw: String): Either[String, models.Template.PickingNamePrefix.Type] =
      super.fromString(raw.stripSuffix("/"))
  }
  type PickingNamePrefix = PickingNamePrefix.Type

  final case class Entry(person: PersonName,
                         namePrefix: PickingNamePrefix,
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

  sealed trait TimeOfDay
  object TimeOfDay {
    case object Morning extends TimeOfDay
    case object Noon extends TimeOfDay
    case object Night extends TimeOfDay

    def fromString(raw: String): Either[String, TimeOfDay] = raw.toUpperCase match {
      case "MORNING" | "AM" => Morning.asRight
      case "NOON" => Noon.asRight
      case "NIGHT" | "PM" => Night.asRight
      case _ => "Expected on of: Morning, AM, Noon, Night, PM".asLeft
    }

    implicit val show: Show[TimeOfDay] = Show.show {
      case Morning => "Morning"
      case Noon => "Noon"
      case Night => "Night"
    }

    implicit val decoder: Decoder[TimeOfDay] = Decoder[String].emap(fromString)

    implicit val order: Order[TimeOfDay] = Order.by {
      case Morning => 1
      case Noon => 2
      case Night => 3
    }

    val All: NonEmptySet[TimeOfDay] = NonEmptySet.of(Morning, Noon, Night)
  }

  final case class PickingTemplate(frequency: Frequency,
                                   timeOfDay: TimeOfDay,
                                   moveType: MoveType,
                                   pickingTypeId: PickingTypeId,
                                   locationId: LocationId,
                                   locationDestId: LocationDestId,
                                   partnerId: PartnerId,
                                   moves: NonEmptyList[MoveTemplate])

  object MoveName extends NonEmptyString("Move name")
  type MoveName = MoveName.Type

  object ProductId extends PosInt("product_id")
  type ProductId = ProductId.Type

  object ProductQuantity extends PosDouble("product_uom_qty")
  type ProductQuantity = ProductQuantity.Type

  final case class MoveTemplate(name: MoveName,
                                productId: ProductId,
                                productQuantity: ProductQuantity)
}