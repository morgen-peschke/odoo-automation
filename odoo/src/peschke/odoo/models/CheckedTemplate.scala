package peschke.odoo.models

import cats.data.NonEmptyList
import peschke.odoo.models.CheckedTemplate.Entry
import peschke.odoo.models.Template._

final case class CheckedTemplate(entries: NonEmptyList[Entry])
object CheckedTemplate {
  final case class Entry(label: EntryLabel, pickings: NonEmptyList[PickingTemplate])

  object PickingName extends NonEmptyString("Picking name")
  type PickingName = PickingName.Type

  final case class PickingTemplate
    (name: PickingName,
     scheduledDate: ScheduledDate,
     moveType: MoveType,
     pickingTypeId: PickingTypeId,
     locationId: Location,
     locationDestId: LocationDest,
     partnerId: PartnerId,
     moves: NonEmptyList[MoveTemplate]
    )

  final case class MoveTemplate(name: MoveName, productId: Product, quantity: ProductQuantity)

}
