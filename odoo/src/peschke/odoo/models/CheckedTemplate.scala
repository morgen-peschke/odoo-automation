package peschke.odoo.models

import cats.data.NonEmptyList
import peschke.odoo.models.CheckedTemplate.Entry
import peschke.odoo.models.Template._

final case class CheckedTemplate(entries: NonEmptyList[Entry])
object CheckedTemplate {
  final case class Entry(label: PersonName, pickings: NonEmptyList[PickingTemplate])

  object PickingName extends NonEmptyString("Picking name")
  type PickingName = PickingName.Type

  final case class PickingTemplate(name: PickingName,
                                   frequency: Frequency,
                                   scheduledDate: ScheduledDate,
                                   moveType: MoveType,
                                   pickingTypeId: PickingTypeId,
                                   locationId: LocationId,
                                   locationDestId: LocationDestId,
                                   partnerId: PartnerId,
                                   moves: NonEmptyList[MoveTemplate])

  final case class MoveTemplate(name: MoveName,
                                productId: ProductId,
                                quantity: ProductQuantity)

}