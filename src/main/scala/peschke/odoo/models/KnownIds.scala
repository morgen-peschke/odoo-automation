package peschke.odoo.models

import cats.syntax.all._
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import peschke.odoo.models.RpcServiceCall.ObjectService.ModelName
import peschke.odoo.models.Template.Location
import peschke.odoo.models.Template.PartnerId
import peschke.odoo.models.Template.PickingTypeId
import peschke.odoo.models.Template.Product
import peschke.odoo.utils.Circe._

final case class KnownIds
  (locations: Map[String, Location.Id],
   products: Map[String, Product.Id],
   pickingTypeIds: Map[String, PickingTypeId],
   partners: Map[String, PartnerId]
  )
object KnownIds {
  def empty: KnownIds = KnownIds(Map.empty, Map.empty, Map.empty, Map.empty)

  val locations: ModelName = ModelName("stock.location")
  val products: ModelName = ModelName("product.template")
  val pickingTypeIds: ModelName = ModelName("stock.picking.type")
  val partners: ModelName = ModelName("res.partner")

  implicit val decoder: Decoder[KnownIds] = accumulatingDecoder { c =>
    (
      c.downField(ModelName.raw(locations)).asAcc[Map[String, Location.Id]],
      c.downField(ModelName.raw(products)).asAcc[Map[String, Product.Id]],
      c.downField(ModelName.raw(pickingTypeIds)).asAcc[Map[String, PickingTypeId]],
      c.downField(ModelName.raw(partners)).asAcc[Map[String, PartnerId]]
    ).mapN(KnownIds.apply)
  }
  implicit val encoder: Encoder[KnownIds] = Encoder.instance { ki =>
    Json.obj(
      ModelName.raw(locations)      := ki.locations,
      ModelName.raw(products)       := ki.products,
      ModelName.raw(pickingTypeIds) := ki.pickingTypeIds,
      ModelName.raw(partners)       := ki.partners
    )
  }
}
