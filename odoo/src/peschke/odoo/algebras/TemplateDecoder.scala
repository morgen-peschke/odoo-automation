package peschke.odoo.algebras

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all._
import io.circe.{Decoder, DecodingFailure}
import peschke.odoo.JsonLoader
import peschke.odoo.models.Template.{Entry, LocationDestId, LocationId, MoveName, MoveTemplate, MoveType, PartnerId, PersonName, PickingNamePrefix, PickingTemplate, PickingTypeId, ProductId, ProductQuantity, TimeOfDay}
import peschke.odoo.models.{Frequency, KnownIds, Template}
import peschke.odoo.utils.Circe._

trait TemplateDecoder[F[_]]{
  def decode(templateSource: JsonLoader.Source, knownMappingsOpt: Option[JsonLoader.Source]): F[Template]
}
object TemplateDecoder {
  def apply[F[_]](implicit TD: TemplateDecoder[F]): TD.type = TD

  def default[F[_]: JsonLoader: Monad]: TemplateDecoder[F] = new TemplateDecoder[F] {

    private def loadKnownMappings(source: JsonLoader.Source): F[KnownIds] = JsonLoader[F].load[KnownIds](source)

    private def wrap[A: Decoder](lookup: String => Option[A]): Decoder[A] = anyOf[A](
      Decoder[A],
      Decoder.instance[A] { c =>
        c.as[String].flatMap { str =>
          lookup(str).toRight(DecodingFailure(s"Unable to resolve <$str> to a known id", c.history))
        }
      }
    )

    private def mkMoveDecoder(knownIds: KnownIds): Decoder[MoveTemplate] = {
      val productIdDecoder = wrap[ProductId](knownIds.products.get)
      accumulatingDecoder { c =>
        (
          c.downField("name").asAcc[MoveName].orElse {
            c.downField("product_id").asAcc[MoveName]
          },
          c.downField("product_id").asAcc(productIdDecoder),
          c.downField("product_uom_qty").asAcc[ProductQuantity]
        ).mapN(MoveTemplate)
      }
    }

    private def mkPickingDecoder(knownIds: KnownIds)(implicit moveDecoder: Decoder[MoveTemplate]): Decoder[PickingTemplate] = {
      val pickingTypeIdDecoder = wrap[PickingTypeId](knownIds.pickingTypeIds.get)
      val locationIdDecoder = wrap[LocationId](knownIds.locations.get)
      val locationDestIdDecoder = wrap[LocationDestId](knownIds.locations.get(_).map { id =>
        LocationDestId(LocationId.raw(id))
      })
      val partnerIdDecoder = wrap[PartnerId](knownIds.partners.get)
      accumulatingDecoder { c =>
        (
          c.downField("frequency").asAcc[Frequency],
          c.downField("timeOfDay").asAcc[TimeOfDay],
          c.downField("move_type").asAcc[MoveType].findValid {
            c.up.up.downField("move_type").asAcc[MoveType]
          },
          c.downField("picking_type_id").asAcc(pickingTypeIdDecoder).findValid {
            c.up.up.downField("picking_type_id").asAcc(pickingTypeIdDecoder)
          },
          c.downField("location_id").asAcc(locationIdDecoder).findValid {
            c.up.up.downField("location_id").asAcc(locationIdDecoder)
          },
          c.downField("location_dest_id").asAcc(locationDestIdDecoder).findValid {
            c.up.up.downField("location_dest_id").asAcc(locationDestIdDecoder)
          },
          c.downField("partner_id").asAcc(partnerIdDecoder).findValid {
            c.up.up.downField("partner_id").asAcc(partnerIdDecoder)
          },
          c.downField("moves").asAcc[NonEmptyList[MoveTemplate]]
        ).mapN(PickingTemplate.apply)
      }
    }

    private def mkEntryDecoder(implicit pickingDecoder: Decoder[PickingTemplate]): Decoder[Entry] =
      accumulatingDecoder { c =>
        (
          c.downField("person").asAcc[PersonName],
          c.downField("pickingNamePrefix").asAcc[PickingNamePrefix],
          c.downField("pickings").asAcc[NonEmptyList[PickingTemplate]]
        ).mapN(Entry)
      }

    override def decode(templateSource: JsonLoader.Source, knownIdsOpt: Option[JsonLoader.Source]): F[Template] =
      knownIdsOpt.fold(KnownIds.empty.pure[F])(loadKnownMappings).flatMap { knownIds =>
        implicit val moveDecoder: Decoder[MoveTemplate] = mkMoveDecoder(knownIds)
        implicit val pickingDecoder: Decoder[PickingTemplate] = mkPickingDecoder(knownIds)
        implicit val entryDecoder: Decoder[Entry] = mkEntryDecoder

        JsonLoader[F].load[NonEmptyList[Entry]](templateSource).map(Template(_))
      }
  }
}
