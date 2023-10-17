package peschke.odoo.algebras

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all._
import io.circe.{Decoder, DecodingFailure}
import peschke.odoo.JsonLoader
import peschke.odoo.models.Template.{Entry, LocationDestId, LocationId, MoveName, MoveTemplate, MoveTemplateSet, MoveType, PartnerId, PersonName, PickingNameTemplate, PickingTemplate, PickingTypeId, ProductId, ProductQuantity, QuantityType, TimeOfDay}
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

    private implicit val quantityTypeDecoder: Decoder[QuantityType] = anyOf[QuantityType](
      fixed[String]("all").as(QuantityType.All),
      Decoder[ProductQuantity].map(QuantityType.Add),
      Decoder[ProductQuantity].at("refill-to").map(QuantityType.Fill)
    )

    private def mkMoveTemplateDecoder(knownIds: KnownIds): Decoder[MoveTemplate] = {
      val productIdDecoder = wrap[ProductId](knownIds.products.get)
      accumulatingDecoder { c =>
        (
          c.downField("name").asAcc[MoveName].findValid {
            c.downField("product_id").asAcc[MoveName]
          },
          c.downField("product_id").asAcc(productIdDecoder),
          c.downField("product_uom_qty").asAcc[QuantityType]
        ).mapN(MoveTemplate)
      }
    }

    private def mkMoveTemplateSetDecoder(implicit mtd: Decoder[MoveTemplate]): Decoder[MoveTemplateSet] = {
      anyOf[MoveTemplateSet](
        Decoder[NonEmptyList[MoveTemplate]].map(MoveTemplateSet.Explicit),
        fixed[String]("all").as(MoveTemplateSet.All)
      )
    }

    private def mkPickingDecoder(knownIds: KnownIds)(implicit moveDecoder: Decoder[MoveTemplateSet]): Decoder[PickingTemplate] = {
      val pickingTypeIdDecoder = wrap[PickingTypeId](knownIds.pickingTypeIds.get)
      val locationIdDecoder = wrap[LocationId](knownIds.locations.get)
      val locationDestIdDecoder = wrap[LocationDestId](knownIds.locations.get(_).map { id =>
        LocationDestId(LocationId.raw(id))
      })
      val partnerIdDecoder = wrap[PartnerId](knownIds.partners.get)
      accumulatingDecoder { c =>
        def fieldOrDefault[A: Decoder](name: String): Decoder.AccumulatingResult[A] =
          c.downField(name).asAcc[A].findValid {
            c.up.up.downField("common").downField(name).asAcc[A]
          }

        (
          fieldOrDefault[Frequency]("frequency"),
          fieldOrDefault[TimeOfDay]("timeOfDay"),
          fieldOrDefault[PickingNameTemplate]("pickingName"),
          fieldOrDefault[MoveType]("move_type"),
          fieldOrDefault("picking_type_id")(pickingTypeIdDecoder),
          fieldOrDefault("location_id")(locationIdDecoder),
          fieldOrDefault("location_dest_id")(locationDestIdDecoder),
          fieldOrDefault("partner_id")(partnerIdDecoder),
          fieldOrDefault[Boolean]("disabled"),
          fieldOrDefault[MoveTemplateSet]("moves")
        ).mapN(PickingTemplate.apply)
      }
    }

    private def mkEntryDecoder(implicit pickingDecoder: Decoder[PickingTemplate]): Decoder[Entry] =
      accumulatingDecoder { c =>
        (
          c.downField("label").asAcc[PersonName],
          c.downField("pickings").asAcc[NonEmptyList[PickingTemplate]]
        ).mapN(Entry)
      }

    override def decode(templateSource: JsonLoader.Source, knownIdsOpt: Option[JsonLoader.Source]): F[Template] =
      knownIdsOpt.fold(KnownIds.empty.pure[F])(loadKnownMappings).flatMap { knownIds =>
        implicit val moveTemplateDecoder: Decoder[MoveTemplate] = mkMoveTemplateDecoder(knownIds)
        implicit val moveTemplateSetDecoder: Decoder[MoveTemplateSet] = mkMoveTemplateSetDecoder
        implicit val pickingDecoder: Decoder[PickingTemplate] = mkPickingDecoder(knownIds)
        implicit val entryDecoder: Decoder[Entry] = mkEntryDecoder

        JsonLoader[F].load[NonEmptyList[Entry]](templateSource).map(Template(_))
      }
  }
}
