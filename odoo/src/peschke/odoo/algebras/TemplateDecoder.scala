package peschke.odoo.algebras

import cats.MonadThrow
import cats.data.NonEmptyList
import cats.syntax.all._
import io.circe.ACursor
import io.circe.Decoder
import io.circe.DecodingFailure
import peschke.odoo.JsonLoader
import peschke.odoo.models.Template._
import peschke.odoo.models._
import peschke.odoo.utils.Circe._

trait TemplateDecoder[F[_]] {
  def decode(templateSource: JsonLoader.Source, knownMappingsOpt: Option[JsonLoader.Source]): F[Template]
}
object TemplateDecoder      {
  def apply[F[_]](implicit TD: TemplateDecoder[F]): TD.type = TD

  def default[F[_]: JsonLoader: MonadThrow: LocationNameExpander]: TemplateDecoder[F] = new TemplateDecoder[F] {

    private def loadKnownMappings(source: JsonLoader.Source): F[KnownIds] = JsonLoader[F].load[KnownIds](source)

    private def wrapLocationNewType
      (dayOfWeek: DayOfWeek, timeOfDay: TimeOfDay)
      (newType:   MaybeNamedIdNewType)
      (lookup:    String => Option[newType.Wrapper.Id])
      : Decoder[F[newType.Wrapper]] =
      anyOf[F[newType.Wrapper]](
        Decoder[Int].emap(newType.Wrapper.fromInt(_)).map(_.pure[F]),
        Decoder[String].map { template =>
          LocationNameExpander[F]
            .generate(template, dayOfWeek, timeOfDay)
            .flatMap { name =>
              lookup(name)
                .liftTo[F](new NoSuchElementException(s"Unable to resolve <$name> to a known id"))
                .map(newType.Wrapper.fromId(_, name.some))
            }
        }
      )

    private def wrapNewType
      (newType: MaybeNamedIdNewType)
      (lookup:  String => Option[newType.Wrapper.Id])
      : Decoder[newType.Wrapper] =
      anyOf[newType.Wrapper](
        Decoder[Int].emap(newType.Wrapper.fromInt(_)),
        Decoder[String].emap { str =>
          lookup(str)
            .toRight(s"Unable to resolve <$str> to a known id")
            .map(newType.Wrapper.fromId(_, str.some))
        }
      )

    private def wrap[A: Decoder](lookup: String => Option[A]): Decoder[A] = anyOf[A](
      Decoder[A],
      Decoder.instance[A] { c =>
        c.as[String].flatMap { str =>
          lookup(str).toRight(DecodingFailure(s"Unable to resolve <$str> to a known id", c.history))
        }
      }
    )

    private implicit val quantityTypeDecoder: Decoder[QuantityType] = anyOf[QuantityType](
      exactly[String]("all").as(QuantityType.All),
      Decoder[ProductQuantity].map(QuantityType.Add),
      Decoder[ProductQuantity].at("refill-to").map(QuantityType.Fill)
    )

    private def mkMoveTemplateDecoder(knownIds: KnownIds): Decoder[MoveTemplate] = {
      val productIdDecoder = wrapNewType(ProductIdNewType)(knownIds.products.get)
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
        exactly[String]("all").as(MoveTemplateSet.All)
      )
    }

    private def mkPickingDecoder
      (knownIds:             KnownIds)
      (implicit moveDecoder: Decoder[MoveTemplateSet])
      : Decoder[F[NonEmptyList[PickingTemplate]]] = {
      val pickingTypeIdDecoder = wrap[PickingTypeId](knownIds.pickingTypeIds.get)
      val partnerIdDecoder = wrap[PartnerId](knownIds.partners.get)
      accumulatingDecoder { c =>
        def asIfDefined[A: Decoder](cursor: ACursor): Option[Decoder.AccumulatingResult[A]] =
          if (cursor.focus.isEmpty) none else cursor.asAcc[A].some

        def ifMissingOrFieldOrDefault[A: Decoder](name: String, default: A): Decoder.AccumulatingResult[A] =
          asIfDefined[A](c.downField(name))
            .orElse(asIfDefined[A](c.up.up.downField("common").downField(name)))
            .getOrElse(default.valid)

        def fieldOrDefault[A: Decoder](name: String): Decoder.AccumulatingResult[A] =
          asIfDefined[A](c.downField(name)).getOrElse(c.up.up.downField("common").downField(name).asAcc[A])

        (
          ifMissingOrFieldOrDefault[Frequency]("frequency", Frequency.Daily),
          ifMissingOrFieldOrDefault[TimeOfDay]("timeOfDay", TimeOfDay.AnyTime),
          fieldOrDefault[PickingNameTemplate]("pickingName"),
          fieldOrDefault[MoveType]("move_type"),
          fieldOrDefault("picking_type_id")(pickingTypeIdDecoder),
          fieldOrDefault("partner_id")(partnerIdDecoder),
          fieldOrDefault[Boolean]("disabled"),
          fieldOrDefault[MoveTemplateSet]("moves")
        ).tupled.andThen {
          case (frequency, timeOfDay, pickingName, moveType, pickingTypeId, partnerId, disabled, moves) =>
            val result = for {
              dow <- frequency.expanded
              tod <- timeOfDay.expanded
            } yield {
              val locationIdDecoder = wrapLocationNewType(dow, tod)(LocationNewType)(knownIds.locations.get)
              val locationDestIdDecoder =
                wrapLocationNewType(dow, tod)(LocationDestNewType)(knownIds.locations.get(_).map { id =>
                  LocationDest.Id(Location.Id.raw(id))
                })

              (
                fieldOrDefault("location_id")(locationIdDecoder),
                fieldOrDefault("location_dest_id")(locationDestIdDecoder)
              ).mapN { (locationIdF, locationDestIdF) =>
                (locationIdF, locationDestIdF).mapN { (locationId, locationDestId) =>
                  PickingTemplate(
                    dow,
                    frequency,
                    tod,
                    pickingName,
                    moveType,
                    pickingTypeId,
                    locationId,
                    locationDestId,
                    partnerId,
                    disabled,
                    moves
                  )
                }
              }
            }

            result.sequence.map(_.sequence)
        }
      }
    }

    private def mkEntryDecoder(implicit pickingDecoder: Decoder[F[NonEmptyList[PickingTemplate]]]): Decoder[F[Entry]] =
      accumulatingDecoder { c =>
        (
          c.downField("label").asAcc[EntryLabel],
          c.downField("tags").asAcc[List[Tag]],
          c.downField("pickings").asAcc[NonEmptyList[F[NonEmptyList[PickingTemplate]]]].map(_.flatSequence)
        ).mapN { (label, tags, pickingsF) =>
          pickingsF.map(Entry(label, tags, _))
        }
      }

    override def decode(templateSource: JsonLoader.Source, knownIdsOpt: Option[JsonLoader.Source]): F[Template] =
      knownIdsOpt.fold(KnownIds.empty.pure[F])(loadKnownMappings).flatMap { knownIds =>
        implicit val moveTemplateDecoder: Decoder[MoveTemplate] = mkMoveTemplateDecoder(knownIds)
        implicit val moveTemplateSetDecoder: Decoder[MoveTemplateSet] = mkMoveTemplateSetDecoder
        implicit val pickingDecoder: Decoder[F[NonEmptyList[PickingTemplate]]] = mkPickingDecoder(knownIds)
        implicit val entryDecoder: Decoder[F[Entry]] = mkEntryDecoder

        JsonLoader[F]
          .load[NonEmptyList[F[Entry]]](templateSource)
          .flatMap(_.sequence)
          .map(Template(_))
      }
  }
}
