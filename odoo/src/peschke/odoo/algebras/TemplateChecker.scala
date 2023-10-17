package peschke.odoo.algebras

import cats.{MonadThrow, Order}
import cats.data.{NonEmptyList, NonEmptySet}
import cats.effect.kernel.Clock
import cats.syntax.all._
import io.circe.Decoder
import org.typelevel.log4cats.LoggerFactory
import peschke.odoo.algebras.PickingNameGenerator.{EntryIndex, PickingIndex}
import peschke.odoo.algebras.TemplateChecker.QuantityOnHand.CurrentQuantity
import peschke.odoo.models.Action.Search.Condition.syntax._
import peschke.odoo.models.RpcServiceCall.ObjectService.{FieldName, ModelName}
import peschke.odoo.models.Template.TimeOfDay.{MorningTime, NightTime}
import peschke.odoo.models.Template._
import peschke.odoo.models.{Template, _}
import peschke.odoo.utils.Circe._

import java.time.{LocalDate, LocalDateTime, LocalTime, ZoneId, ZonedDateTime}

trait TemplateChecker[F[_]]{
  def check(template: Template,
            timesOpt: Option[NonEmptySet[TimeOfDay]],
            dateOverrideOpt: Option[LocalDate],
            morningTimeOpt: Option[MorningTime],
            nightTimeOpt: Option[NightTime]): F[Option[CheckedTemplate]]
}
object TemplateChecker {
  def apply[F[_]](implicit TC: TemplateChecker[F]): TC.type = TC

  def default[
    F[_] : ServiceCallBuilder : JsonRpc : MonadThrow : LoggerFactory : Clock: PickingNameGenerator
  ](zoneId: ZoneId): TemplateChecker[F] =
    new TemplateChecker[F] {
      private val logger = LoggerFactory[F].getLogger

      override def check(template: Template,
                         timesOpt: Option[NonEmptySet[TimeOfDay]],
                         dateOverrideOpt: Option[LocalDate],
                         morningTimeOpt: Option[MorningTime],
                         nightTimeOpt: Option[NightTime]): F[Option[CheckedTemplate]] =
        dateOverrideOpt.map(_.pure[F]).getOrElse {
          Clock[F].realTimeInstant.map(ZonedDateTime.ofInstant(_, zoneId).toLocalDate)
        }.flatMap(checkTemplate(
          template,
          _,
          timesOpt.getOrElse(TimeOfDay.All),
          morningTimeOpt.getOrElse(MorningTime.Default),
          nightTimeOpt.getOrElse(NightTime.Default)
        ))

      private def checkTemplate(template: Template,
                                today: LocalDate,
                                times: NonEmptySet[TimeOfDay],
                                morningTime: MorningTime,
                                nightTime: NightTime): F[Option[CheckedTemplate]] =
        template.entries.toList
          .traverseWithIndexM { (entry, index) =>
            checkEntry(entry, today, times, EntryIndex(index), morningTime, nightTime)
          }
          .map(_.flatten)
          .map(NonEmptyList.fromList(_).map(CheckedTemplate(_)))

      private def checkEntry(entry: Template.Entry,
                             today: LocalDate,
                             times: NonEmptySet[TimeOfDay],
                             entryIndex: EntryIndex,
                             morningTime: MorningTime,
                             nightTime: NightTime): F[Option[CheckedTemplate.Entry]] =
        logger.debug(show"Checking entries for ${entry.label}") >>
          entry.pickings.toList
            .traverseWithIndexM { (template, pickingIndex) =>
              checkPicking(template, today, times, entryIndex -> PickingIndex(pickingIndex), morningTime, nightTime)
            }
            .map(_.flatten)
            .flatMap(NonEmptyList.fromList(_) match {
              case None => logger.debug(s"Skipping ${entry.label}, no pickings need to be created").as(none)
              case Some(pickings) => CheckedTemplate.Entry(entry.label, pickings).some.pure[F]
            })

      private def checkPicking(picking: Template.PickingTemplate,
                               today: LocalDate,
                               times: NonEmptySet[Template.TimeOfDay],
                               index: (EntryIndex, PickingIndex),
                               morningTime: MorningTime,
                               nightTime: NightTime): F[Option[CheckedTemplate.PickingTemplate]] =
        PickingNameGenerator[F].generate(picking, today, index)
          .flatMap { name =>
            val shouldSkipBecauseOfDate = picking.frequency match {
              case Frequency.Daily => false
              case Frequency.Weekly(days) => !days.contains(DayOfWeek.ofDay(today))
            }

            val shouldSkipBecauseOfTime = !times.contains(picking.timeOfDay)

            if (picking.disabled) logger.debug(show"Skipping $name because it is disabled").as(none)
            else if (shouldSkipBecauseOfDate) logger.debug(show"Skipping $name because of the day of the week").as(none)
            else if (shouldSkipBecauseOfTime) logger.debug(show"Skipping $name because of the time of day").as(none)
            else
              checkMoveSet(picking)
                .flatMap(NonEmptyList.fromList(_) match {
                  case None => logger.debug(show"Skipping $name because no moves need to be created").as(none)
                  case Some(moves) =>
                    CheckedTemplate.PickingTemplate(
                      name,
                      picking.frequency,
                      ScheduledDate(LocalDateTime.of(today, picking.timeOfDay match {
                        case TimeOfDay.Morning => TimeOfDay.MorningTime.raw(morningTime)
                        case TimeOfDay.Noon => LocalTime.NOON
                        case TimeOfDay.Night => TimeOfDay.NightTime.raw(nightTime)
                      })),
                      picking.moveType,
                      picking.pickingTypeId,
                      picking.locationId,
                      picking.locationDestId,
                      picking.partnerId,
                      moves
                    ).some.pure[F]
                })
          }

      private def checkMoveSet(picking: Template.PickingTemplate): F[List[CheckedTemplate.MoveTemplate]] =
        picking.moves match {
          case MoveTemplateSet.Explicit(moves) => moves.toList.flatTraverse(checkMove(picking, _).map(_.toList))
          case MoveTemplateSet.All =>
            queryLocationContents(picking.locationId).map(_.map {
              case product -> quantity =>
                CheckedTemplate.MoveTemplate(MoveName(ProductName.raw(product.name)), product.id, quantity)
            })
        }

      private def checkMove(picking: Template.PickingTemplate,
                            move: Template.MoveTemplate): F[Option[CheckedTemplate.MoveTemplate]] =
        calculateQuantity(
          move.productId,
          picking.locationId,
          picking.locationDestId,
          move.quantityType
        ).flatMap {
            case None => logger.debug(show"Skipping ${move.name}: current stock is at or over max quantity").as(none)
            case Some(productQuantity) =>
              CheckedTemplate.MoveTemplate(move.name, move.productId, productQuantity).some.pure[F]
          }

      private def queryCurrentQuantity(productId: ProductId, locationId: Either[LocationId, LocationDestId]): F[CurrentQuantity] =
        ServiceCallBuilder[F]
          .fromAction(Action.Search(
            QuantityOnHand.Model,
            QuantityOnHand.ProductId :: QuantityOnHand.LocationId :: QuantityOnHand.Quantity :: Nil,
            locationId.fold(QuantityOnHand.LocationId.in(_), QuantityOnHand.LocationId.in(_)) ::
              QuantityOnHand.ProductId.in(productId) :: Nil,
            none
          ))
          .flatMap(JsonRpc[F].call)
          .flatMap { response =>
            response.result.as[List[QuantityOnHand]].fold(
              UnexpectedResponse.liftTo[F, List[QuantityOnHand]](show"quantity check for $productId @ $locationId", response, _),
              _.pure[F]
            )
          }
          .map(_.map(_.quantity).foldLeft(CurrentQuantity.Zero)(_ plus _))

      private def queryLocationContents(locationId: LocationId): F[List[(ProductInfo, Template.ProductQuantity)]] =
        ServiceCallBuilder[F]
          .fromAction(Action.Search(
            QuantityOnHand.Model,
            QuantityOnHand.ProductId :: QuantityOnHand.LocationId :: QuantityOnHand.Quantity :: Nil,
            QuantityOnHand.LocationId.in(locationId) :: Nil,
            none
          ))
          .flatMap(JsonRpc[F].call)
          .flatMap { response =>
            response.result.as[List[QuantityOnHand]].fold(
              UnexpectedResponse.liftTo[F, List[QuantityOnHand]](show"all quantities check @ $locationId", response, _),
              _.pure[F]
            )
          }
          .map(_.groupMapReduce(_.product)(_.quantity)(_ plus _).toList.flatMap {
            case productId -> quantityOnHand =>
              ProductQuantity.fromDouble(CurrentQuantity.raw(quantityOnHand)).toOption.map(productId -> _)
          })

      private def calculateQuantity(productId: ProductId,
                                    locationSrcId: LocationId,
                                    locationDestId: LocationDestId,
                                    quantityType: QuantityType): F[Option[ProductQuantity]] =
        quantityType match {
          case QuantityType.Add(quantity) => quantity.some.pure[F]
          case QuantityType.Fill(maxQuantity) =>
            queryCurrentQuantity(productId, locationDestId.asRight)
              .map { current =>
                ProductQuantity.fromDouble(ProductQuantity.raw(maxQuantity) - CurrentQuantity.raw(current)).toOption
              }
          case QuantityType.All =>
            queryCurrentQuantity(productId, locationSrcId.asLeft)
              .map { current =>
                ProductQuantity.fromDouble(CurrentQuantity.raw(current)).toOption
              }
        }
    }

  final case class QuantityOnHand(product: ProductInfo, locationId: LocationId, quantity: CurrentQuantity)
  object QuantityOnHand {
    val Model: ModelName = ModelName("stock.quant")

    val ProductId: FieldName = FieldName("product_id")
    val LocationId: FieldName = FieldName("location_id")
    val Quantity: FieldName = FieldName("quantity")

    object CurrentQuantity extends NonNegativeDouble(Quantity.show) {
      val Zero: Type = CurrentQuantity(0.0d)

      implicit final class Ops(private val cq: Type) extends AnyVal {
        def plus (other: Type): Type = apply(raw(cq) + raw(other))
      }
    }
    type CurrentQuantity = CurrentQuantity.Type

    implicit val decoder: Decoder[QuantityOnHand] = accumulatingDecoder { c =>
      (
        (
          c.downField("product_id").downArray.asAcc[ProductId],
          c.downField("product_id").downN(1).asAcc[ProductName]
        ).mapN(ProductInfo.apply),
        c.downField("location_id").downArray.asAcc[LocationId],
        c.downField("quantity").asAcc[CurrentQuantity]
      ).mapN(QuantityOnHand.apply)
    }
  }

  object ProductName extends NonEmptyString("Product name")
  type ProductName = ProductName.Type

  final case class ProductInfo(id: ProductId, name: ProductName)
  object ProductInfo {
    implicit val order: Order[ProductInfo] = Order.by(p => ProductId.raw(p.id))
  }
}
