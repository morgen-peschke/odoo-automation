package peschke.odoo.algebras

import cats.Monad
import cats.MonadThrow
import cats.Order
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.effect.kernel.Clock
import cats.syntax.all._
import io.circe.Decoder
import org.typelevel.log4cats.LoggerFactory
import peschke.odoo.algebras.PickingNameGenerator.EntryIndex
import peschke.odoo.algebras.PickingNameGenerator.PickingIndex
import peschke.odoo.algebras.TemplateChecker.QuantityOnHand.CurrentQuantity
import peschke.odoo.algebras.TemplateChecker.SkippableChecks
import peschke.odoo.algebras.TemplateChecker.SkippableChecks.DisabledFlag
import peschke.odoo.models.Action.Search.Condition.syntax._
import peschke.odoo.models.RpcServiceCall.ObjectService.FieldName
import peschke.odoo.models.RpcServiceCall.ObjectService.ModelName
import peschke.odoo.models.Template.TimeOfDay.ScheduleAt
import peschke.odoo.models.Template.TimeOfDay.ScheduleAtOverrides
import peschke.odoo.models.Template._
import peschke.odoo.models._
import peschke.odoo.utils.Circe._

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.ZoneId
import java.time.ZonedDateTime
import java.time.{DayOfWeek => _}

trait TemplateChecker[F[_]] {
  def check
    (template: Template,
     timesOpt: Option[NonEmptySet[TimeOfDay]],
     dateOverridesOpt: Option[NonEmptySet[DateOverride]],
     scheduleAtOverrides: ScheduleAtOverrides,
     filters: TemplateFilters,
     checksToSkip: Set[SkippableChecks]
    )
    : F[Option[NonEmptyList[CheckedTemplate]]]
}
object TemplateChecker      {
  def apply[F[_]](implicit TC: TemplateChecker[F]): TC.type = TC

  def default[
      F[_]: ServiceCallBuilder: JsonRpc: MonadThrow: LoggerFactory: PickingNameGenerator: DateOverrideResolver: Clock: TemplateFilterer
  ](zoneId: ZoneId): TemplateChecker[F] =
    new TemplateChecker[F] {
      private val logger = LoggerFactory[F].getLoggerFromClass(classOf[TemplateChecker[F]])

      override def check
        (template:            Template,
         timesOpt:            Option[NonEmptySet[TimeOfDay]],
         dateOverridesOpt:    Option[NonEmptySet[DateOverride]],
         scheduleAtOverrides: ScheduleAtOverrides,
         filters:             TemplateFilters,
         checksToSkip:        Set[SkippableChecks]
        )
        : F[Option[NonEmptyList[CheckedTemplate]]] =
        for {
          dates          <- DateOverrideResolver[F].resolveAll(dateOverridesOpt)
          checkTimestamp <- Clock[F].realTimeInstant.map(ZonedDateTime.ofInstant(_, zoneId))
          time = timesOpt.getOrElse(TimeOfDay.All)
          scheduleAt = scheduleAtOverrides.asScheduleAt
          checked        <- dates.traverse(checkTemplate(template, _, time, scheduleAt, checkTimestamp, filters, checksToSkip))
        } yield checked.sequence

      private def checkTemplate
        (template:       Template,
         today:          LocalDate,
         times:          NonEmptySet[TimeOfDay],
         scheduleAt:     ScheduleAt,
         checkTimestamp: ZonedDateTime,
         filters:        TemplateFilters,
         checksToSkip:   Set[SkippableChecks]
        )
        : F[Option[CheckedTemplate]] = {
        val maxWidth = template.entries.toList.indices.last.toString.length
        template
          .entries.toList
          .traverseWithIndexM { (entry, index) =>
            checkEntry(
              entry,
              today,
              times,
              EntryIndex.fromInt(index, maxWidth),
              scheduleAt,
              checkTimestamp,
              filters,
              checksToSkip
            )
          }
          .map(_.flatten)
          .map(NonEmptyList.fromList(_).map(CheckedTemplate(_)))
      }

      private def checkEntry
        (entry:          Template.Entry,
         today:          LocalDate,
         times:          NonEmptySet[TimeOfDay],
         entryIndex:     EntryIndex,
         scheduleAt:     ScheduleAt,
         checkTimestamp: ZonedDateTime,
         filters:        TemplateFilters,
         checksToSkip:   Set[SkippableChecks]
        )
        : F[Option[CheckedTemplate.Entry]] =
        logger.info(show"Checking entries for ${entry.label}") >>
          Monad[F].ifM(TemplateFilterer[F].keepEntry(entry, filters))(
            ifFalse = none[CheckedTemplate.Entry].pure[F],
            ifTrue = {
              val maxWidth = entry.pickings.toList.indices.last.toString.length
              entry
                .pickings
                .toList
                .traverseWithIndexM { (template, pickingIndex) =>
                  checkPicking(
                    template,
                    today,
                    times,
                    entryIndex -> PickingIndex.fromInt(pickingIndex, maxWidth),
                    scheduleAt,
                    checkTimestamp,
                    filters,
                    checksToSkip
                  )
                }
                .map(_.flatten)
                .flatMap(NonEmptyList.fromList(_) match {
                  case None           => logger.info(s"Skipping ${entry.label}, no pickings need to be created").as(none)
                  case Some(pickings) => CheckedTemplate.Entry(entry.label, pickings).some.pure[F]
                })
            }
          )

      private def checkPicking
        (picking:        Template.PickingTemplate,
         today:          LocalDate,
         times:          NonEmptySet[Template.TimeOfDay],
         index:          (EntryIndex, PickingIndex),
         scheduleAt:     ScheduleAt,
         checkTimestamp: ZonedDateTime,
         filters:        TemplateFilters,
         checksToSkip:   Set[SkippableChecks]
        )
        : F[Option[CheckedTemplate.PickingTemplate]] =
        PickingNameGenerator[F]
          .generate(picking, today, index, checkTimestamp)
          .flatMap { name =>
            if (picking.disabled && !checksToSkip.contains(DisabledFlag))
              logger.info(show"Skipping $name because it is disabled").as(none)
            else
              TemplateFilterer[F].keepPicking(picking, name, filters).flatMap { passedFilterChecks =>
                val shouldSkipBecauseOfDate = picking.dayOfWeek =!= DayOfWeek.ofDay(today)
                val shouldSkipBecauseOfTime = !times.contains(picking.timeOfDay)
                if (!passedFilterChecks) none[CheckedTemplate.PickingTemplate].pure[F]
                else if (shouldSkipBecauseOfDate)
                  logger.debug(show"Skipping $name because of the day of the week").as(none)
                else if (shouldSkipBecauseOfTime) logger.debug(show"Skipping $name because of the time of day").as(none)
                else
                  checkMoveSet(picking, name, filters, checksToSkip)
                    .flatMap(NonEmptyList.fromList(_) match {
                      case None        => logger.info(show"Skipping $name because no moves need to be created").as(none)
                      case Some(moves) =>
                        CheckedTemplate
                          .PickingTemplate(
                            name,
                            ScheduledDate(
                              LocalDateTime.of(
                                today,
                                picking.timeOfDay match {
                                  case TimeOfDay.Morning => TimeOfDay.MorningTime.raw(scheduleAt.am)
                                  case TimeOfDay.Noon    => LocalTime.NOON
                                  case TimeOfDay.Night   => TimeOfDay.NightTime.raw(scheduleAt.pm)
                                  case TimeOfDay.AnyTime => checkTimestamp.toLocalTime
                                }
                              )
                            ),
                            picking.moveType,
                            picking.pickingTypeId,
                            picking.locationId,
                            picking.locationDestId,
                            picking.partnerId,
                            moves
                          ).some.pure[F]
                    })
              }
          }

      private def checkMoveSet
        (picking:      Template.PickingTemplate,
         pickingName:  CheckedTemplate.PickingName,
         filters:      TemplateFilters,
         checksToSkip: Set[SkippableChecks]
        )
        : F[List[CheckedTemplate.MoveTemplate]] =
        picking.moves match {
          case MoveTemplateSet.Explicit(moves) =>
            moves.toList.flatTraverse(checkMove(picking, pickingName, _, filters, checksToSkip).map(_.toList))
          case MoveTemplateSet.All             =>
            queryLocationContents(picking.locationId.id).flatMap(_.flatTraverse { case product -> quantity =>
              val move = MoveTemplate(
                MoveName(product.nameOpt.getOrElse("???")),
                product,
                QuantityType.Add(quantity)
              )
              TemplateFilterer[F].keepMove(move, pickingName, filters).map {
                case false => List.empty
                case true  => CheckedTemplate.MoveTemplate(move.name, product, quantity) :: Nil
              }
            })
        }

      private def checkMove
        (picking:      Template.PickingTemplate,
         pickingName:  CheckedTemplate.PickingName,
         move:         Template.MoveTemplate,
         filters:      TemplateFilters,
         checksToSkip: Set[SkippableChecks]
        )
        : F[Option[CheckedTemplate.MoveTemplate]] =
        Monad[F].ifM(TemplateFilterer[F].keepMove(move, pickingName, filters))(
          ifFalse = none[CheckedTemplate.MoveTemplate].pure[F],
          ifTrue = {
            def runCheck: F[Option[CheckedTemplate.MoveTemplate]] = calculateQuantity(
              move.productId.id,
              picking.locationId.id,
              picking.locationDestId.id,
              move.quantityType
            ).flatMap {
              case None                  =>
                logger.info(show"Skipping ${move.name}: current stock is at or over max quantity").as(none)
              case Some(productQuantity) =>
                CheckedTemplate.MoveTemplate(move.name, move.productId, productQuantity).some.pure[F]
            }

            if (checksToSkip.contains(SkippableChecks.Quantity))
              move.quantityType match {
                case QuantityType.Add(quantity)     =>
                  CheckedTemplate.MoveTemplate(move.name, move.productId, quantity).some.pure[F]
                case QuantityType.Fill(maxQuantity) =>
                  CheckedTemplate.MoveTemplate(move.name, move.productId, maxQuantity).some.pure[F]
                case QuantityType.All               => runCheck

              }
            else runCheck
          }
        )

      private def queryCurrentQuantity
        (productId: Product.Id, locationId: Either[Location.Id, LocationDest.Id])
        : F[CurrentQuantity] =
        ServiceCallBuilder[F]
          .fromAction(
            Action.Search(
              QuantityOnHand.Model,
              QuantityOnHand.ProductId :: QuantityOnHand.LocationId :: QuantityOnHand.Quantity :: Nil,
              locationId.fold(QuantityOnHand.LocationId.in(_), QuantityOnHand.LocationId.in(_)) ::
                QuantityOnHand.ProductId.in(productId) :: Nil,
              none
            )
          )
          .flatMap(JsonRpc[F].call)
          .flatMap { response =>
            response
              .result.as[List[QuantityOnHand]].fold(
                UnexpectedResponse
                  .liftTo[F, List[QuantityOnHand]](show"quantity check for $productId @ $locationId", response, _),
                _.pure[F]
              )
          }
          .map(_.map(_.quantity).foldLeft(CurrentQuantity.Zero)(_ plus _))

      private def queryLocationContents(locationId: Location.Id): F[List[(Product, Template.ProductQuantity)]] =
        ServiceCallBuilder[F]
          .fromAction(
            Action.Search(
              QuantityOnHand.Model,
              QuantityOnHand.ProductId :: QuantityOnHand.LocationId :: QuantityOnHand.Quantity :: Nil,
              QuantityOnHand.LocationId.in(locationId) :: Nil,
              none
            )
          )
          .flatMap(JsonRpc[F].call)
          .flatMap { response =>
            response
              .result.as[List[QuantityOnHand]].fold(
                UnexpectedResponse.liftTo[F, List[QuantityOnHand]](show"all quantities check @ $locationId", response, _),
                _.pure[F]
              )
          }
          .map(_.groupMapReduce(_.product)(_.quantity)(_ plus _).toList.flatMap { case productId -> quantityOnHand =>
            ProductQuantity.fromDouble(CurrentQuantity.raw(quantityOnHand)).toOption.map(productId -> _)
          })

      private def calculateQuantity
        (productId: Product.Id, locationSrcId: Location.Id, locationDestId: LocationDest.Id, quantityType: QuantityType)
        : F[Option[ProductQuantity]] =
        quantityType match {
          case QuantityType.Add(quantity)     => quantity.some.pure[F]
          case QuantityType.Fill(maxQuantity) =>
            queryCurrentQuantity(productId, locationDestId.asRight)
              .map { current =>
                ProductQuantity.fromDouble(ProductQuantity.raw(maxQuantity) - CurrentQuantity.raw(current)).toOption
              }
          case QuantityType.All               =>
            queryCurrentQuantity(productId, locationSrcId.asLeft)
              .map { current =>
                ProductQuantity.fromDouble(CurrentQuantity.raw(current)).toOption
              }
        }
    }

  sealed trait SkippableChecks extends enumeratum.EnumEntry with enumeratum.EnumEntry.Hyphencase
  object SkippableChecks       extends enumeratum.Enum[SkippableChecks] {
    case object DisabledFlag extends SkippableChecks
    case object Quantity     extends SkippableChecks

    override val values: IndexedSeq[SkippableChecks] = findValues
  }

  final case class QuantityOnHand(product: Product, locationId: Location.Id, quantity: CurrentQuantity)
  object QuantityOnHand {
    val Model: ModelName = ModelName("stock.quant")

    val ProductId: FieldName = FieldName("product_id")
    val LocationId: FieldName = FieldName("location_id")
    val Quantity: FieldName = FieldName("quantity")

    object CurrentQuantity extends NonNegativeDouble(Quantity.show) {
      val Zero: Type = CurrentQuantity(0.0d)

      implicit final class Ops(private val cq: Type) extends AnyVal {
        def plus(other: Type): Type = apply(raw(cq) + raw(other))
      }
    }
    type CurrentQuantity = CurrentQuantity.Type

    implicit val decoder: Decoder[QuantityOnHand] = accumulatingDecoder { c =>
      (
        (
          c.downField("product_id").downArray.asAcc[Product.Id],
          c.downField("product_id").downN(1).asAcc[String].map(_.some)
        ).mapN(Product.fromId),
        c.downField("location_id").downArray.asAcc[Location.Id],
        c.downField("quantity").asAcc[CurrentQuantity]
      ).mapN(QuantityOnHand.apply)
    }
  }

  object ProductName extends NonEmptyString("Product name")
  type ProductName = ProductName.Type

  final case class ProductInfo(id: Product.Id, name: ProductName)
  object ProductInfo {
    implicit val order: Order[ProductInfo] = Order.by(p => Product.Id.raw(p.id))
  }
}
