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
     labelFilter: LabelFilter,
     tagFilter: TagFilter
    )
    : F[Option[NonEmptyList[CheckedTemplate]]]
}
object TemplateChecker      {
  def apply[F[_]](implicit TC: TemplateChecker[F]): TC.type = TC

  def default[
      F[_]: ServiceCallBuilder: JsonRpc: MonadThrow: LoggerFactory: PickingNameGenerator: DateOverrideResolver: Clock
  ](zoneId: ZoneId): TemplateChecker[F] =
    new TemplateChecker[F] {
      private val logger = LoggerFactory[F].getLoggerFromClass(classOf[TemplateChecker[F]])
      override def check
        (template:            Template,
         timesOpt:            Option[NonEmptySet[TimeOfDay]],
         dateOverridesOpt:    Option[NonEmptySet[DateOverride]],
         scheduleAtOverrides: ScheduleAtOverrides,
         labelFilter:         LabelFilter,
         tagFilter:           TagFilter
        )
        : F[Option[NonEmptyList[CheckedTemplate]]] =
        for {
          dates          <- DateOverrideResolver[F].resolveAll(dateOverridesOpt)
          checkTimestamp <- Clock[F].realTimeInstant.map(ZonedDateTime.ofInstant(_, zoneId))
          time = timesOpt.getOrElse(TimeOfDay.All)
          scheduleAt = scheduleAtOverrides.asScheduleAt
          checked        <- dates.traverse(checkTemplate(template, _, time, scheduleAt, checkTimestamp, labelFilter, tagFilter))
        } yield checked.sequence

      private def checkTemplate
        (template:       Template,
         today:          LocalDate,
         times:          NonEmptySet[TimeOfDay],
         scheduleAt:     ScheduleAt,
         checkTimestamp: ZonedDateTime,
         labelFilter:    LabelFilter,
         tagFilter:      TagFilter
        )
        : F[Option[CheckedTemplate]] =
        template
          .entries.toList
          .traverseWithIndexM { (entry, index) =>
            checkEntry(entry, today, times, EntryIndex(index), scheduleAt, checkTimestamp, labelFilter, tagFilter)
          }
          .map(_.flatten)
          .map(NonEmptyList.fromList(_).map(CheckedTemplate(_)))

      private def checkEntry
        (entry:          Template.Entry,
         today:          LocalDate,
         times:          NonEmptySet[TimeOfDay],
         entryIndex:     EntryIndex,
         scheduleAt:     ScheduleAt,
         checkTimestamp: ZonedDateTime,
         labelFilter:    LabelFilter,
         tagFilter:      TagFilter
        )
        : F[Option[CheckedTemplate.Entry]] =
        logger.info(show"Checking entries for ${entry.label}") >>
          Monad[F].ifM((checkLabel(entry, labelFilter), checkTags(entry, tagFilter)).mapN(_ && _))(
            ifFalse = none[CheckedTemplate.Entry].pure[F],
            ifTrue = entry
              .pickings
              .toList
              .traverseWithIndexM { (template, pickingIndex) =>
                checkPicking(
                  template,
                  today,
                  times,
                  entryIndex -> PickingIndex(pickingIndex),
                  scheduleAt,
                  checkTimestamp
                )
              }
              .map(_.flatten)
              .flatMap(NonEmptyList.fromList(_) match {
                case None           => logger.info(s"Skipping ${entry.label}, no pickings need to be created").as(none)
                case Some(pickings) => CheckedTemplate.Entry(entry.label, pickings).some.pure[F]
              })
          )

      private def checkLabel(entry: Template.Entry, labelFilter: LabelFilter): F[Boolean] =
        labelFilter.textFilter match {
          case TextFilter.True => true.pure[F]
          case filter          =>
            filter
              .matches(entry.label.string)
              .traverse(f => logger.info(show"${entry.label} matched string $f"))
              .map(_.isDefined)
              .flatTap {
                case true  => logger.debug(s"${entry.label} passed label filters check")
                case false => logger.info(s"Skipping entry because of label filters: ${entry.label}")
              }
        }

      private def checkTags(entry: Template.Entry, tagFilter: TagFilter): F[Boolean] =
        tagFilter match {
          case TagFilter.Exists(TextFilter.truthy) => true.pure[F]
          case TagFilter.Exists(filter)            =>
            NonEmptyList.fromList {
              entry
                .tags
                .flatMap(t => filter.matches(t.string).tupleLeft(t))
            } match {
              case Some(matches) =>
                val tag = if (matches.length === 1) "tag" else "tags"
                logger
                  .info {
                    matches
                      .map { case t -> f =>
                        show"[$t] $f"
                      }
                      .mkString_(show"Keeping entry because $tag matched filter\n  ", "\n  ", "")
                  }.as(true)
              case None          =>
                logger.info(show"Skipping entry because no tag matched filter\n  $filter").as(false)
            }
          case TagFilter.ForAll(filter)            =>
            NonEmptyList.fromList {
              entry
                .tags
                .flatMap(t => filter.fails(t.string).tupleLeft(t))
            } match {
              case Some(matches) =>
                val tag = if (matches.length === 1) "tag" else "tags"
                logger
                  .info {
                    matches
                      .map { case t -> f =>
                        show"[$t] $f"
                      }
                      .mkString_(show"Skipping entry because $tag failed filter\n  ", "\n  ", "")
                  }.as(false)
              case None          =>
                logger.info(show"Keeping entry because no tag matched filter\n  $filter").as(true)
            }
        }

      private def checkPicking
        (picking:        Template.PickingTemplate,
         today:          LocalDate,
         times:          NonEmptySet[Template.TimeOfDay],
         index:          (EntryIndex, PickingIndex),
         scheduleAt:     ScheduleAt,
         checkTimestamp: ZonedDateTime
        )
        : F[Option[CheckedTemplate.PickingTemplate]] =
        PickingNameGenerator[F]
          .generate(picking, today, index, checkTimestamp)
          .flatMap { name =>
            val shouldSkipBecauseOfDate = picking.frequency match {
              case Frequency.Daily        => false
              case Frequency.Weekly(days) => !days.contains(DayOfWeek.ofDay(today))
            }

            val shouldSkipBecauseOfTime = !times.contains(picking.timeOfDay)

            if (picking.disabled) logger.info(show"Skipping $name because it is disabled").as(none)
            else if (shouldSkipBecauseOfDate) logger.debug(show"Skipping $name because of the day of the week").as(none)
            else if (shouldSkipBecauseOfTime) logger.debug(show"Skipping $name because of the time of day").as(none)
            else
              checkMoveSet(picking)
                .flatMap(NonEmptyList.fromList(_) match {
                  case None        => logger.info(show"Skipping $name because no moves need to be created").as(none)
                  case Some(moves) =>
                    CheckedTemplate
                      .PickingTemplate(
                        name,
                        picking.frequency,
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

      private def checkMoveSet(picking: Template.PickingTemplate): F[List[CheckedTemplate.MoveTemplate]] =
        picking.moves match {
          case MoveTemplateSet.Explicit(moves) => moves.toList.flatTraverse(checkMove(picking, _).map(_.toList))
          case MoveTemplateSet.All             =>
            queryLocationContents(picking.locationId).map(_.map { case product -> quantity =>
              CheckedTemplate.MoveTemplate(MoveName(ProductName.raw(product.name)), product.id, quantity)
            })
        }

      private def checkMove
        (picking: Template.PickingTemplate, move: Template.MoveTemplate)
        : F[Option[CheckedTemplate.MoveTemplate]] =
        calculateQuantity(
          move.productId,
          picking.locationId,
          picking.locationDestId,
          move.quantityType
        ).flatMap {
          case None                  => logger.info(show"Skipping ${move.name}: current stock is at or over max quantity").as(none)
          case Some(productQuantity) =>
            CheckedTemplate.MoveTemplate(move.name, move.productId, productQuantity).some.pure[F]
        }

      private def queryCurrentQuantity
        (productId: ProductId, locationId: Either[LocationId, LocationDestId])
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

      private def queryLocationContents(locationId: LocationId): F[List[(ProductInfo, Template.ProductQuantity)]] =
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
        (productId: ProductId, locationSrcId: LocationId, locationDestId: LocationDestId, quantityType: QuantityType)
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

  final case class QuantityOnHand(product: ProductInfo, locationId: LocationId, quantity: CurrentQuantity)
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
