package peschke.odoo.algebras

import cats.MonadThrow
import cats.data.NonEmptyList
import cats.effect.std.Console
import cats.syntax.all._
import io.circe.syntax._
import org.typelevel.log4cats.LoggerFactory
import peschke.odoo.AppConfig.AppCommand.CreatePickings
import peschke.odoo.models.CheckedTemplate._
import peschke.odoo.models.RpcServiceCall.ObjectService.FieldName
import peschke.odoo.models.RpcServiceCall.ObjectService.ModelName
import peschke.odoo.models._

trait PickingCreator[F[_]] {
  def create(createPickings: CreatePickings): F[Unit]

  def summarize(pickings: CreatePickings): F[Unit]
}
object PickingCreator      {
  def apply[F[_]](implicit PC: PickingCreator[F]): PC.type = PC

  def default[
      F[_]: ServiceCallBuilder: JsonRpc: MonadThrow: LoggerFactory: TemplateDecoder: TemplateChecker: Console
  ]: PickingCreator[F] =
    new PickingCreator[F] {
      private val logger = LoggerFactory[F].getLoggerFromClass(classOf[PickingCreator[F]])

      private def validate(createPickings: CreatePickings): F[Option[NonEmptyList[CheckedTemplate]]] =
        TemplateDecoder[F]
          .decode(createPickings.template, createPickings.knownIdsOpt)
          .flatMap(
            TemplateChecker[F].check(
              _,
              createPickings.times,
              createPickings.dateOverridesOpt,
              createPickings.scheduleAtOverrides,
              createPickings.templateFilters
            )
          )

      private def processTemplate(template: CheckedTemplate): F[Unit] =
        template.entries.traverse(processEntry).map(_.reduce)

      private def processEntry(entry: Entry): F[Unit] =
        logger.info(show"Processing entries for ${entry.label}") >>
          entry.pickings.traverse(processPicking).map(_.reduce)

      private def processPicking(picking: PickingTemplate): F[Unit] =
        createPicking(picking)
          .flatMap(pickingId => picking.moves.traverse(createMove(pickingId, picking, _)))
          .map(_.reduce)

      private def createPicking(picking: PickingTemplate): F[Picking.Id] =
        ServiceCallBuilder[F]
          .fromAction(
            Action.Create(
              Picking.Model,
              NonEmptyList.of(
                Picking.Name -> picking.name.asJson,
                Picking.MoveType -> picking.moveType.asJson,
                Picking.PickingTypeId -> picking.pickingTypeId.asJson,
                Picking.PartnerId -> picking.partnerId.asJson,
                Picking.LocationId -> picking.locationId.asJson,
                Picking.LocationDestId -> picking.locationDestId.asJson,
                Picking.ScheduledDate -> picking.scheduledDate.asJson // ,
                // Picking.ImmediateTransfer -> false.asJson
              )
            )
          )
          .flatMap(JsonRpc[F].call)
          .flatMap { response =>
            response
              .result.as[Picking.Id].fold(
                UnexpectedResponse.liftTo[F, Picking.Id]("creating picking", response, _),
                _.pure[F]
              )
          }
          .flatTap(id => logger.info(show"Created stock.picking $id (${picking.name})"))

      private def createMove(pickingId: Picking.Id, picking: PickingTemplate, move: MoveTemplate): F[Unit] =
        ServiceCallBuilder[F]
          .fromAction(
            Action.Create(
              Move.Model,
              NonEmptyList.of(
                Move.Name -> move.name.asJson,
                Move.ProductId -> move.productId.asJson,
                Move.ProductQuantity -> move.quantity.asJson,
                Move.PickingId -> pickingId.asJson,
                Picking.LocationId -> picking.locationId.asJson,
                Picking.LocationDestId -> picking.locationDestId.asJson
              )
            )
          )
          .flatMap(JsonRpc[F].call)
          .flatMap { body =>
            body
              .result.as[Picking.Id].fold(
                UnexpectedResponse.liftTo[F, Picking.Id]("creating move", body, _),
                _.pure[F]
              )
          }
          .flatMap(id =>
            logger.info(
              show"Created stock.move $id (${move.name}) ${move.quantity} from ${picking.locationId} -> ${picking.locationDestId}"
            )
          )

      override def create(createPickings: CreatePickings): F[Unit] =
        validate(createPickings).flatMap {
          case None                   => logger.info("Nothing to create")
          case Some(checkedTemplates) =>
            checkedTemplates.traverse(processTemplate).map(_.reduce)
        }

      override def summarize(pickings: CreatePickings): F[Unit] = {
        val out = Console[F]
        validate(pickings).flatMap {
          case None            => out.println("Nothing would be done.")
          case Some(validated) =>
            validated.traverse_(_.entries.traverse { entry =>
              val labelBar = "=" * entry.label.string.length
              out.println {
                s"""
                   |.=$labelBar=.
                   || ${entry.label} |
                   |'=$labelBar='""".stripMargin
              } >> entry.pickings.traverse_ { picking =>
                out.println {
                  s""" ${picking.name}
                     | ${"-" * picking.name.string.length}
                     | ${picking.locationId} ~~> ${picking.locationDestId}""".stripMargin
                } >> out.println {
                  picking
                    .moves.map { move =>
                      s"  * ${move.productId} x ${move.quantity}"
                    }.mkString_("\n")
                }
              }
            })
        }
      }
    }

  private object Picking {
    object Id extends PosInt("stock.picking")
    type Id = Id.Type

    val Model: ModelName = ModelName("stock.picking")

    val Name: FieldName = FieldName("name")
    val MoveType: FieldName = FieldName("move_type")
    val PickingTypeId: FieldName = FieldName("picking_type_id")
    val PartnerId: FieldName = FieldName("partner_id")
    val LocationId: FieldName = FieldName("location_id")
    val LocationDestId: FieldName = FieldName("location_dest_id")
    val ScheduledDate: FieldName = FieldName("scheduled_date")
  }

  private object Move {
    val Model: ModelName = ModelName("stock.move")

    val Name: FieldName = FieldName("name")
    val ProductId: FieldName = FieldName("product_id")
    val ProductQuantity: FieldName = FieldName("product_uom_qty")
    val PickingId: FieldName = FieldName("picking_id")
  }
}
