package peschke.odoo.algebras

import cats.data.{NonEmptyList, NonEmptySet}
import cats.effect.kernel.Clock
import cats.syntax.all._
import cats.{MonadThrow, Show}
import io.circe.syntax._
import org.typelevel.log4cats.LoggerFactory
import peschke.odoo.AppConfig.AppCommand.CreatePickings
import peschke.odoo.models.RpcServiceCall.ObjectService.{FieldName, ModelName}
import peschke.odoo.models.Template._
import peschke.odoo.models._

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZoneId, ZonedDateTime}

trait PickingCreator[F[_]] {
  def create(createPickings: CreatePickings): F[Unit]
}
object PickingCreator {
  def apply[F[_]](implicit PC: PickingCreator[F]): PC.type = PC

  def default[
    F[_] : ServiceCallBuilder : JsonRpc : MonadThrow : LoggerFactory : TemplateDecoder : Clock
  ](zoneId: ZoneId): PickingCreator[F] =
    new PickingCreator[F] {
      private val logger = LoggerFactory[F].getLogger
      private implicit val localDateShow: Show[LocalDate] = {
        val formatter = DateTimeFormatter.ISO_LOCAL_DATE
        Show.show(_.format(formatter))
      }

      private def processTemplate(template: Template, today: LocalDate, times: NonEmptySet[TimeOfDay]): F[Unit] =
        template.entries.traverse(processEntry(_, today, times)).map(_.reduce)

      private def processEntry(entry: Entry, today: LocalDate, times: NonEmptySet[TimeOfDay]): F[Unit] =
        logger.info(show"Processing entries for ${entry.person}") >>
          entry.pickings.traverseWithIndexM(processPicking(_, entry.namePrefix, today, times, _)).map(_.reduce)

      private def processPicking(picking: PickingTemplate,
                                 pickingNamePrefix: PickingNamePrefix,
                                 today: LocalDate,
                                 times: NonEmptySet[TimeOfDay],
                                 index: Int): F[Unit] = {
        val name = PickingName(show"$pickingNamePrefix/$today/${picking.timeOfDay}/$index")

        val shouldSkipBecauseOfDate = picking.frequency match {
          case Frequency.Daily => false
          case Frequency.Weekly(days) => !days.contains(today.getDayOfWeek)
        }

        val shouldSkipBecauseOfTime = !times.contains(picking.timeOfDay)

        if (shouldSkipBecauseOfDate) logger.info(show"Skipping $name because of the day of the week")
        else if (shouldSkipBecauseOfTime) logger.info(show"Skipping $name because of the time of day")
        else
          createPicking(picking, name)
            .flatMap(pickingId => picking.moves.traverse(createMove(pickingId, picking, _)))
            .map(_.reduce)
      }

      private def createPicking(picking: PickingTemplate, name: PickingName): F[Picking.Id] =
        ServiceCallBuilder[F]
          .fromAction(Action.Create(
            Picking.Model,
            NonEmptyList.of(
              Picking.Name -> name.asJson,
              Picking.MoveType -> picking.moveType.asJson,
              Picking.PickingTypeId -> picking.pickingTypeId.asJson,
              Picking.PartnerId -> picking.partnerId.asJson,
              Picking.LocationId -> picking.locationId.asJson,
              Picking.LocationDestId -> picking.locationDestId.asJson
            )
          ))
          .flatMap(JsonRpc[F].call)
          .flatMap { result =>
            result.result.as[Picking.Id].fold(
              UnexpectedResponse.liftTo[F, Picking.Id]("creating picking", result, _),
              _.pure[F]
            )
          }
          .flatTap(id => logger.info(show"Created stock.picking $id ($name)"))

      private def createMove(pickingId: Picking.Id, picking: PickingTemplate, move: MoveTemplate): F[Unit] =
        ServiceCallBuilder[F]
          .fromAction(Action.Create(
            Move.Model,
            NonEmptyList.of(
              Move.Name -> move.name.asJson,
              Move.ProductId -> move.productId.asJson,
              Move.ProductQuantity -> move.productQuantity.asJson,
              Move.PickingId -> pickingId.asJson,
              Picking.LocationId -> picking.locationId.asJson,
              Picking.LocationDestId -> picking.locationDestId.asJson
            )
          ))
          .flatMap(JsonRpc[F].call)
          .flatMap { body =>
            body.result.as[Picking.Id].fold(
              UnexpectedResponse.liftTo[F, Picking.Id]("creating move", body, _),
              _.pure[F]
            )
          }
          .flatMap(id => logger.info(show"Created stock.move $id (${move.name})"))


      override def create(createPickings: CreatePickings): F[Unit] =
        for {
          template <- TemplateDecoder[F].decode(createPickings.template, createPickings.knownIdsOpt)
          today <- createPickings.dateOverrideOpt.map(_.pure[F]).getOrElse {
            Clock[F].realTimeInstant.map(ZonedDateTime.ofInstant(_, zoneId).toLocalDate)
          }
          result <- processTemplate(template, today, createPickings.times.getOrElse(TimeOfDay.All))
        } yield result
    }

  private object PickingName extends NonEmptyString("Picking name")
  private type PickingName = PickingName.Type

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
  }

  private object Move {
    val Model: ModelName = ModelName("stock.move")

    val Name: FieldName = FieldName("name")
    val ProductId: FieldName = FieldName("product_id")
    val ProductQuantity: FieldName = FieldName("product_uom_qty")
    val PickingId: FieldName = FieldName("picking_id")
  }
}