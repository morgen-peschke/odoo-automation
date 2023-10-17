package peschke.odoo.algebras

import cats.MonadThrow
import cats.data.ValidatedNel
import cats.syntax.all._
import io.circe.{Decoder, DecodingFailure}
import org.typelevel.log4cats.LoggerFactory
import peschke.odoo.models.Action.Search
import peschke.odoo.models.Action.Search.Limit
import peschke.odoo.models.Action.Search.Condition.syntax._
import peschke.odoo.models.RpcServiceCall.ObjectService.{FieldName, ModelName}
import peschke.odoo.models.Template.{LocationId, PartnerId, PickingTypeId, ProductId}
import peschke.odoo.models.{KnownIds, RpcServiceCall}
import peschke.odoo.utils.Circe._

trait KnownIdsBuilder[F[_]] {
  def build: F[KnownIds]
}
object KnownIdsBuilder {
  def apply[F[_]](implicit KI: KnownIdsBuilder[F]): KI.type = KI

  def default[F[_]: ServiceCallBuilder: JsonRpc: MonadThrow: LoggerFactory]: KnownIdsBuilder[F] = new KnownIdsBuilder[F] {
    private val logger = LoggerFactory[F].getLogger
    private val name: FieldName = FieldName("name")

    def decoder[Id: Decoder](nameField: FieldName): Decoder[List[(String, Id)]] = {
      implicit val innerDecoder: Decoder[(String, Id)] = accumulatingDecoder { c =>
        (
          c.downField(FieldName.raw(nameField)).asAcc[String],
          c.downField("id").asAcc[Id]
        ).tupled
      }
      Decoder[List[(String, Id)]]
    }

    private def buildRequest(model: ModelName, nameField: FieldName): F[RpcServiceCall] =
      ServiceCallBuilder[F].fromAction(Search(
        model = model,
        fields = nameField :: Nil,
        conditions = FieldName("active").is("true") :: Nil,
        limitOpt = none[Limit]
      ))

    private def load[Id: Decoder](model: ModelName,
                                  fieldName: FieldName): F[ValidatedNel[DecodingFailure, Map[String, Id]]] =
      buildRequest(model, fieldName)
        .flatMap(JsonRpc[F].call)
        .map(_.result.hcursor.asAcc(decoder[Id](fieldName)).map(_.toMap))

    override def build: F[KnownIds] =
      (
        load[LocationId](KnownIds.locations,  FieldName("complete_name")),
        load[ProductId](KnownIds.products, name),
        load[PickingTypeId](KnownIds.pickingTypeIds,  FieldName("display_name")),
        load[PartnerId](KnownIds.partners, name)
      ).tupled.flatMap(_.mapN(KnownIds.apply).fold(
        e =>
          logger
            .error(e.mkString_("Unable to load ids from server:\n  ", "\n  ", "\n"))
            .flatMap(_ => new IllegalStateException().raiseError[F, KnownIds]),
        _.pure[F]
      ))
  }
}