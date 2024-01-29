package peschke.odoo.algebras

import cats.effect.Temporal
import cats.effect.kernel.Resource
import cats.syntax.all._
import cats.{Monad, MonadThrow, Show}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.http4s.circe.CirceEntityDecoder.circeEntityDecoder
import org.http4s.client.Client
import org.http4s.{Response, Status}
import org.typelevel.log4cats.{LoggerFactory, SelfAwareStructuredLogger}
import peschke.odoo.algebras.JsonRpc.RpcResponse
import peschke.odoo.models.RpcServiceCall.{CommonService, ObjectService}
import peschke.odoo.models.{BodyDecodingFailure, RpcServiceCall, UnexpectedStatus}
import peschke.odoo.utils.Circe._
import upperbound.Limiter

import scala.concurrent.duration.FiniteDuration

trait JsonRpc[F[_]] {
  def call(serviceCall: RpcServiceCall): F[RpcResponse]
}
object JsonRpc {
  sealed trait Live
  sealed trait Dummy

  final case class RpcResponse(jsonrpc: String, id: Option[Int], result: Json)
  implicit val responseDecoder: Decoder[RpcResponse] = accumulatingDecoder { c =>
    (
      c.downField("jsonrpc").asAcc[String],
      c.downField("id").asAcc[Option[Int]],
      c.downField("result").asAcc[Json]
    ).mapN(RpcResponse)
  }
  implicit val responseEncoder: Encoder[RpcResponse] = Encoder.instance { rpc =>
    Json.obj(
      "jsonrpc" := rpc.jsonrpc,
      "id" := rpc.id.asJson,
      "result" := rpc.result
    )
  }
  implicit val responseShow: Show[RpcResponse] = Show.show(_.asJson.spaces2)

  def apply[F[_]](implicit JR: JsonRpc[F]): JR.type = JR

  def default[F[_]: Temporal: RequestBuilder: LoggerFactory](client: Client[F],
                                                             limiter: Limiter[F],
                                                             recoveryDelay: FiniteDuration): JsonRpc[F] with Live =
    new JsonRpc[F] with Live {
      private val logger: SelfAwareStructuredLogger[F] = LoggerFactory[F].getLogger

      private def resetDelay: Resource[F, Unit] =
        Resource.eval(logger.warn(s"Hit request limit, attempting to reset the limiter by sleeping $recoveryDelay")) >>
          Resource.sleep(recoveryDelay)

      private def parseResult(res: Response[F], serviceCall: RpcServiceCall): F[RpcResponse] =
        res.as[RpcResponse].attempt.flatMap(_.fold(
          throwable =>
            RequestBuilder[F]
              .requestCurl(serviceCall)
              .flatMap(BodyDecodingFailure.liftTo[F, RpcResponse](_, throwable)),
          _.pure[F]
        ))

      override def call(serviceCall: RpcServiceCall): F[RpcResponse] =
        RequestBuilder[F].request(serviceCall).flatMap { req =>
          limiter.submit(
            client
              .run(req)
              .flatMap { res =>
                if (res.status === Status.TooManyRequests) resetDelay >> client.run(req)
                else res.pure[Resource[F, *]]
              }
              .use { res =>
                if (res.status.isSuccess) parseResult(res, serviceCall)
                else
                  RequestBuilder[F]
                    .requestCurl(serviceCall)
                    .flatMap(UnexpectedStatus.liftResponse[F, RpcResponse](res, _))
              }
          )
        }
    }

  def dryRun[F[_] : Monad: LoggerFactory](mkResponse: RpcServiceCall => F[Json]): JsonRpc[F] with Dummy =
    new JsonRpc[F] with Dummy {
      private val logger = LoggerFactory[F].getLogger

      override def call(serviceCall: RpcServiceCall): F[RpcResponse] =
        mkResponse(serviceCall)
          .flatTap(json => logger.debug(s"[Dry Run] ${json.noSpaces}"))
          .map(RpcResponse("2.0", None, _))
    }

  def readOnly[F[_]](live: JsonRpc[F] with Live, dryRun: JsonRpc[F] with Dummy): JsonRpc[F] = {
    case
      serviceCall@(CommonService.Version |
                   CommonService.Login(_, _, _) |
                   ObjectService.FieldsGet(_, _, _) |
                   ObjectService.SearchRead(_, _, _, _, _) |
                   ObjectService.Read(_, _, _, _)) => live.call(serviceCall)
    case serviceCall => dryRun.call(serviceCall)
  }

  def verbose[F[_]: MonadThrow: RequestBuilder: LoggerFactory](wrapped: JsonRpc[F]): JsonRpc[F] = new JsonRpc[F] {
    private val logger = LoggerFactory[F].getLogger

    override def call(serviceCall: RpcServiceCall): F[RpcResponse] =
      wrapped.call(serviceCall).attemptTap { _ =>
        RequestBuilder[F]
          .requestCurl(serviceCall)
          .flatMap(curl => logger.info(s"[Verbose] $curl"))
      }
  }
}