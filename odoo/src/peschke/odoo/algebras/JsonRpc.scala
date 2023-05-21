package peschke.odoo.algebras

import cats.{Functor, Show}
import cats.effect.kernel.Concurrent
import cats.syntax.all._
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._
import org.http4s.circe.CirceEntityDecoder.circeEntityDecoder
import org.http4s.circe.CirceEntityEncoder.circeEntityEncoder
import org.http4s.client.Client
import org.http4s.{Method, Request}
import org.typelevel.log4cats.LoggerFactory
import peschke.odoo.algebras.JsonRpc.RpcResponse
import peschke.odoo.models.authentication.ServerUri
import peschke.odoo.models.{BodyDecodingFailure, RpcServiceCall, UnexpectedStatus}
import peschke.odoo.utils.Circe._

trait JsonRpc[F[_]] {
  def call(serviceCall: RpcServiceCall): F[RpcResponse]
}
object JsonRpc {

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

  def default[F[_]: Concurrent](serverUri: ServerUri, client: Client[F]): JsonRpc[F] =
    serviceCall => {
      def request: Request[F] = baseRequest[F](serverUri).withEntity(requestBody(serviceCall))
      client.run(request).use { res =>
        if (res.status.isSuccess) res.as[RpcResponse].attempt.flatMap(_.fold(
          throwable => fullCurl[F](request).flatMap(BodyDecodingFailure.liftTo[F, RpcResponse](_, throwable)),
          _.pure[F]
        ))
        else fullCurl[F](request).flatMap(UnexpectedStatus.liftResponse[F, RpcResponse](res, _))
      }
    }

  def dryRun[F[_]: Functor](mkResponse: RpcServiceCall => F[Json]): JsonRpc[F] =
    mkResponse(_).map(RpcResponse("2.0", None, _))

  def dryRunVerbose[F[_] : LoggerFactory : Concurrent](serverUri: ServerUri)
                                                      (mkResponse: RpcServiceCall => F[Json]): JsonRpc[F] =
    new JsonRpc[F] {
      private val logger = LoggerFactory[F].getLogger

      override def call(serviceCall: RpcServiceCall): F[RpcResponse] = {
        val body = requestBody(serviceCall)
        fullCurl[F](baseRequest(serverUri).withEntity(body))
          .flatMap(curl => logger.info(s"[Dry Run]: $curl"))
          .flatMap(_ => mkResponse(serviceCall).map(RpcResponse("2.0", None, _)))
      }
    }

  private def fullCurl[F[_]: Concurrent](request: Request[F]): F[String] =
    request.bodyText.compile.string.map { body =>
    // Escaping shamelessly "borrowed" from org.http4s.internal.CurlConverter#escapeQuotationMarks
    val bodyArg = if (body.isEmpty) "" else s" --data '${body.replaceAll("'", """'\\''""")}'"
    s"${request.asCurl(_ => false)} \\\n  $bodyArg"
  }

  private def baseRequest[F[_]](serverUri: ServerUri): Request[F] = Request(
    method = Method.POST,
    uri = ServerUri.raw(serverUri)
  )

  private def requestBody(serviceCall: RpcServiceCall): Json = Json.obj(
    "jsonrpc" := "2.0",
    "method" := "call",
    "params" := Json.obj(
      "service" := serviceCall.serviceName,
      "method" := serviceCall.methodName,
      "args" := serviceCall.args
    )
  )
}