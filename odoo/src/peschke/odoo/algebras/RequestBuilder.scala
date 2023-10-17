package peschke.odoo.algebras

import cats.Applicative
import cats.syntax.all._
import io.circe.Json
import io.circe.syntax._
import org.http4s.circe.CirceEntityEncoder.circeEntityEncoder
import org.http4s.{Method, Request}
import peschke.odoo.models.RpcServiceCall
import peschke.odoo.models.authentication.ServerUri


trait RequestBuilder[F[_]] {
  def request(serviceCall: RpcServiceCall): F[Request[F]]

  def requestCurl(serviceCall: RpcServiceCall): F[String]
}
object RequestBuilder {
  def apply[F[_]](implicit RB: RequestBuilder[F]): RB.type = RB

  def default[F[_]: Applicative](serverUri: ServerUri): RequestBuilder[F] = new RequestBuilder[F]{
    private val baseRequest: Request[F] = Request(
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

    override def request(serviceCall: RpcServiceCall): F[Request[F]] =
      baseRequest.withEntity(requestBody(serviceCall)).pure[F]

    override def requestCurl(serviceCall: RpcServiceCall): F[String] = {
      val body = requestBody(serviceCall).spaces2
      // Escaping shamelessly "borrowed" from org.http4s.internal.CurlConverter#escapeQuotationMarks
      val bodyArg = s"--data '${body.replaceAll("'", """'\\''""")}'"
      s"${baseRequest.asCurl(_ => false)} \\\n  $bodyArg".pure[F]
    }
  }
}
