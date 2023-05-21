package peschke.odoo.models

import cats.ApplicativeThrow
import cats.effect.kernel.Concurrent
import cats.syntax.all._
import org.http4s.{Response, Status}
import peschke.odoo.algebras.JsonRpc.RpcResponse

import scala.util.control.NoStackTrace

class UnexpectedStatus(status: Status, bodyDescription: String, requestDescription: String) extends RuntimeException {
  override def getMessage: String =
    show"Unexpected status: $status\n\nRequest:\n$requestDescription\n\nResponse:\n$bodyDescription\n"
}
object UnexpectedStatus {
  def fromResponse[F[_]: Concurrent](response: Response[F], requestDescription: String): F[UnexpectedStatus] =
    response.bodyText[F].compile.string.attempt.map(_.fold(
      throwable =>
        new UnexpectedStatus(
          response.status,
          s"Unable to read body: ${throwable.getMessage}",
          requestDescription
        ),
      new UnexpectedStatus(response.status, _, requestDescription)
    ))

  def liftResponse[F[_]: Concurrent, A](response: Response[F], requestCurl: String): F[A] =
    fromResponse[F](response, requestCurl).flatMap(_.raiseError[F, A])
}

class UnexpectedResponse(operation: String, body: RpcResponse, causedBy: Exception)
  extends RuntimeException(show"Unexpected response while $operation: $body", causedBy)

object UnexpectedResponse {
  def liftTo[F[_]: ApplicativeThrow, A](operation: String, body: RpcResponse, causedBy: Exception): F[A] =
    new UnexpectedResponse(operation, body, causedBy).raiseError[F, A]
}

class BodyDecodingFailure(requestDescription: String, failure: Throwable)
  extends RuntimeException(show"Body decoding failure in request:\n$requestDescription", failure)
  with NoStackTrace

object BodyDecodingFailure {
  def liftTo[F[_] : ApplicativeThrow, A](requestDescription: String, failure: Throwable): F[A] =
    new BodyDecodingFailure(requestDescription, failure).raiseError[F, A]
}