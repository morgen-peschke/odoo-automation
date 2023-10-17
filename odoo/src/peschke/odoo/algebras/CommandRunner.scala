package peschke.odoo.algebras

import cats.MonadThrow
import cats.effect.std.Console
import cats.syntax.all._
import io.circe.syntax._
import org.typelevel.log4cats.LoggerFactory
import peschke.odoo.AppConfig.AppCommand
import peschke.odoo.models._
import peschke.odoo.models.authentication.LoginUid

trait CommandRunner[F[_]] {
  def run(command: AppCommand): F[Unit]
}
object CommandRunner {
  def apply[F[_]](implicit AR: CommandRunner[F]): AR.type = AR

  def default[
    F[_] : MonadThrow
    : LoggerFactory : Console
    : ServiceCallBuilder: JsonRpc: LoginManager: PickingCreator: KnownIdsBuilder
  ]: CommandRunner[F] =
    new CommandRunner[F] {
      private val logger = LoggerFactory[F].getLogger

      override def run(command: AppCommand): F[Unit] =
        command match {
          case AppCommand.DoAction(Action.Login) =>
            ServiceCallBuilder[F]
              .fromAction(Action.Login)
              .flatMap(JsonRpc[F].call)
              .flatTap(r => logger.info(show"Result: $r"))
              .flatMap(_.result.as[LoginUid].liftTo[F])
              .flatMap(LoginManager[F].saveLogIn(_))
          case AppCommand.DoAction(action) =>
            ServiceCallBuilder[F]
              .fromAction(action)
              .flatMap(JsonRpc[F].call)
              .flatMap(r => logger.info(show"Result: $r"))
          case cp @ AppCommand.CreatePickings(_, _, _, _, _, _) => PickingCreator[F].create(cp)
          case AppCommand.ReloadKnownIds =>
            KnownIdsBuilder[F].build
              .map(_.asJson)
              .map(_.spaces2SortKeys)
              .flatMap(Console[F].println(_))
        }
    }
}
