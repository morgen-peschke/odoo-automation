package peschke.odoo.cli

import peschke.odoo.{AppConfig, JsonLoader}
import cats.syntax.all._
import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.{Console, Random}
import com.monovore.decline.Help
import io.circe.Json
import io.circe.syntax._
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory
import peschke.odoo.AppConfig.DryRun
import peschke.odoo.algebras.{CommandRunner, Generator, JsonRpc, KnownIdsBuilder, LoginManager, PickingCreator, ServiceCallBuilder, TemplateDecoder}
import peschke.odoo.models.RpcServiceCall
import peschke.odoo.models.RpcServiceCall.{CommonService, ObjectService}

import java.time.ZoneId

object Run extends IOApp {

  implicit val logging: LoggerFactory[IO] = Slf4jFactory[IO]

  override def run(args: List[String]): IO[ExitCode] =
    ArgumentParser
      .default[IO](JsonLoader.default[IO])
      .parse(args)
      .flatMap(_.fold(handleError, run))

  private def handleError(help: Help): IO[ExitCode] = Console[IO].println(help).as(ExitCode.Error)

  def run(config: AppConfig): IO[ExitCode] =
    EmberClientBuilder.default[IO].build.use { client =>
      Random.scalaUtilRandom[IO].flatMap { implicit random =>
        implicit val jsonLoader: JsonLoader[IO] = JsonLoader.default[IO]
        implicit val loginManager: LoginManager[IO] = LoginManager.default[IO](config.loginCache, config.auth)
        implicit val serviceCallBuilder: ServiceCallBuilder[IO] = ServiceCallBuilder.default[IO](config.auth)
        implicit val templateDecoder: TemplateDecoder[IO] = TemplateDecoder.default[IO]
        implicit val jsonRpc: JsonRpc[IO] = {
          val idGen = Generator.usingRandom[IO, Json](_.nextIntBounded(10000).map(_.asJson))
          val makeResponse: RpcServiceCall => IO[Json] = {
            case CommonService.Version |
                 ObjectService.FieldsGet(_, _, _) |
                 ObjectService.SearchRead(_, _, _, _, _) |
                 ObjectService.Read(_, _, _, _) |
                 ObjectService.Write(_, _, _, _) => Json.obj().pure[IO]
            case CommonService.Login(_, _, _) | ObjectService.Create(_, _, _) => idGen.create
          }
          config.dryRun match {
            case DryRun.Disabled => JsonRpc.default[IO](config.auth.serverUrl, client)
            case DryRun.Enabled => JsonRpc.dryRun[IO](makeResponse)
            case DryRun.Verbose => JsonRpc.dryRunVerbose[IO](config.auth.serverUrl)(makeResponse)
          }
        }
        implicit val pickingCreator: PickingCreator[IO] = PickingCreator.default[IO](ZoneId.systemDefault())
        implicit val knownIdsBuilder: KnownIdsBuilder[IO] = KnownIdsBuilder.default[IO]
        implicit val commandRunner: CommandRunner[IO] = CommandRunner.default[IO]
        commandRunner.run(config.command).as(ExitCode.Success)
      }
    }
}
