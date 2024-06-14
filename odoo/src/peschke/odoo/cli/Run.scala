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
import peschke.odoo.AppConfig.{DryRun, Verbose}
import peschke.odoo.algebras.{CommandRunner, DateOverrideResolver, Generator, JsonRpc, KnownIdsBuilder, LoginManager, PickingCreator, PickingNameGenerator, RequestBuilder, ServiceCallBuilder, TemplateChecker, TemplateDecoder}
import peschke.odoo.models.RpcServiceCall.{CommonService, ObjectService}
import upperbound.Limiter

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
      Limiter.start[IO](config.minIntervalBetweenRequests).use { limiter =>
        Random.scalaUtilRandom[IO].flatMap { implicit random =>
          implicit val jsonLoader: JsonLoader[IO] = JsonLoader.default[IO]
          implicit val loginManager: LoginManager[IO] = LoginManager.default[IO](config.loginCache, config.auth)
          implicit val serviceCallBuilder: ServiceCallBuilder[IO] = ServiceCallBuilder.default[IO](config.auth)
          implicit val templateDecoder: TemplateDecoder[IO] = TemplateDecoder.default[IO]
          implicit val requestBuilder: RequestBuilder[IO] = RequestBuilder.default[IO](config.auth.serverUrl)
          implicit val jsonRpc: JsonRpc[IO] = {
            def liveRpc = JsonRpc.default[IO](
              client,
              limiter,
              config.minIntervalBetweenRequests
            )

            def dryRunRpc = {
              val idGen = Generator.usingRandom[IO, Json](_.nextIntBounded(10000).map(_.asJson))
              JsonRpc.dryRun[IO] {
                case CommonService.Version |
                     ObjectService.FieldsGet(_, _, _) |
                     ObjectService.SearchRead(_, _, _, _, _) |
                     ObjectService.Read(_, _, _, _) |
                     ObjectService.Write(_, _, _, _) => Json.obj().pure[IO]
                case CommonService.Login(_, _, _) | ObjectService.Create(_, _, _) => idGen.create
              }
            }

            val runner = config.dryRun match {
              case DryRun.Enabled => dryRunRpc
              case DryRun.Disabled => liveRpc
              case DryRun.ReadOnly => JsonRpc.readOnly(liveRpc, dryRunRpc)
            }
            config.verbose match {
              case Verbose.Enabled => JsonRpc.verbose(runner)
              case _ => runner
            }
          }
          implicit val pickingNameGenerator: PickingNameGenerator[IO] = PickingNameGenerator.default[IO](
            globalNamePrefixOpt = config.globalNamePrefixOpt,
            globalNameSuffixOpt = config.globalNameSuffixOpt
          )
          implicit val dateOverrideResolver: DateOverrideResolver[IO] =
            DateOverrideResolver.default[IO](ZoneId.systemDefault())
          implicit val templateChecker: TemplateChecker[IO] = TemplateChecker.default[IO](ZoneId.systemDefault())
          implicit val pickingCreator: PickingCreator[IO] = PickingCreator.default[IO]
          implicit val knownIdsBuilder: KnownIdsBuilder[IO] = KnownIdsBuilder.default[IO]
          implicit val commandRunner: CommandRunner[IO] = CommandRunner.default[IO]

          commandRunner.run(config.command).as(ExitCode.Success)
        }
      }
    }
}
