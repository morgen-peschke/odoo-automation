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
object CommandRunner      {
  def apply[F[_]](implicit AR: CommandRunner[F]): AR.type = AR

  def default[
      F[
          _
      ]: MonadThrow: LoggerFactory: Console: ServiceCallBuilder: JsonRpc: LoginManager: PickingCreator: KnownIdsBuilder
  ]: CommandRunner[F] =
    new CommandRunner[F] {
      private val logger = LoggerFactory[F].getLoggerFromClass(classOf[CommandRunner[F]])

      override def run(command: AppCommand): F[Unit] =
        command match {
          case AppCommand.DoAction(Action.Login) =>
            ServiceCallBuilder[F]
              .fromAction(Action.Login)
              .flatMap(JsonRpc[F].call)
              .flatTap(r => logger.info(show"Result: $r"))
              .flatMap(_.result.as[LoginUid].liftTo[F])
              .flatMap(LoginManager[F].saveLogIn(_))
          case AppCommand.DoAction(action)       =>
            ServiceCallBuilder[F]
              .fromAction(action)
              .flatMap(JsonRpc[F].call)
              .flatMap(r => logger.info(show"Result: $r"))
          case cp: AppCommand.CreatePickings     => PickingCreator[F].create(cp)
          case AppCommand.ReloadKnownIds         =>
            KnownIdsBuilder[F]
              .build
              .map(_.asJson)
              .map(_.spaces2SortKeys)
              .flatMap(Console[F].println(_))
        }
    }
}

import org.typelevel.ci.CIString

sealed trait Foo {
  def name: CIString
  def fold[A](whenBar: => A, whenBaz: => A, whenOther: CIString => A): A =
    this match {
      case Foo.Bar                => whenBar
      case Foo.Baz                => whenBaz
      case Foo.Qux                => whenOther(Foo.Qux.name)
      case Foo.Unrecognized(name) => whenOther(name)
    }

  def fold[A](whenBar: => A, whenBaz: => A, whenQux: => A, whenOther: CIString => A): A =
    this match {
      case Foo.Bar                => whenBar
      case Foo.Baz                => whenBaz
      case Foo.Qux                => whenQux
      case Foo.Unrecognized(name) => whenOther(name)
    }
}
object Foo       {

  def bar: Foo = Bar
  def baz: Foo = Baz
  def qux: Foo = Qux
  def other(name: String): Foo = fromCIString(CIString(name))

  def fromCIString(name: CIString): Foo = name match {
    case Bar.name => bar
    case Baz.name => baz
    case Qux.name => qux
    case _        => Unrecognized(name)
  }

  private case object Bar extends Foo {
    val name: CIString = CIString("bar")
  }

  private case object Baz extends Foo {
    val name: CIString = CIString("baz")
  }

  private case object Qux extends Foo {
    val name: CIString = CIString("qux")
  }

  private case class Unrecognized(name: CIString) extends Foo
}
