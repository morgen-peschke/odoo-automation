import $ivy.`com.goyeau::mill-scalafix::0.2.10`
import com.goyeau.mill.scalafix.ScalafixModule
import mill._, scalalib._, scalafmt._

trait StyleModule extends ScalafmtModule with ScalafixModule {
  override def scalacOptions =
    super.scalacOptions() ++ Seq(
      "-encoding",
      "UTF-8",
      "-deprecation",
      "-unchecked",
      "-feature",
      "-Ywarn-unused",
      "-Ywarn-dead-code",
      "-Ywarn-value-discard",
      "-Xfatal-warnings",
      "-language:higherKinds"
    )

  override def scalafixIvyDeps = super.scalafixIvyDeps() ++ Agg(
    ivy"com.github.liancheng::organize-imports:0.6.0",
    ivy"org.typelevel::typelevel-scalafix:0.1.5"
  )

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    ivy"com.olegpy::better-monadic-for:0.3.1",
    ivy"org.typelevel:::kind-projector:0.13.2"
  )

  override def scalaDocOptions = super.scalaDocOptions() ++ Seq("-no-link-warnings")
}

object odoo extends ScalaModule with StyleModule {
  def scalaVersion = "2.13.8"

  override def ivyDeps: T[Agg[Dep]] = super.ivyDeps() ++ Agg(
    ivy"com.monovore::decline:2.3.0",
    ivy"is.cir::ciris:3.1.0",
    ivy"org.http4s::http4s-ember-client:1.0.0-M39",
    ivy"org.http4s::http4s-dsl:1.0.0-M39",
    ivy"org.http4s::http4s-circe:1.0.0-M39",
    ivy"org.typelevel::cats-effect:3.3.12",
    ivy"org.rudogma::supertagged:2.0-RC2",
    ivy"com.beachape::enumeratum:1.7.0",
    ivy"com.lihaoyi::sourcecode:0.3.0",
    ivy"io.circe::circe-core:0.14.1",
    ivy"io.circe::circe-parser:0.14.1",
    ivy"org.typelevel::log4cats-slf4j:2.5.0"
  )

  override def runIvyDeps = Agg(ivy"ch.qos.logback:logback-classic:1.2.10")

  object test extends Tests with TestModule.Munit with StyleModule {
    override def ivyDeps: T[Agg[Dep]] = super.ivyDeps() ++ Agg(
      ivy"org.scalameta::munit:0.7.29",
      ivy"org.scalacheck::scalacheck:1.17.0",
      ivy"org.scalameta::munit-scalacheck:0.7.29"
    )
  }
}