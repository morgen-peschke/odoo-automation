ThisBuild / scalaVersion := "2.13.18"

lazy val odoo =
    project
      .in(file("."))
      .settings(
        name := "odoo-automation",
        description := "odoo-automation",
        organization := "com.peschke",
        licenses ++= Seq(License.GPL3_or_later),
        developers := List(
          Developer(
            id = "morgen-peschke",
            name = "Morgen Peschke",
            email = "",
            url = url("https://github.com/morgen-peschke"))
        ),
        maintainer := "morgen.peschke@gmail.com",
        version := "0.1.0",
        versionScheme := Some("early-semver"),
        scalaVersion := "2.13.18",
        addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.4" cross CrossVersion.full),
        libraryDependencies ++= Seq(
          "com.monovore" %% "decline" % "2.3.0",
          "is.cir" %% "ciris" % "3.1.0",
          "org.http4s" %% "http4s-ember-client" % "1.0.0-M39",
          "org.http4s" %% "http4s-dsl" % "1.0.0-M39",
          "org.http4s" %% "http4s-circe" % "1.0.0-M39",
          "org.typelevel" %% "cats-effect" % "3.3.12",
          "org.rudogma" %% "supertagged" % "2.0-RC2",
          "com.beachape" %% "enumeratum" % "1.7.0",
          "com.lihaoyi" %% "sourcecode" % "0.3.0",
          "io.circe" %% "circe-core" % "0.14.15",
          "io.circe" %% "circe-parser" % "0.14.15",
          "org.typelevel" %% "log4cats-slf4j" % "2.5.0",
          "org.slf4j" % "slf4j-api" % "2.0.13",
          "com.github.spullara.mustache.java" % "compiler" % "0.9.10",
          "org.systemfw" %% "upperbound" % "0.5.0",
          "org.typelevel" %% "cats-parse" % "0.3.9",
          "ch.qos.logback" % "logback-classic" % "1.2.10" % Runtime,
          "org.scalameta" %% "munit" % "0.7.29" % Test,
          "org.scalacheck" %% "scalacheck" % "1.17.0" % Test,
          "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test
        ),
        // avoid generating docs
        Compile / doc / sources                := Nil,
        Compile / packageDoc / publishArtifact := false
      )
      .enablePlugins(JavaAppPackaging)
