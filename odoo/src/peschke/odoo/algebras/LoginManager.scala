package peschke.odoo.algebras

import cats.effect.kernel.Concurrent
import cats.syntax.all._
import io.circe.syntax._
import fs2.io.file.Files
import peschke.odoo.AppConfig.{AuthConfig, LoginCache}
import peschke.odoo.JsonLoader
import peschke.odoo.models.authentication.{LoggedIn, LoginUid}

trait LoginManager[F[_]]{
  def getCachedLogin: F[LoggedIn]
  def saveLogIn(uid: LoginUid): F[Unit]
}
object LoginManager {
  def apply[F[_]](implicit LM: LoginManager[F]): LM.type = LM

  def default[F[_]: JsonLoader: Concurrent: Files](loginCache: LoginCache, authConfig: AuthConfig): LoginManager[F] =
    new LoginManager[F] {
      private val cacheFilePath = LoginCache.raw(loginCache)
      private val cacheFileSource = JsonLoader.Source.JsonFile(cacheFilePath)

      override def getCachedLogin: F[LoggedIn] =
        JsonLoader[F].load[LoginUid](cacheFileSource).map(LoggedIn(authConfig.database, _, authConfig.apiKey))

      override def saveLogIn(uid: LoginUid): F[Unit] =
        fs2.Stream
          .emit(uid.asJson.noSpaces)
          .through(fs2.text.utf8.encode[F])
          .through(Files[F].writeAll(cacheFilePath))
          .void
          .compile
          .drain
    }
}
