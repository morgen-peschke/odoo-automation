package peschke.odoo

import cats.effect.kernel.Async
import cats.syntax.all._
import fs2.io.file.Files
import fs2.io.file.Path
import fs2.text
import io.circe.Decoder
import io.circe.parser
import peschke.odoo.utils.Circe._

import scala.util.Try

trait JsonLoader[F[_]] {
  def load[A: Decoder](source: JsonLoader.Source): F[A]
}
object JsonLoader      {
  def apply[F[_]](implicit JL: JsonLoader[F]): JL.type = JL

  sealed abstract class Source
  object Source {
    case object StdIn                     extends Source
    final case class JsonFile(path: Path) extends Source
    final case class RawJson(raw: String) extends Source

    implicit val decoder: Decoder[Source] = anyOf[Source](
      exactly("stdin").as(StdIn),
      Decoder[String].at("file").emapTry(raw => Try(Path(raw)).map(JsonFile)),
      Decoder[String].at("json").map(RawJson)
    )
  }

  def default[F[_]: Files: Async]: JsonLoader[F] = new JsonLoader[F] {
    def fetchRawString(source: Source): F[String] = source match {
      case Source.StdIn          => fs2.io.stdinUtf8[F](4096).compile.string
      case Source.JsonFile(path) => Files[F].readAll(path).through(text.utf8.decode).compile.string
      case Source.RawJson(raw)   => raw.pure[F]
    }

    override def load[A: Decoder](source: Source): F[A] =
      fetchRawString(source)
        .flatMap { raw =>
          parser
            .parse(raw)
            .leftMap(e => new IllegalArgumentException(show"Malformed JSON: $e"))
            .flatMap { json =>
              Decoder[A]
                .decodeAccumulating(json.hcursor)
                .leftMap { e =>
                  new IllegalArgumentException(s"Invalid JSON:\n${e.mkString_("\n")}")
                }
                .toEither
            }
            .liftTo[F]
        }
  }
}
