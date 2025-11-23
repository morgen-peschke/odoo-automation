package peschke.odoo.utils

import cats.Eq
import cats.Semigroup
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.Validated
import cats.syntax.all._
import com.monovore.decline.Argument
import io.circe.Decoder.AccumulatingResult
import io.circe.Decoder.Result
import io.circe._
import org.typelevel.ci.CIString

object Circe {
  def accumulatingDecoder[A](f: ACursor => AccumulatingResult[A]): Decoder[A] = new Decoder[A] {
    override def apply(c: HCursor): Result[A] = decodeAccumulating(c).toEither.leftMap(_.head)

    override def decodeAccumulating(c: HCursor): AccumulatingResult[A] = f(c)
  }

  def exactly[A: Eq: Decoder: Encoder](sentinel: A): Decoder[Unit] = {
    def pretty(a: A): String = Encoder[A].apply(a).printWith(Printer.noSpaces)
    val errorMessage = s"Expected ${pretty(sentinel)}"
    Decoder.accumulatingInstance { c =>
      c.asAcc[A].andThen { value =>
        Validated.condNel(value === sentinel, (), DecodingFailure(s"$errorMessage but was ${pretty(value)}", c.history))
      }
    }
  }

  implicit def decoderSemigroup[A]: Semigroup[Decoder[A]] =
    Semigroup.instance { (lhs, rhs) =>
      new Decoder[A] {
        final def apply(c: HCursor): Decoder.Result[A] = tryDecode(c)

        override def tryDecode(c: ACursor): Decoder.Result[A] =
          lhs.tryDecode(c).orElse(rhs.tryDecode(c))

        override def decodeAccumulating(c: HCursor): AccumulatingResult[A] =
          tryDecodeAccumulating(c)

        override def tryDecodeAccumulating(c: ACursor): AccumulatingResult[A] =
          lhs.tryDecodeAccumulating(c).findValid(rhs.tryDecodeAccumulating(c))
      }
    }

  def anyOf[A](d0: Decoder[A], dN: Decoder[A]*): Decoder[A] =
    NonEmptyChain.fromChainPrepend(d0, Chain.fromSeq(dN)).reduce

  implicit val ciStringDecoder: Decoder[CIString] = Decoder[String].map(CIString(_))
  implicit val ciStringEncoder: Encoder[CIString] = Encoder[String].contramap(_.toString)

  implicit val circeJsonArgument: Argument[Json] = Argument.from("JSON") { raw =>
    io.circe.parser.parse(raw).leftMap(_.message).toValidatedNel
  }
}
