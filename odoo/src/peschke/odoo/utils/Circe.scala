package peschke.odoo.utils

import cats.data.{Chain, NonEmptyChain, Validated}
import cats.syntax.all._
import cats.{Eq, Semigroup}
import io.circe.Decoder.{AccumulatingResult, Result}
import io.circe._
import org.typelevel.ci.CIString

object Circe {
  def accumulatingDecoder[A](f: ACursor => AccumulatingResult[A]): Decoder[A] = new Decoder[A] {
    override def apply(c: HCursor): Result[A] = decodeAccumulating(c).toEither.leftMap(_.head)

    override def decodeAccumulating(c: HCursor): AccumulatingResult[A] = f(c)
  }

  def fixed[A: Eq: Decoder: Encoder](sentinel: A): Decoder[Unit] = {
    val errorMessage = s"Expected ${Encoder[A].apply(sentinel).printWith(Printer.noSpaces)}"
    accumulatingDecoder { c =>
      c.asAcc[A].andThen { value =>
        Validated.condNel(value === sentinel, (), DecodingFailure(errorMessage, c.history))
      }
    }
  }

  def recursiveAccumulatingDecoder[A](f: (Decoder[A], ACursor) => AccumulatingResult[A]): Decoder[A] =
    new Decoder[A] { self =>
      override def apply(c: HCursor): Result[A] = decodeAccumulating(c).toEither.leftMap(_.head)

      override def decodeAccumulating(c: HCursor): AccumulatingResult[A] = f(self, c)
    }

  def recursiveEncoder[A](f: (Encoder[A], A) => Json): Encoder[A] = new Encoder[A] { self =>
    override def apply(a: A): Json = f(self, a)
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
          lhs.tryDecodeAccumulating(c).recoverWith {
            case lhsFailure => rhs.tryDecodeAccumulating(c).leftMap(lhsFailure.concatNel)
          }
      }
    }

  def anyOf[A](d0: Decoder[A], dN: Decoder[A]*): Decoder[A] =
    NonEmptyChain.fromChainPrepend(d0, Chain.fromSeq(dN)).reduce


  implicit final class DecoderHelpers(private val c: ACursor) extends AnyVal {
    def asAcc[A: Decoder]: AccumulatingResult[A] = Decoder[A].tryDecodeAccumulating(c)
  }

  implicit val ciStringDecoder: Decoder[CIString] = Decoder[String].map(CIString(_))
  implicit val ciStringEncoder: Encoder[CIString] = Encoder[String].contramap(_.toString)
}

