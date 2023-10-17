package peschke.odoo.models

import cats.{Order, Show}
import cats.syntax.all._
import com.monovore.decline.Argument
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

import java.time.LocalTime
import java.time.format.{DateTimeFormatter, DateTimeParseException}

trait NewBoolean extends supertagged.NewType[Boolean] {
  val Enabled: Type = apply(true)
  val Disabled: Type = apply(false)

  implicit val show: Show[Type] = Show.show(raw(_).show)

  implicit val decoder: Decoder[Type] = Decoder[Boolean].map(apply(_))
  implicit val encoder: Encoder[Type] = Encoder[Boolean].contramap(raw)

  implicit val argument: Argument[Type] = Argument.from("boolean")(_.toUpperCase match {
    case "T" | "TRUE" => Enabled.valid
    case "F" | "FALSE" => Disabled.valid
    case _ => "Expected one of: 'true', 't', 'false', 'f'".invalidNel
  })
}

trait NewString extends supertagged.NewType[String] {
  def fromString(raw: String): Either[String, Type]

  implicit val show: Show[Type] = Show.show(raw)
  implicit val order: Order[Type] = Order.by(raw)
  implicit val decoder: Decoder[Type] = Decoder[String].emap(fromString)
  implicit val keyDecoder: KeyDecoder[Type] = KeyDecoder.instance(fromString(_).toOption)
  implicit val encoder: Encoder[Type] = Encoder[String].contramap(raw)
  implicit val keyEncoder: KeyEncoder[Type] = KeyEncoder.instance(raw)

  implicit val argument: Argument[Type] = Argument.from("str")(fromString(_).toValidatedNel)
  implicit val listArgument: Argument[List[Type]] =
    Argument.from("str0,str1,...strN") { raw =>
      if (raw.isEmpty) List.empty[Type].valid
      else raw.split(',').toList.traverse(fromString(_).toValidatedNel)
    }
}

abstract class NonEmptyString(description: String) extends NewString {
  def fromString(raw: String): Either[String, Type] =
    if (raw.isEmpty) s"$description cannot be empty".asLeft
    else if (raw.isBlank) s"$description cannot be blank".asLeft
    else apply(raw).asRight
}

trait NewInt extends supertagged.NewType[Int] {
  def fromInt(i: Int): Either[String, Type]

  implicit val show: Show[Type] = Show.show(raw(_).show)

  implicit val decoder: Decoder[Type] = Decoder[Int].emap(fromInt)
  implicit val encoder: Encoder[Type] = Encoder[Int].contramap(raw)

  implicit val argument: Argument[Type] = Argument.from("int") { raw =>
    Argument[Int].read(raw).andThen(fromInt(_).toValidatedNel)
  }
}

abstract class PosInt(description: String) extends NewInt {
  def fromInt(i: Int): Either[String, Type] =
    if (i <= 0) s"$description must be greater than 0".asLeft
    else apply(i).asRight
}

trait NewDouble extends supertagged.NewType[Double] {
  def fromDouble(i: Double): Either[String, Type]

  implicit val show: Show[Type] = Show.show(raw(_).show)

  implicit val decoder: Decoder[Type] = Decoder[Double].emap(fromDouble)
  implicit val encoder: Encoder[Type] = Encoder[Double].contramap(raw)

  implicit val argument: Argument[Type] = Argument.from("float") { raw =>
    Argument[Int].read(raw).andThen(fromDouble(_).toValidatedNel)
  }
}

abstract class PosDouble(description: String) extends NewDouble {
  def fromDouble(d: Double): Either[String, Type] =
    if (d <= 0d) s"$description must be greater than 0.0".asLeft
    else apply(d).asRight
}

abstract class NonNegativeDouble(description: String) extends NewDouble {
  def fromDouble(d: Double): Either[String, Type] =
    if (d < 0d) s"$description must be greater than or equal to than 0.0".asLeft
    else apply(d).asRight
}

abstract class NewLocalTime(val name: String) extends supertagged.NewType[LocalTime] {
  def fromLocalTime(t: LocalTime): Either[String, Type]

  def fromString(s: String): Either[String, Type] =
    Either
      .catchOnly[DateTimeParseException](LocalTime.parse(s, NewLocalTime.formatter))
      .leftMap(ex => s"Illegal $name: ${ex.getMessage}")
      .flatMap(fromLocalTime(_).leftMap(e => s"Illegal $name: $e"))

  implicit val show: Show[Type] = Show.show(raw(_).format(NewLocalTime.formatter))

  implicit val decoder: Decoder[Type] = Decoder[String].emap(fromString)
  implicit val encoder: Encoder[Type] = Encoder[String].contramap(raw(_).format(NewLocalTime.formatter))

  implicit val argument: Argument[Type] = Argument.from(NewLocalTime.pattern) { raw =>
    Argument[String].read(raw).andThen(fromString(_).toValidatedNel)
  }
}
object NewLocalTime {
  private val pattern = "HH:mm"
  private val formatter = DateTimeFormatter.ofPattern(pattern)
}