package peschke.odoo.models

import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.syntax.all._
import com.monovore.decline.Argument
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import peschke.odoo.models.RpcServiceCall.ObjectService.FieldName
import peschke.odoo.models.RpcServiceCall.ObjectService.Id
import peschke.odoo.models.RpcServiceCall.ObjectService.ModelName
import peschke.odoo.utils.Circe._

sealed trait Action
object Action {
  case object ServerInfo extends Action

  case object Login extends Action

  final case class Fields(model: ModelName, attributes: Either[List[Fields.Attribute], Fields.DefaultAttributes])
      extends Action
  object Fields {
    object Attribute extends NonEmptyString("Attribute")
    type Attribute = Attribute.Type

    object DefaultAttributes {
      val attributes: List[Attribute] = List(
        "string",
        "help",
        "required",
        "manual",
        "type",
        "selection",
        "relation",
        "relation_field",
        "digits",
        "searchable"
      ).map(Attribute(_))
    }
    type DefaultAttributes = DefaultAttributes.type

    implicit val defaultAttributesArgument: Argument[DefaultAttributes] =
      Argument.fromMap("default", Map("default" -> DefaultAttributes))

    implicit val attributesArgument: Argument[List[Attribute]] =
      Argument.from("attr0,attr1,...attrN") { raw =>
        raw.split(',').toList.traverse(Attribute.fromString(_).toValidatedNel)
      }

    implicit val attributeListDecoder: Decoder[Either[List[Attribute], DefaultAttributes]] =
      anyOf[Either[List[Attribute], DefaultAttributes]](
        fixed("default").as(DefaultAttributes.asRight),
        Decoder[List[Fields.Attribute]].map(_.asLeft)
      )

    implicit val attributeListArgument: Argument[Either[List[Attribute], DefaultAttributes]] =
      Argument.from("default|attr0,attr1,...,attrN") {
        case "default" => DefaultAttributes.asRight.valid
        case csv       => attributesArgument.read(csv).map(_.asLeft)
      }

    implicit val decoder: Decoder[Fields] = accumulatingDecoder { c =>
      (
        c.downField("model").asAcc[ModelName],
        c.downField("attributes").asAcc[Either[List[Attribute], DefaultAttributes]]
      ).mapN(Fields.apply)
    }
  }

  final case class Search
    (model: ModelName, fields: List[FieldName], conditions: List[Search.Condition], limitOpt: Option[Search.Limit])
      extends Action
  object Search {
    object Condition extends supertagged.NewType[List[Json]] {
      implicit val decoder: Decoder[Type] = Decoder[List[Json]].map(apply(_))
      implicit val encoder: Encoder[Type] = Encoder[List[Json]].contramap(raw)

      implicit val argument: Argument[Type] = Argument.from("json array") { raw =>
        io.circe.parser.parse(raw)
          .leftMap(_.message)
          .flatMap(_.as[List[Json]].leftMap(_.show))
          .map(apply(_))
          .toValidatedNel
      }

      object syntax {
        implicit final class FieldNameConditionOps(private val fieldName: FieldName) extends AnyVal {
          def is[A: Encoder](a: A): Condition = Condition(fieldName.asJson :: "=".asJson :: a.asJson :: Nil)
          def in[A: Encoder](a0: A, aN: A*): Condition =
            Condition(fieldName.asJson :: "in".asJson :: Json.fromValues((a0 :: aN.toList).map(_.asJson)) :: Nil)
        }
      }
    }
    type Condition = Condition.Type

    object Limit extends PosInt("Limit")
    type Limit = Limit.Type

    implicit val decoder: Decoder[Search] = accumulatingDecoder { c =>
      (
        c.downField("model").asAcc[ModelName],
        c.downField("fields").asAcc[List[FieldName]],
        c.downField("conditions").asAcc[List[Condition]],
        c.downField("limit").asAcc[Option[Limit]]
      ).mapN(Search.apply)
    }
  }

  final case class Read(model: ModelName, ids: NonEmptyList[Id], fields: List[FieldName]) extends Action
  object Read {
    implicit val decoder: Decoder[Read] = accumulatingDecoder { c =>
      (
        c.downField("model").asAcc[ModelName],
        c.downField("ids").asAcc[NonEmptyList[Id]],
        c.downField("fields").asAcc[List[FieldName]]
      ).mapN(Read.apply)
    }
  }

  final case class Create(model: ModelName, parameters: NonEmptyList[(FieldName, Json)]) extends Action
  object Create {
    implicit val decoder: Decoder[Create] = accumulatingDecoder { c =>
      (
        c.downField("model").asAcc[ModelName],
        c.downField("parameters").asAcc[NonEmptyMap[FieldName, Json]].map(_.toNel)
      ).mapN(Create.apply)
    }
  }

  final case class Write(model: ModelName, ids: NonEmptyList[Id], updates: NonEmptyList[(FieldName, Json)])
      extends Action
  object Write {
    implicit val decoder: Decoder[Write] = accumulatingDecoder { c =>
      (
        c.downField("model").asAcc[ModelName],
        c.downField("ids").asAcc[NonEmptyList[Id]],
        c.downField("updates").asAcc[NonEmptyList[(FieldName, Json)]]
      ).mapN(Write.apply)
    }
  }

  implicit val decoder: Decoder[Action] = anyOf[Action](
    fixed("server-info").as(ServerInfo),
    fixed("login").as(Login),
    Decoder[Fields].at("fields").widen,
    Decoder[Search].at("search").widen,
    Decoder[Read].at("read").widen,
    Decoder[Write].at("write").widen,
    Decoder[Create].at("create").widen
  )
}
