package peschke.odoo.models

import cats.data.NonEmptyList
import cats.syntax.all._
import com.monovore.decline.Argument
import io.circe.Json
import io.circe.syntax._
import peschke.odoo.models.Action.Fields.Attribute
import peschke.odoo.models.Action.Search.Condition
import peschke.odoo.models.Action.Search.Limit
import peschke.odoo.models.RpcServiceCall.MethodName
import peschke.odoo.models.RpcServiceCall.ServiceName
import peschke.odoo.models.authentication.ApiKey
import peschke.odoo.models.authentication.Database
import peschke.odoo.models.authentication.LoggedIn
import peschke.odoo.models.authentication.Username

sealed trait RpcServiceCall {
  def serviceName: ServiceName
  def methodName: MethodName
  def args: List[Json]
}
object RpcServiceCall       {
  object ServiceName extends NonEmptyString("Service name")

  type ServiceName = ServiceName.Type

  object MethodName extends NonEmptyString("Method name")

  type MethodName = MethodName.Type

  trait CommonService { _: RpcServiceCall =>
    override def serviceName: ServiceName = CommonService.Name
  }

  object CommonService {
    val Name: ServiceName = ServiceName("common")

    case object Version extends RpcServiceCall with CommonService {
      override val methodName: MethodName = MethodName("version")

      override def args: List[Json] = Nil
    }

    final case class Login(database: Database, username: Username, apiKey: ApiKey)
        extends RpcServiceCall
        with CommonService {
      override def methodName: MethodName = Login.Name

      override def args: List[Json] =
        List(Database.raw(database), Username.raw(username), ApiKey.raw(apiKey)).map(_.asJson)
    }

    object Login {
      val Name: MethodName = MethodName("login")
    }
  }

  trait ObjectService { _: RpcServiceCall =>
    override def serviceName: ServiceName = ObjectService.Name

    override def methodName: MethodName = ObjectService.Method

    def loggedIn: LoggedIn

    def extraArgs: List[Json]

    override final def args: List[Json] = loggedIn.addToArgs(extraArgs)
  }

  object ObjectService {
    val Name: ServiceName = ServiceName("object")
    val Method: MethodName = MethodName("execute_kw")

    object Id extends PosInt("Id")
    type Id = Id.Type

    object FieldName extends NonEmptyString("Field name") {
      override implicit val argument: Argument[Type] = Argument.from("field")(fromString(_).toValidatedNel)
    }

    type FieldName = FieldName.Type

    object ModelName extends NonEmptyString("Model name")

    type ModelName = ModelName.Type

    final case class FieldsGet(loggedIn: LoggedIn, modelName: ModelName, attributes: List[Attribute])
        extends RpcServiceCall
        with ObjectService {
      override def extraArgs: List[Json] = List(
        modelName.asJson,
        "fields_get".asJson,
        Json.arr(),
        Json.obj("attributes" := attributes)
      )
    }

    final case class SearchRead
      (loggedIn: LoggedIn,
       modelName: ModelName,
       conditions: List[Condition],
       fields: List[FieldName],
       limitOpt: Option[Limit]
      ) extends RpcServiceCall
        with ObjectService {
      override def extraArgs: List[Json] = List(
        modelName.asJson,
        "search_read".asJson,
        Json.arr(conditions.asJson),
        Json.obj("fields" := fields).deepMerge {
          limitOpt.fold(Json.obj()) { limit =>
            Json.obj("limit" := limit)
          }
        }
      )
    }

    final case class Read(loggedIn: LoggedIn, modelName: ModelName, ids: NonEmptyList[Id], fields: List[FieldName])
        extends RpcServiceCall
        with ObjectService {
      override def extraArgs: List[Json] = List(
        modelName.asJson,
        "read".asJson,
        Json.arr(ids.asJson),
        Json.obj("fields" := fields)
      )
    }

    final case class Write
      (loggedIn: LoggedIn, modelName: ModelName, ids: NonEmptyList[Id], updates: NonEmptyList[(FieldName, Json)])
        extends RpcServiceCall
        with ObjectService {
      override def extraArgs: List[Json] = List(
        modelName.asJson,
        "write".asJson,
        Json.arr(ids.asJson),
        Json.fromFields(updates.map { case (k, v) =>
          FieldName.raw(k) -> v
        }.toList)
      )
    }

    final case class Create(loggedIn: LoggedIn, modelName: ModelName, parameters: NonEmptyList[(FieldName, Json)])
        extends RpcServiceCall
        with ObjectService {
      override def extraArgs: List[Json] = List(
        modelName.asJson,
        "create".asJson,
        Json.arr(parameters.toNem.asJson)
      )
    }
  }
}
