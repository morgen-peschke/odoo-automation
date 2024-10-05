package peschke.odoo.algebras

import cats.Applicative
import cats.syntax.all._
import peschke.odoo.AppConfig.AuthConfig
import peschke.odoo.models.Action
import peschke.odoo.models.RpcServiceCall
import peschke.odoo.models.RpcServiceCall.CommonService
import peschke.odoo.models.RpcServiceCall.ObjectService

trait ServiceCallBuilder[F[_]] {
  def fromAction(action: Action): F[RpcServiceCall]
}
object ServiceCallBuilder      {
  def apply[F[_]](implicit SCB: ServiceCallBuilder[F]): SCB.type = SCB

  def default[F[_]: Applicative: LoginManager](authConfig: AuthConfig): ServiceCallBuilder[F] = {
    case Action.ServerInfo => CommonService.Version.pure[F].widen

    case Action.Login =>
      CommonService.Login(authConfig.database, authConfig.username, authConfig.apiKey).pure[F].widen

    case Action.Fields(model, attributes) =>
      LoginManager[F]
        .getCachedLogin
        .map(ObjectService.FieldsGet(_, model, attributes.fold(identity, _.attributes)))

    case Action.Search(model, fields, conditions, limitOpt) =>
      LoginManager[F]
        .getCachedLogin
        .map(ObjectService.SearchRead(_, model, conditions, fields, limitOpt))

    case Action.Read(model, ids, fields) =>
      LoginManager[F]
        .getCachedLogin
        .map(ObjectService.Read(_, model, ids, fields))

    case Action.Write(model, ids, updates) =>
      LoginManager[F]
        .getCachedLogin
        .map(ObjectService.Write(_, model, ids, updates))

    case Action.Create(model, parameters) =>
      LoginManager[F]
        .getCachedLogin
        .map(ObjectService.Create(_, model, parameters))
  }
}
