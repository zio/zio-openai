package zio.openai.codegen.model

import io.github.vigoo.metagen.core.ScalaType
import zio.openai.codegen.model.Parameter.{ PathParameter, QueryParameter }

import scala.meta.Term

final case class Endpoint(
  name: String,
  method: HttpMethod,
  pathPattern: String,
  isDeprecated: Boolean,
  parameters: List[Parameter],
  body: Option[RequestBody],
  response: Option[ResponseBody]
) {
  def methodName: Term.Name = Term.Name(name)

  def pathParameters: List[Parameter.PathParameter] =
    parameters.collect { case pp: PathParameter =>
      pp
    }
  def queryParameters: List[Parameter.QueryParameter] =
    parameters.collect { case qp: QueryParameter =>
      qp
    }

  def responseType(model: Model): ScalaType =
    response match {
      case Some(ResponseBody(_, typ)) => typ.scalaType(model)
      case None                       => ScalaType.unit
    }

  def bodyContentTypeAsString: String =
    body match {
      case Some(RequestBody(contentType, _)) => contentType.asString
      case None                              => ContentType.`application/json`.asString
    }

  def hasSingleBodyParameter(model: Model): Option[TypeDefinition.Object] =
    if (parameters.nonEmpty) None
    else
      body.flatMap {
        case RequestBody(_, obj: TypeDefinition.Object) => Some(obj)
        case RequestBody(_, ref: TypeDefinition.Ref)    =>
          model.finalTypes.get(ref.referencedName).collect { case obj: TypeDefinition.Object =>
            obj
          }
        case _                                          => None
      }

  def transformEnums(f: TypeDefinition.Enum => TypeDefinition.Enum): Endpoint =
    copy(
      parameters = parameters.map(_.transformEnums(f)),
      body = body.map(_.transformEnums(f)),
      response = response.map(_.transformEnums(f))
    )
}
