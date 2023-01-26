package zio.openai.codegen.model

import io.github.vigoo.metagen.core.ScalaType
import zio.openai.codegen.model.Parameter.{PathParameter, QueryParameter}

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
      case None => ContentType.`application/json`.asString
    }
}
