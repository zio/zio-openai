package zio.openai.codegen.model

import io.github.vigoo.metagen.core.ScalaType
import zio.openai.codegen.generator.Types
import zio.openai.codegen.model.Parameter.{ PathParameter, QueryParameter }

import scala.meta._

final case class Endpoint(
  name: String,
  method: HttpMethod,
  pathPattern: String,
  isDeprecated: Boolean,
  parameters: List[Parameter],
  body: Option[RequestBody],
  response: Option[ResponseBody],
  summary: Option[String]
) {
  def methodName: Term.Name = Term.Name(name)
  def methodNameStreaming: Term.Name = Term.Name(name + "Streaming")

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
      case Some(ResponseBody(_, typ)) => typ.scalaType(model, UseCase.Response)
      case None                       => ScalaType.unit
    }

  def streamingResponseType(model: Model): ScalaType =
    name match {
      case "createChatCompletion" =>
        // NOTE: not defined properly in the OpenAPI spec
        model.finalTypes("CreateChatCompletionStreamResponse").scalaType(model, UseCase.Response)
      case "createCompletion"     =>
        model.finalTypes("CreateCompletionResponse").scalaType(model, UseCase.Response)
      case _                      =>
        responseType(model)
    }

  def bodyContentTypeAsString: String =
    body match {
      case Some(RequestBody(contentType, _)) => contentType.asString
      case None                              => ContentType.`application/json`.asString
    }

  def bodyContentTypeAsMediaType =
    body match {
      case Some(RequestBody(contentType, _)) => contentType.asMediaType
      case None                              => q"${Types.zhttpMediaType.term}.application.json"
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

  def hasStreamingOverride(model: Model): Boolean =
    hasSingleBodyParameter(model).exists { obj =>
      obj.fields.exists(_.controlsStreamingResponse)
    }

  def transform(f: TypeDefinition => TypeDefinition): Endpoint =
    copy(
      parameters = parameters.map(_.transform(f)),
      body = body.map(_.transform(f)),
      response = response.map(_.transform(f))
    )

  def allTypes: Set[TypeDefinition] = {
    val params = parameters.map(_.typ).toSet
    val bodyTypes = body.map(_.typ).toSet
    val responseTypes = response.map(_.typ).toSet
    params ++ bodyTypes ++ responseTypes
  }
}
