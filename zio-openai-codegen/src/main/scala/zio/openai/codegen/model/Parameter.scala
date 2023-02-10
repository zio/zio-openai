package zio.openai.codegen.model

import io.swagger.v3.oas.models.parameters
import io.swagger.v3.oas.models.parameters.{
  CookieParameter,
  HeaderParameter,
  Parameter as OpenAPIParameter
}

import scala.meta.Term

sealed trait Parameter {
  val name: String
  val typ: TypeDefinition
  val isRequired: Boolean
  val description: Option[String]

  def paramName: Term.Name = Term.Name(name)

  def transform(f: TypeDefinition => TypeDefinition): Parameter
}

object Parameter {
  final case class PathParameter(
    name: String,
    description: Option[String],
    typ: TypeDefinition,
    isRequired: Boolean
  ) extends Parameter {
    def transform(f: TypeDefinition => TypeDefinition): PathParameter =
      copy(typ = typ.transform(f))
  }

  final case class QueryParameter(
    name: String,
    description: Option[String],
    typ: TypeDefinition,
    isRequired: Boolean
  ) extends Parameter {
    def transform(f: TypeDefinition => TypeDefinition): QueryParameter =
      copy(typ = typ.transform(f))
  }

  def from(parent: String, param: OpenAPIParameter): Parameter =
    param match {
      case pathParam: parameters.PathParameter   =>
        val name = pathParam.getName
        PathParameter(
          name,
          Option(pathParam.getDescription),
          TypeDefinition.from(
            ParentChain.empty,
            parent + "_" + name,
            pathParam.getSchema
          ), // TODO: grouping?
          Option(pathParam.getRequired).exists(_.booleanValue())
        )
      case queryParam: parameters.QueryParameter =>
        val name = queryParam.getName
        QueryParameter(
          name,
          Option(queryParam.getDescription),
          TypeDefinition.from(
            ParentChain.empty,
            parent + "_" + name,
            queryParam.getSchema
          ), // TODO: grouping?
          Option(queryParam.getRequired).exists(_.booleanValue())
        )
      case _                                     =>
        throw new IllegalArgumentException(s"Unsupported parameter type: ${param.getClass.getName}")
    }
}
