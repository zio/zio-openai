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

  def paramName: Term.Name = Term.Name(name)

  def transformEnums(f: TypeDefinition.Enum => TypeDefinition.Enum): Parameter
}

object Parameter {
  final case class PathParameter(
    name: String,
    description: Option[String],
    typ: TypeDefinition,
    isRequired: Boolean
  ) extends Parameter {
    def transformEnums(f: TypeDefinition.Enum => TypeDefinition.Enum): PathParameter =
      copy(typ = typ.transformEnums(f))
  }

  final case class QueryParameter(
    name: String,
    description: Option[String],
    typ: TypeDefinition,
    isRequired: Boolean
  ) extends Parameter {
    def transformEnums(f: TypeDefinition.Enum => TypeDefinition.Enum): QueryParameter =
      copy(typ = typ.transformEnums(f))
  }

  def from(parent: String, param: OpenAPIParameter): Parameter =
    param match {
      case pathParam: parameters.PathParameter   =>
        val name = pathParam.getName
        PathParameter(
          name,
          Option(pathParam.getDescription),
          TypeDefinition.from(parent + "_" + name, name, pathParam.getSchema),
          Option(pathParam.getRequired).exists(_.booleanValue())
        )
      case queryParam: parameters.QueryParameter =>
        val name = queryParam.getName
        QueryParameter(
          name,
          Option(queryParam.getDescription),
          TypeDefinition.from(parent + "_" + name, name, queryParam.getSchema),
          Option(queryParam.getRequired).exists(_.booleanValue())
        )
      case _                                     =>
        throw new IllegalArgumentException(s"Unsupported parameter type: ${param.getClass.getName}")
    }
}
