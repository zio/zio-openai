package zio.openai.codegen.model

import io.swagger.v3.oas.models.{ OpenAPI, Operation, PathItem }

import scala.jdk.CollectionConverters.*

final case class Model(
  types: Map[String, TypeDefinition],
  apis: List[API]
) {
  lazy val allTypes: Map[String, TypeDefinition] =
    Model.collectReferencedTypes(types.values.toSeq)

  lazy val objects: List[TypeDefinition.Object] =
    allTypes.collect { case (_, o: TypeDefinition.Object) =>
      o
    }.toList

  lazy val alternatives: List[TypeDefinition.Alternatives] =
    allTypes.collect { case (_, a: TypeDefinition.Alternatives) =>
      a
    }.toList

  lazy val enums: List[TypeDefinition.Enum] =
    allTypes.collect { case (_, e: TypeDefinition.Enum) =>
      e
    }.toList
}

object Model {
  def from(openAPI: OpenAPI): Model = {
    val types = openAPI.getComponents.getSchemas.asScala.map { case (name, schema) =>
      name -> TypeDefinition.from(name, schema)
    }.toMap

    val paths: Map[String, PathItem] = openAPI.getPaths.asScala.toMap
    val apis = API.fromPaths(paths)

    Model(types, apis)
  }

  private def collectReferencedTypes(types: Seq[TypeDefinition]): Map[String, TypeDefinition] =
    types.flatMap {
      case obj @ TypeDefinition.Object(name, _, fields)          =>
        Map(name -> obj) ++ collectReferencedTypes(fields.map(_.typ))
      case alt @ TypeDefinition.Alternatives(name, alternatives) =>
        Map(name -> alt) ++ collectReferencedTypes(alternatives)
      case arr @ TypeDefinition.Array(itemType)                  =>
        Map(arr.name -> arr) ++ collectReferencedTypes(Seq(itemType))
      case arr @ TypeDefinition.NonEmptyArray(itemType)          =>
        Map(arr.name -> arr) ++ collectReferencedTypes(Seq(itemType))
      case arr @ TypeDefinition.ConstrainedArray(itemType, _, _) =>
        Map(arr.name -> arr) ++ collectReferencedTypes(Seq(itemType))
      case typ: TypeDefinition                                   =>
        Map(typ.name -> typ)
    }.toMap
}
