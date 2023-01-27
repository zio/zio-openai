package zio.openai.codegen.model

import io.swagger.v3.oas.models.{ OpenAPI, Operation, PathItem }

import scala.jdk.CollectionConverters.*

final case class Model(
  types: Map[String, TypeDefinition],
  apis: List[API]
) {
  private lazy val allTypes: Map[String, TypeDefinition] =
    Model.collectReferencedTypes(types.values.toSeq)

  private lazy val finalTypes: Map[String, TypeDefinition] =
    allTypes.mapValues(_.transformEnums(enumMapping))

  lazy val objects: List[TypeDefinition.Object] =
    finalTypes.collect { case (_, o: TypeDefinition.Object) =>
      o
    }.toList

  lazy val alternatives: List[TypeDefinition.Alternatives] =
    finalTypes.collect { case (_, a: TypeDefinition.Alternatives) =>
      a
    }.toList

  lazy val (enums, enumMapping) =
    unifyEnums(
      allTypes.collect { case (_, e: TypeDefinition.Enum) =>
        e
      }.toList
    )

  private def unifyEnums(
    allEnums: List[TypeDefinition.Enum]
  ): (List[TypeDefinition.Enum], Map[TypeDefinition.Enum, TypeDefinition.Enum]) = {
    val grouped = allEnums.groupBy(enum => (enum.directName, enum.values))
    allEnums.foldLeft(
      (List.empty[TypeDefinition.Enum], Map.empty[TypeDefinition.Enum, TypeDefinition.Enum])
    ) { case ((result, mapping), enum) =>
      val group = grouped((enum.directName, enum.values))
      if (group.size == 1) {
        // This is a unique enum
        (enum :: result, mapping + (enum -> enum))
      } else {
        // This is a duplicate enum
        result.find(other =>
          other.directName == enum.directName && other.values == enum.values
        ) match {
          case Some(existing) =>
            // We already have a unified enum
            (result, mapping + (enum -> existing))
          case None           =>
            // This is the first duplicate enum
            val unified = TypeDefinition.Enum(enum.directName, enum.directName, enum.values)
            (unified :: result, mapping + (enum -> unified))
        }
      }
    }
  }
}

object Model {
  def from(openAPI: OpenAPI): Model = {
    val types = openAPI.getComponents.getSchemas.asScala.map { case (name, schema) =>
      name -> TypeDefinition.from(name, name, schema)
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
