package zio.openai.codegen.model

import io.swagger.v3.oas.models.{ OpenAPI, Operation, PathItem }

import scala.jdk.CollectionConverters.*

final case class Model(
  types: Map[String, TypeDefinition],
  initialAPIs: List[API]
) {
  private lazy val allTypes: Map[String, TypeDefinition] =
    Model.collectReferencedTypes(types.values.toSeq) ++
      Model.collectReferencedTypes(initialAPIs.flatMap(api => api.allTypes))

  lazy val finalTypes: Map[String, TypeDefinition] =
    allTypes.mapValues(_.transform(unifyTypes))

  lazy val apis: List[API] =
    initialAPIs.map(_.transform(unifyTypes))

  lazy val objects: List[TypeDefinition.Object] =
    finalTypes.collect { case (_, o: TypeDefinition.Object) =>
      o
    }.toList

  lazy val dynamicObjects: List[TypeDefinition.DynamicObject] =
    finalTypes.collect { case (_, o: TypeDefinition.DynamicObject) =>
      o
    }.toList

  lazy val alternatives: List[TypeDefinition.Alternatives] =
    finalTypes.collect { case (_, a: TypeDefinition.Alternatives) =>
      a
    }.toList

  lazy val (smartNewTypes, smartNewTypeMapping) =
    unifySmartNewTypes(
      allTypes.collect { case (_, a: TypeDefinition.SmartNewType) =>
        a
      }.toList
    )

  lazy val (enums, enumMapping) =
    unifyEnums(
      allTypes.collect { case (_, e: TypeDefinition.Enum) =>
        e
      }.toList
    )

  private def unifyTypes(typ: TypeDefinition): TypeDefinition =
    typ match {
      case e: TypeDefinition.Enum           =>
        enumMapping(e)
      case snt: TypeDefinition.SmartNewType =>
        smartNewTypeMapping(snt)
      case other                            =>
        other
    }

  private def unifyEnums(
    allEnums: List[TypeDefinition.Enum]
  ): (List[TypeDefinition.Enum], Map[TypeDefinition.Enum, TypeDefinition.Enum]) = {
    val grouped = allEnums.groupBy(enum => enum.directName).mapValues(_.groupBy(enum => enum.values))
    allEnums.foldLeft(
      (List.empty[TypeDefinition.Enum], Map.empty[TypeDefinition.Enum, TypeDefinition.Enum])
    ) { case ((result, mapping), enum) =>
      val nameGroup = grouped(enum.directName)
      val valueGroup = nameGroup(enum.values)
      if (
        nameGroup.size > 1 || // we don't want to unify since the different groups will overwrite each other
        valueGroup.size == 1 ||
        enum.directName == "CaseType0" || // We don't want to move these generated cases into top level
        enum.directName == "CaseType1" ||
        enum.directName == "CaseType2" ||
        enum.directName == "CaseType3" ||
        enum.directName == "CaseType4" ||
        enum.directName == "CaseType5" ||
        enum.directName == "CaseType6" ||
        enum.directName == "CaseType7"
      ) {
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
            val unified = TypeDefinition.Enum(enum.directName, None, enum.values, enum.description)
            (unified :: result, mapping + (enum -> unified))
        }
      }
    }
  }

  // TODO: merge these unify functions
  private def unifySmartNewTypes(all: List[TypeDefinition.SmartNewType]): (
    List[TypeDefinition.SmartNewType],
    Map[TypeDefinition.SmartNewType, TypeDefinition.SmartNewType]
  ) = {
    val grouped = all.groupBy(typ => typ.withoutParent)
    all.foldLeft(
      (
        List.empty[TypeDefinition.SmartNewType],
        Map.empty[TypeDefinition.SmartNewType, TypeDefinition.SmartNewType]
      )
    ) { case ((result, mapping), typ) =>
      val group = grouped(typ.withoutParent)
      if (group.size == 1) {
        // This is a unique enum
        (typ :: result, mapping + (typ -> typ))
      } else {
        // This is a duplicate enum
        result.find(other => other.withoutParent == typ.withoutParent) match {
          case Some(existing) =>
            // We already have a unified enum
            (result, mapping + (typ -> existing))
          case None           =>
            // This is the first duplicate enum
            val unified = typ.withoutParent
            (unified :: result, mapping + (typ -> unified))
        }
      }
    }
  }

  def resolve(typ: TypeDefinition): TypeDefinition =
    typ match {
      case ref @ TypeDefinition.Ref(_) =>
        finalTypes(ref.referencedName)
      case other                       =>
        other
    }
}

object Model {
  def from(openAPI: OpenAPI): Model = {
    val types = openAPI.getComponents.getSchemas.asScala.map { case (name, schema) =>
      name -> TypeDefinition.from(ParentChain.empty, name, schema)
    }.toMap

    val paths: Map[String, PathItem] = openAPI.getPaths.asScala.toMap
    val apis = API.fromPaths(paths)

    Model(types, apis)
  }

  private def collectReferencedTypes(types: Seq[TypeDefinition]): Map[String, TypeDefinition] =
    types.flatMap {
      case obj @ TypeDefinition.Object(_, _, _, fields)                =>
        Map(obj.name -> obj) ++ collectReferencedTypes(fields.map(_.typ))
      case alt @ TypeDefinition.Alternatives(_, _, alternatives, _, _) =>
        Map(alt.name -> alt) ++ collectReferencedTypes(alternatives)
      case arr @ TypeDefinition.Array(itemType)                        =>
        Map(arr.name -> arr) ++ collectReferencedTypes(Seq(itemType))
      case arr @ TypeDefinition.NonEmptyArray(itemType)                =>
        Map(arr.name -> arr) ++ collectReferencedTypes(Seq(itemType))
      case arr @ TypeDefinition.ConstrainedArray(itemType, _, _)       =>
        Map(arr.name -> arr) ++ collectReferencedTypes(Seq(itemType))
      case TypeDefinition.Ref(_)                                       =>
        Map.empty[String, TypeDefinition]
      case typ: TypeDefinition                                         =>
        Map(typ.name -> typ)
    }.toMap
}
