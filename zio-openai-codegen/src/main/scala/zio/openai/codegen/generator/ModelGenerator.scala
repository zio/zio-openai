package zio.openai.codegen.generator

import io.github.vigoo.metagen.core.*
import zio.ZIO
import zio.nio.file.Path
import zio.openai.codegen.model.{ Field, Model, TypeDefinition }

import scala.meta.*

// TODO: add scaladoc support to metagen and use the description field
// TODO: verify that non-required vs required-nullable works properly

trait ModelGenerator { this: HasParameters =>
  def generateModels(
    model: Model
  ): ZIO[Any, GeneratorFailure[OpenAIGeneratorFailure], Set[Path]] = {
    val generate =
      for {
        _        <- Generator.setRoot(parameters.targetRoot)
        _        <- Generator.setScalaVersion(parameters.scalaVersion)
        objs     <- ZIO.foreach(model.objects.filter(_.isTopLevel)) { obj =>
                      Generator.generateScalaPackage[Any, OpenAIGeneratorFailure](
                        Packages.models,
                        obj.scalaName
                      )(generateTopLevelObjectClass(model, obj))
                    }
        dynObjs  <- ZIO.foreach(model.dynamicObjects.filter(_.isTopLevel)) { dynObj =>
                      Generator.generateScalaPackage[Any, OpenAIGeneratorFailure](
                        Packages.models,
                        dynObj.scalaName
                      )(generateTopLevelDynamicObjectClass(model, dynObj))
                    }
        alts     <- ZIO.foreach(model.alternatives.filter(_.isTopLevel)) { alt =>
                      Generator.generateScalaPackage[Any, OpenAIGeneratorFailure](
                        Packages.models,
                        alt.scalaName
                      )(generateTopLevelAlternativesTrait(model, alt))
                    }
        enums    <- ZIO.foreach(model.enums.filter(_.isTopLevel)) { enum =>
                      Generator.generateScalaPackage[Any, OpenAIGeneratorFailure](
                        Packages.models,
                        enum.scalaName
                      )(generateTopLevelEnumTrait(model, enum))
                    }
        newtypes <-
          Generator.generateScalaPackageObject[Any, OpenAIGeneratorFailure](
            Packages.openai,
            "model"
          )(generateTopLevelSmartNewTypes(model, model.smartNewTypes.filter(_.isTopLevel)))

      } yield objs.toSet ++ dynObjs.toSet ++ alts.toSet ++ enums.toSet ++ Set(newtypes)

    generate.provide(
      Generator.live
    )
  }

  protected def getObjectFieldsAsParams(model: Model, fields: List[Field], allowDefaults: Boolean) =
    fields.map { field =>
      val fieldType = field.typ.scalaType(model)
      val fieldName = field.scalaNameTerm

      if (field.isNullable || !field.isRequired) {
        val p = param"$fieldName: ${Types.optional(fieldType).typ}"
        if (!field.isRequired && allowDefaults)
          param"$fieldName: ${Types.optional(fieldType).typ} = ${Types.optionalAbsent.term}"
        else
          param"$fieldName: ${Types.optional(fieldType).typ}"
      } else
        param"$fieldName: ${fieldType.typ}"
    }

  private def generateTopLevelObjectClass(
    model: Model,
    obj: TypeDefinition.Object
  ): ZIO[CodeFileGenerator, OpenAIGeneratorFailure, Term.Block] =
    generateObjectClass(model, obj).map { cls =>
      q"""
     import zio.openai.internal.{jsonObjectSchema, nonEmptyChunkSchema, optionalSchema}

     ..$cls
     """
    }

  private def generateObjectClass(
    model: Model,
    obj: TypeDefinition.Object
  ): ZIO[CodeFileGenerator, OpenAIGeneratorFailure, List[Stat]] = {

    val typ = obj.scalaType(model)

    val fields = getObjectFieldsAsParams(model, obj.fields, allowDefaults = true)
    val fieldsWithoutDefaults = getObjectFieldsAsParams(model, obj.fields, allowDefaults = false)

    val caseClassName = ScalaType(Packages.zioSchema / "Schema", s"CaseClass${fields.size}")

    val fieldSchemas = obj.fields.map { field =>
      val fieldType = field.typ.scalaType(model)

      if (field.isNullable || !field.isRequired)
        q"""${Types.schemaField.term}(
            ${Lit.String(field.name)},
            ${Types.schema_.term}[${Types.optional(fieldType).typ}],
            get0 = obj => obj.${field.scalaNameTerm},
            set0 = (obj: ${typ.typ}, v: ${Types
          .optional(fieldType)
          .typ}) => obj.copy(${field.scalaNameTerm} = v)
       )"""
      else
        q"""${Types.schemaField.term}(
            ${Lit.String(field.name)},
            ${Types.schema_.term}[${fieldType.typ}],
            get0 = obj => obj.${field.scalaNameTerm},
            set0 = (obj: ${typ.typ}, v: ${fieldType.typ}) => obj.copy(${field.scalaNameTerm} = v)
       )"""
    }
    val schema =
      q"""${caseClassName.term}(
         ${Types.typeId.term}.parse(${Lit.String(typ.asString)}),
         ..$fieldSchemas,
         (..$fieldsWithoutDefaults) => ${typ.termName}(..${obj.fields.map(field =>
        field.scalaNameTerm
      )})
       )"""

    ZIO
      .foreach(obj.children(model)) {
        case child: TypeDefinition.Object        =>
          generateObjectClass(model, child)
        case child: TypeDefinition.Alternatives  =>
          generateAlternativesTrait(model, child)
        case child: TypeDefinition.Enum          =>
          generateEnumTrait(model, child)
        case child: TypeDefinition.DynamicObject =>
          generateDynamicObjectClass(model, child)
        case child: TypeDefinition.SmartNewType  =>
          generateSmartNewType(model, child)
        case _                                   =>
          ZIO.fail(OpenAIGeneratorFailure.UnsupportedChildType)
      }
      .map { children =>
        List[Stat](
          q"""final case class ${typ.typName}(..$fields)""",
          q"""
              object ${typ.termName} {
                implicit val schema: ${Types.schemaOf(typ).typ} = $schema

                ..${children.flatten}
              }
          """
        )
      }
  }

  private def generateTopLevelDynamicObjectClass(
    model: Model,
    obj: TypeDefinition.DynamicObject
  ): ZIO[CodeFileGenerator, OpenAIGeneratorFailure, Term.Block] =
    generateDynamicObjectClass(model, obj).map { cls =>
      q"""
     import zio.openai.internal.{jsonObjectSchema, nonEmptyChunkSchema, optionalSchema}

     ..$cls
     """
    }

  private def generateDynamicObjectClass(
    model: Model,
    obj: TypeDefinition.DynamicObject
  ): ZIO[CodeFileGenerator, OpenAIGeneratorFailure, List[Stat]] = {
    val typ = obj.scalaType(model)

    val knownFieldParams = getObjectFieldsAsParams(model, obj.knownFields, allowDefaults = true)
    val knownFieldGetters =
      obj.knownFields.map { field =>
        val fieldType = field.typ.scalaType(model)
        val fieldName = field.scalaNameTerm

        q"""def ${fieldName}: ${Types.optional(fieldType).typ} =
             values.get(${Lit.String(field.name)}).flatMap(_.as[${fieldType.typ}].toOption)
         """
      }

    val schema =
      q"""
         implicit lazy val schema: ${Types.schemaOf(typ).typ} =
           jsonObjectSchema.transform(${typ.term}.apply, _.values)
       """

    ZIO
      .foreach(obj.children(model)) {
        case child: TypeDefinition.Object        =>
          generateObjectClass(model, child)
        case child: TypeDefinition.Alternatives  =>
          generateAlternativesTrait(model, child)
        case child: TypeDefinition.Enum          =>
          generateEnumTrait(model, child)
        case child: TypeDefinition.DynamicObject =>
          generateDynamicObjectClass(model, child)
        case _                                   =>
          ZIO.fail(OpenAIGeneratorFailure.UnsupportedChildType)
      }
      .map { children =>
        List[Stat](
          q"""final case class ${typ.typName}(values: ${ScalaType
            .map(ScalaType.string, Types.json)
            .typ})
                    extends ${Types.dynamicObjectOf(typ).init} {

                override protected def updateValues(updated: Map[String, Json]): ${typ.typ} =
                  copy(values = updated)

                ..$knownFieldGetters
              }
            """,
          q"""
          object ${typ.termName} {
            def apply(..$knownFieldParams): ${typ.typ} = {
              import _root_.zio.json._
              ${typ.termName}(List(..${obj.knownFields.map(field =>
            q"${field.scalaNameTerm}.flatMap(value => value.toJsonAST.toOption.map(json => ${Lit
              .String(field.name)} -> json)).toOption"
          )}).flatten.toMap)
            }

            $schema
            ..${children.flatten}
          }
      """
        )
      }
  }

  private def generateTopLevelAlternativesTrait(
    model: Model,
    alt: TypeDefinition.Alternatives
  ): ZIO[CodeFileGenerator, OpenAIGeneratorFailure, Term.Block] =
    generateAlternativesTrait(model, alt).map { cls =>
      q"""
     import zio.openai.internal.{jsonObjectSchema, nonEmptyChunkSchema, optionalSchema}

     ..$cls
     """
    }

  private def generateAlternativesTrait(
    model: Model,
    alt: TypeDefinition.Alternatives
  ): ZIO[CodeFileGenerator, OpenAIGeneratorFailure, List[Stat]] = {

    val typ = alt.scalaType(model)

    val cases: List[Defn] =
      alt.constructors(model).flatMap { case (altCons, alt) =>
        val altType = alt.scalaType(model)
        List(
          q"""
            final case class ${altCons.typName}(value: ${altType.typ}) extends ${typ.init}
            """,
          q"""
            object ${altCons.termName} {
              lazy val schema: Schema[${altCons.typ}] =
                ${Types.schema_.term}[${altType.typ}]
                  .transform(
                    ${altCons.term}.apply,
                    _.value
                  )

              lazy val schemaCase: ${Types.schemaCaseOf(typ, altCons).typ} =
                ${Types.schemaCase_.term}(
                  ${Lit.String(altCons.name)},
                  schema,
                  _.asInstanceOf[${altCons.typ}],
                  _.asInstanceOf[${typ.typ}],
                  _.isInstanceOf[${altCons.typ}]
                )
            }
         """
        )
      }

    val schemaCases = alt.constructors(model).map { case (altCons, alt) =>
      q"""${altCons.term}.schemaCase"""
    }

    val caseSetChain =
      schemaCases.tail.foldLeft[Term](
        q"${Types.caseSetCons.term}(${schemaCases.head}, ${Types.caseSetEmpty.term}[${typ.typ}]())"
      ) { case (prev, c) =>
        q"$prev.:+:($c)"
      }

    ZIO.succeed {
      List[Stat](
        q"""sealed trait ${typ.typName}""",
        q"""
            object ${typ.termName} {

            implicit lazy val schema: ${Types.schemaOf(typ).typ} =
              ${Types.schemaEnumN.term}(
                ${Types.typeId.term}.parse(${Lit.String(typ.asString)}),
                $caseSetChain
              )

           ..$cases
          }
         """
      )
    }
  }

  private def generateTopLevelEnumTrait(
    model: Model,
    enum: TypeDefinition.Enum
  ): ZIO[CodeFileGenerator, OpenAIGeneratorFailure, Term.Block] =
    generateEnumTrait(model, enum).map { cls =>
      q"""
     import zio.openai.internal.{jsonObjectSchema, nonEmptyChunkSchema, optionalSchema}

     ..$cls
     """
    }

  private def generateEnumTrait(
    model: Model,
    enum: TypeDefinition.Enum
  ): ZIO[CodeFileGenerator, OpenAIGeneratorFailure, List[Stat]] = {

    val typ = enum.scalaType(model)

    val cases: List[Defn] =
      enum.values.map { value =>
        val name = Term.Name(value.capitalize)
        q"""
          final case object $name extends ${typ.init}
         """
      }

    val fromStringMatches =
      enum.values.map { value =>
        p"case ${Lit.String(value)} => ${Types.eitherRight.term}[${ScalaType.string.typ}, ${typ.typ}](${Term
          .Name(value.capitalize)})"
      } :+ p"case other => ${Types.eitherLeft.term}[${ScalaType.string.typ}, ${typ.typ}](${Lit.String("Invalid value: ")} + other)"
    val fromString = q"(s: ${ScalaType.string.typ}) => s match { ..case $fromStringMatches }"

    val toStringMatches =
      enum.values.map { value =>
        p"case ${Term.Name(
          value.capitalize
        )} => ${Types.eitherRight.term}[${ScalaType.string.typ}, ${ScalaType.string.typ}](${Lit.String(value)})"
      }
    val toString = q"(s: ${typ.typ}) => s match { ..case $toStringMatches }"

    ZIO.succeed {
      List[Stat](
        q"""sealed trait ${typ.typName}""",
        q"""
          object ${typ.termName} {

            implicit lazy val schema: ${Types.schemaOf(typ).typ} =
              ${Types.schema_.term}[${ScalaType.string.typ}].transformOrFail(
                  $fromString,
                  $toString
              )

           ..$cases
          }
         """
      )
    }
  }

  def generateTopLevelSmartNewTypes(
    model: Model,
    smartNewTypes: List[TypeDefinition.SmartNewType]
  ): ZIO[CodeFileGenerator, OpenAIGeneratorFailure, Term.Block] =
    ZIO
      .foreach(smartNewTypes) { smartNewType =>
        generateSmartNewType(model, smartNewType)
      }
      .map(_.flatten)
      .map(Term.Block(_))

  private def generateSmartNewType(
    model: Model,
    smartNewType: TypeDefinition.SmartNewType
  ): ZIO[CodeFileGenerator, OpenAIGeneratorFailure, List[Stat]] = {

    val typ = smartNewType.scalaType(model)
    val sup =
      smartNewType match {
        case _: TypeDefinition.ConstrainedInteger =>
          ScalaType.int
        case _: TypeDefinition.ConstrainedNumber  =>
          ScalaType.double
        case _: TypeDefinition.ConstrainedString  =>
          ScalaType.string
      }

    val assertion =
      smartNewType match {
        case TypeDefinition.ConstrainedInteger(directName, parentName, min, max) =>
          q"""greaterThanOrEqualTo(${Lit.Int(min)}) && lessThanOrEqualTo(${Lit.Int(max)})"""
        case TypeDefinition.ConstrainedNumber(directName, parentName, min, max)  =>
          q"""greaterThanOrEqualTo(${Lit.Double(min)}) && lessThanOrEqualTo(${Lit.Double(max)})"""
        case TypeDefinition.ConstrainedString(directName, parentName, min, max)  =>
          q"""hasLength(greaterThanOrEqualTo(${Lit.Int(min)}) && lessThanOrEqualTo(${Lit.Int(
            max
          )}))"""
      }

    val mods: List[Mod] =
      if (parameters.scalaVersion.startsWith("3"))
        List(Mod.Override(), Mod.Inline())
      else
        List(Mod.Override())

    val wrappedAssertion =
      if (parameters.scalaVersion.startsWith("3"))
        assertion
      else
        q"""assert { $assertion }"""

    ZIO.succeed {
      List(
        q"""object ${typ.termName} extends ${Types.subtypeOf(sup).init} {
            import zio.prelude.Assertion._
            ..$mods def assertion = $wrappedAssertion

            implicit val schema: ${Types.schemaOf(typ).typ} =
              ${Types.schema_.term}[${sup.typ}].transform(wrap(_), unwrap)

          }
       """,
        Defn.Type(
          Nil,
          typ.typName,
          Type.ParamClause(Nil),
          Type.Select(typ.term, Type.Name("Type")),
          Type.Bounds(None, None)
        )
      )
    }
  }
}
