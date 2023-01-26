package zio.openai.codegen.generator

import io.github.vigoo.metagen.core.*
import zio.ZIO
import zio.nio.file.Path
import zio.openai.codegen.model.{ Model, TypeDefinition }

import scala.meta.*

// TODO: collect duplicate enums and generate a single top level type from them
// TODO: snake case to pascal case
// TODO: generate alternatives to their parent's companion objects with shorter name
// TODO: constrained types should be mapped to zio-prelude newtypes
// TODO: add scaladoc support to metagen and use the description field
// TODO: better case names than CaseN
// TODO: field descriptions

trait ModelGenerator { this: HasParameters =>
  def generateModels(
    model: Model
  ): ZIO[Any, GeneratorFailure[OpenAIGeneratorFailure], Set[Path]] = {
    val generate =
      for {
        _     <- Generator.setRoot(parameters.targetRoot)
        _     <- Generator.setScalaVersion(parameters.scalaVersion)
        objs  <- ZIO.foreach(model.objects) { obj =>
                   Generator.generateScalaPackage[Any, OpenAIGeneratorFailure](
                     Packages.models,
                     obj.name
                   )(generateObjectClass(model, obj))
                 }
        alts  <- ZIO.foreach(model.alternatives) { alt =>
                   Generator.generateScalaPackage[Any, OpenAIGeneratorFailure](
                     Packages.models,
                     alt.name
                   )(generateAlternativesTrait(model, alt))
                 }
        enums <- ZIO.foreach(model.enums) { enum =>
                   Generator.generateScalaPackage[Any, OpenAIGeneratorFailure](
                     Packages.models,
                     enum.name
                   )(generateEnumTrait(model, enum))
                 }
      } yield objs.toSet ++ alts.toSet ++ enums.toSet

    generate.provide(
      Generator.live
    )
  }

  private def generateObjectClass(
    model: Model,
    obj: TypeDefinition.Object
  ): ZIO[CodeFileGenerator, OpenAIGeneratorFailure, Term.Block] = {

    val typ = obj.scalaType(model)

    val fields =
      obj.fields.map { field =>
        val fieldType = field.typ.scalaType(model)
        val fieldName = Term.Name(field.name)

        if (field.isNullable || !field.isRequired)
          param"$fieldName: ${ScalaType.option(fieldType).typ}"
        else
          param"$fieldName: ${fieldType.typ}"
      }

    val caseClassName = ScalaType(Packages.zioSchema / "Schema", s"CaseClass${fields.size}")

    val fieldSchemas = obj.fields.map { field =>
      val fieldType = field.typ.scalaType(model)

      if (field.isNullable || !field.isRequired)
        q"""${Types.schemaField.term}(
            ${Lit.String(field.name)},
            ${Types.schema_.term}[${ScalaType.option(fieldType).typ}],
            get0 = obj => obj.${Term.Name(field.name)},
            set0 = (obj: ${typ.typ}, v: ${ScalaType.option(fieldType).typ}) => obj.copy(${Term.Name(
          field.name
        )} = v)
       )"""
      else
        q"""${Types.schemaField.term}(
            ${Lit.String(field.name)},
            ${Types.schema_.term}[${fieldType.typ}],
            get0 = obj => obj.${Term.Name(field.name)},
            set0 = (obj: ${typ.typ}, v: ${fieldType.typ}) => obj.copy(${Term.Name(field.name)} = v)
       )"""
    }
    val schema =
      q"""${caseClassName.term}(
         ${Types.typeId.term}.parse(${Lit.String(typ.asString)}),
         ..$fieldSchemas,
         (..$fields) => ${typ.termName}(..${obj.fields.map(field => Term.Name(field.name))})
       )"""

    ZIO.succeed {
      q"""
        import zio.openai.model.nonEmptyChunkSchema

        final case class ${typ.typName}(..$fields)

        object ${typ.termName} {
          implicit val schema: ${Types.schemaOf(typ).typ} = $schema
        }
     """
    }
  }

  private def generateAlternativesTrait(
    model: Model,
    alt: TypeDefinition.Alternatives
  ): ZIO[CodeFileGenerator, OpenAIGeneratorFailure, Term.Block] = {

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
      q"""
        import zio.openai.model.nonEmptyChunkSchema

        sealed trait ${typ.typName}

        object ${typ.termName} {

          implicit lazy val schema: ${Types.schemaOf(typ).typ} =
            ${Types.schemaEnumN.term}(
              ${Types.typeId.term}.parse(${Lit.String(typ.asString)}),
              $caseSetChain
            )

         ..$cases
        }
       """
    }
  }

  private def generateEnumTrait(
    model: Model,
    enum: TypeDefinition.Enum
  ): ZIO[CodeFileGenerator, OpenAIGeneratorFailure, Term.Block] = {

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
      q"""
        sealed trait ${typ.typName}
  
        object ${typ.termName} {

          implicit lazy val schema: ${Types.schemaOf(typ).typ} =
            ${Types.schema_.term}[${ScalaType.string.typ}].transformOrFail(
                $fromString,
                $toString
            )

         ..$cases
        }
       """
    }
  }
}
