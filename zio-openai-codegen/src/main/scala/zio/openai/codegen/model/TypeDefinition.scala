package zio.openai.codegen.model

import io.github.vigoo.metagen.core.ScalaType
import io.swagger.v3.oas.models.media.Schema
import zio.openai.codegen.generator.{ Packages, Types }
import zio.openai.codegen.model

import scala.jdk.CollectionConverters.*

sealed trait TypeDefinition {
  val name: String
  val description: Option[String]

  def scalaType(model: Model): ScalaType
}

object TypeDefinition {
  final case class Object(name: String, description: Option[String], fields: List[Field])
      extends TypeDefinition {

    override def scalaType(model: Model): ScalaType =
      ScalaType(Packages.models, name)
  }

  final case object PrimitiveBoolean extends TypeDefinition {
    override val name: String = "boolean"
    override val description: Option[String] = None

    override def scalaType(model: Model): ScalaType =
      ScalaType.boolean
  }

  final case object PrimitiveString extends TypeDefinition {
    override val name: String = "string"
    override val description: Option[String] = None

    override def scalaType(model: Model): ScalaType =
      ScalaType.string
  }

  final case class ConstrainedString(minLength: Int, maxLength: Int) extends TypeDefinition {
    override val name: String = "string"
    override val description: Option[String] = None

    override def scalaType(model: Model): ScalaType =
      ScalaType.string // TODO
  }

  final case object Binary extends TypeDefinition {
    override val name: String = "binary"
    override val description: Option[String] = None

    override def scalaType(model: Model): ScalaType =
      Types.chunkOf(ScalaType.byte)
  }

  final case object PrimitiveInteger extends TypeDefinition {
    override val name: String = "integer"
    override val description: Option[String] = None

    override def scalaType(model: Model): ScalaType =
      ScalaType.int
  }

  final case class ConstrainedInteger(min: Int, max: Int) extends TypeDefinition {
    override val name: String = "integer"
    override val description: Option[String] = None

    override def scalaType(model: Model): ScalaType =
      ScalaType.int // TODO
  }

  final case object PrimitiveNumber extends TypeDefinition {
    override val name: String = "number"
    override val description: Option[String] = None

    override def scalaType(model: Model): ScalaType =
      ScalaType.double
  }

  final case class ConstrainedNumber(min: Double, max: Double) extends TypeDefinition {
    override val name: String = "number"
    override val description: Option[String] = None

    override def scalaType(model: Model): ScalaType =
      ScalaType.double // TODO
  }

  final case class Array(itemType: TypeDefinition) extends TypeDefinition {
    override val name: String = "array"
    override val description: Option[String] = None

    override def scalaType(model: Model): ScalaType =
      Types.chunkOf(itemType.scalaType(model))
  }

  final case class NonEmptyArray(itemType: TypeDefinition) extends TypeDefinition {
    override val name: String = "array"
    override val description: Option[String] = None

    override def scalaType(model: Model): ScalaType =
      Types.nonEmptyChunkOf(itemType.scalaType(model))
  }

  final case class ConstrainedArray(itemType: TypeDefinition, min: Int, max: Int)
      extends TypeDefinition {
    override val name: String = "array"
    override val description: Option[String] = None

    override def scalaType(model: Model): ScalaType =
      Types.chunkOf(itemType.scalaType(model)) // TODO
  }

  final case class Alternatives(name: String, alternatives: List[TypeDefinition])
      extends TypeDefinition {
    override val description: Option[String] = None

    override def scalaType(model: Model): ScalaType =
      ScalaType(Packages.models, name)

    def constructors(model: Model): List[(ScalaType, TypeDefinition)] = {
      val typ = scalaType(model)
      alternatives.zipWithIndex.map { case (alt, idx) =>
        typ / s"Case$idx" -> alt
      }
    }
  }

  final case class Enum(name: String, values: List[String]) extends TypeDefinition {
    override val description: Option[String] = None

    override def scalaType(model: Model): ScalaType =
      ScalaType(Packages.models, name)
  }

  final case class Ref(name: String) extends TypeDefinition {
    override val description: Option[String] = None

    override def scalaType(model: Model): ScalaType = {
      val referencedName = name.stripPrefix("#/components/schemas/")
      model.types(referencedName).scalaType(model)
    }
  }

  def from(name: String, schema: Schema[?]): TypeDefinition =
    Option(schema.get$ref()) match {
      case Some(ref) => Ref(ref)
      case None      =>
        val oneOf = Option(schema.getOneOf).map(_.asScala).getOrElse(List.empty)

        if (oneOf.nonEmpty) {
          Alternatives(
            name,
            oneOf.zipWithIndex.map { case (schema, idx) =>
              TypeDefinition.from(name + "_case" + idx, schema)
            }.toList
          )
        } else {
          Option(schema.getType).getOrElse("object") match {
            case "object" =>
              val props = Option(schema.getProperties).map(_.asScala).getOrElse(Map.empty)
              val reqd = Option(schema.getRequired).map(_.asScala).getOrElse(List.empty)

              Object(
                name,
                Option(schema.getDescription),
                props.map { case (fieldName, fieldSchema) =>
                  Field(
                    fieldName,
                    TypeDefinition.from(name + "_" + fieldName, fieldSchema),
                    reqd.contains(fieldName),
                    Option(fieldSchema.getNullable).exists(_.booleanValue())
                  )
                }.toList
              )

            case "boolean" =>
              PrimitiveBoolean

            case "string" =>
              val format = Option(schema.getFormat).getOrElse("")

              if (format == "binary") {
                Binary
              } else {
                val enum = Option(schema.getEnum)
                  .map(_.asScala.toList.asInstanceOf[List[String]])
                  .getOrElse(Nil)
                if (enum.nonEmpty) {
                  Enum(name, enum)
                } else {
                  val minLength = Option(schema.getMinLength).map(_.intValue())
                  val maxLength = Option(schema.getMaxLength).map(_.intValue())

                  if (minLength.isDefined || maxLength.isDefined)
                    ConstrainedString(
                      minLength.getOrElse(0),
                      maxLength.getOrElse(Int.MaxValue)
                    )
                  else
                    PrimitiveString
                }
              }

            case "integer" =>
              val min = Option(schema.getMinimum).map(_.intValue())
              val max = Option(schema.getMaximum).map(_.intValue())

              if (min.isDefined || max.isDefined)
                ConstrainedInteger(
                  min.getOrElse(Int.MinValue),
                  max.getOrElse(Int.MaxValue)
                )
              else
                PrimitiveInteger

            case "number" =>
              val min = Option(schema.getMinimum).map(_.doubleValue())
              val max = Option(schema.getMaximum).map(_.doubleValue())

              if (min.isDefined || max.isDefined)
                ConstrainedNumber(
                  min.getOrElse(Double.MinValue),
                  max.getOrElse(Double.MaxValue)
                )
              else
                PrimitiveNumber

            case "array" =>
              val minItems = Option(schema.getMinItems).map(_.intValue()).getOrElse(0)
              if (minItems == 0) {
                Array(TypeDefinition.from(name + "_" + "item", schema.getItems))
              } else if (minItems == 1) {
                NonEmptyArray(TypeDefinition.from(name + "_" + "item", schema.getItems))
              } else {
                ConstrainedArray(
                  TypeDefinition.from(name + "_" + "item", schema.getItems),
                  minItems,
                  Option(schema.getMaxItems).map(_.intValue()).getOrElse(Int.MaxValue)
                )
              }

            case other: String =>
              throw new IllegalArgumentException(s"Unsupported schema type: $other")
          }
        }
    }
}
