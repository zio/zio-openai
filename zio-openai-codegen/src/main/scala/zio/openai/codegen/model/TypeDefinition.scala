package zio.openai.codegen.model

import io.github.vigoo.metagen.core.ScalaType
import io.swagger.v3.oas.models.media.Schema
import zio.Chunk
import zio.openai.codegen.generator.Naming.toPascalCase
import zio.openai.codegen.generator.{ Packages, Types }

import scala.jdk.CollectionConverters.*

sealed trait TypeDefinition {
  val name: String
  val description: Option[String]

  def scalaType(model: Model): ScalaType

  def transformEnums(f: TypeDefinition.Enum => TypeDefinition.Enum): TypeDefinition =
    this match {
      case TypeDefinition.Object(directName, parentName, description, fields) =>
        TypeDefinition.Object(directName, parentName, description, fields.map(_.transformEnums(f)))
      case TypeDefinition.PrimitiveBoolean                                    => this
      case TypeDefinition.PrimitiveString                                     => this
      case TypeDefinition.ConstrainedString(minLength, maxLength)             => this
      case TypeDefinition.Binary                                              => this
      case TypeDefinition.PrimitiveInteger                                    => this
      case TypeDefinition.ConstrainedInteger(min, max)                        => this
      case TypeDefinition.PrimitiveNumber                                     => this
      case TypeDefinition.ConstrainedNumber(min, max)                         => this
      case TypeDefinition.Array(itemType)                                     => this
      case TypeDefinition.NonEmptyArray(itemType)                             => this
      case TypeDefinition.ConstrainedArray(itemType, min, max)                => this
      case TypeDefinition.Alternatives(directName, parentName, alternatives)  =>
        TypeDefinition.Alternatives(directName, parentName, alternatives.map(_.transformEnums(f)))
      case e @ TypeDefinition.Enum(name, directName, values)                  => f(e)
      case TypeDefinition.Ref(name)                                           => this
      case TypeDefinition.DynamicObject(directName, parentName, knownFields)  =>
        TypeDefinition.DynamicObject(directName, parentName, knownFields.map(_.transformEnums(f)))
    }
}

object TypeDefinition {
  trait NonPrimitive { self: TypeDefinition =>
    val directName: String
    val parentName: Option[String]
    override val name: String = parentName match {
      case Some(value) => value + "_" + directName
      case None        => directName
    }

    val scalaName: String = toPascalCase(directName)

    override def scalaType(model: Model): ScalaType =
      parentName match {
        case Some(parentName) =>
          model.finalTypes.get(parentName) match {
            case Some(parentType) =>
              parentType.scalaType(model) / scalaName
            case None             => ScalaType(Packages.models, scalaName)
          }
        case None             => ScalaType(Packages.models, scalaName)
      }

    def isTopLevel: Boolean = parentName.isEmpty

    def children(model: Model): List[TypeDefinition] =
      model.finalTypes.collect {
        case (_, child: NonPrimitive) if child.parentName.contains(name) => child
      }.toList
  }

  final case class Object(
    directName: String,
    parentName: Option[String],
    description: Option[String],
    fields: List[Field]
  ) extends TypeDefinition with NonPrimitive

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

  final case class Alternatives(
    directName: String,
    parentName: Option[String],
    alternatives: List[TypeDefinition]
  ) extends TypeDefinition with NonPrimitive {
    override val description: Option[String] = None

    def constructors(model: Model): List[(ScalaType, TypeDefinition)] = {
      val typ = scalaType(model)
      alternatives.zipWithIndex.map { case (alt, idx) =>
        typ / s"Case$idx" -> alt
      }
    }
  }

  final case class Enum(
    directName: String,
    parentName: Option[String],
    values: List[String]
  ) extends TypeDefinition with NonPrimitive {
    override val description: Option[String] = None
  }

  final case class Ref(name: String) extends TypeDefinition {
    override val description: Option[String] = None

    val referencedName: String = name.stripPrefix("#/components/schemas/")

    override def scalaType(model: Model): ScalaType =
      model.types(referencedName).scalaType(model)
  }

  final case class DynamicObject(
    directName: String,
    parentName: Option[String],
    knownFields: List[Field]
  ) extends TypeDefinition with NonPrimitive {
    override val description: _root_.scala.Option[String] = None
  }

  def from(parents: ParentChain, directName: String, schema: Schema[?]): TypeDefinition =
    Option(schema.get$ref()) match {
      case Some(ref) => Ref(ref)
      case None      =>
        val oneOf = Option(schema.getOneOf).map(_.asScala).getOrElse(List.empty)

        if (oneOf.nonEmpty) {
          Alternatives(
            directName,
            parents.name,
            alternatives = oneOf.zipWithIndex.map { case (schema, idx) =>
              TypeDefinition.from(parents / directName, "case" + idx, schema)
            }.toList
          )
        } else {
          Option(schema.getType).getOrElse("object") match {
            case "object" =>
              val props = Option(schema.getProperties).map(_.asScala).getOrElse(Map.empty)
              val reqd = Option(schema.getRequired).map(_.asScala).getOrElse(List.empty)

              if (props.isEmpty) {
                DynamicObject(
                  directName,
                  parents.name,
                  knownFields = getKnownFieldsFor(directName, parents)
                )
              } else {
                Object(
                  directName,
                  parents.name,
                  Option(schema.getDescription),
                  fields = props.map { case (fieldName, fieldSchema) =>
                    Field(
                      fieldName,
                      TypeDefinition.from(parents / directName, fieldName, fieldSchema),
                      reqd.contains(fieldName),
                      Option(fieldSchema.getNullable).exists(_.booleanValue()),
                      Option(fieldSchema.getDescription)
                    )
                  }.toList
                )
              }

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
                  Enum(directName, parents.name, enum)
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
                Array(TypeDefinition.from(parents, directName + "_" + "item", schema.getItems))
              } else if (minItems == 1) {
                NonEmptyArray(
                  TypeDefinition.from(parents, directName + "_" + "item", schema.getItems)
                )
              } else {
                ConstrainedArray(
                  TypeDefinition.from(parents, directName + "_" + "item", schema.getItems),
                  minItems,
                  Option(schema.getMaxItems).map(_.intValue()).getOrElse(Int.MaxValue)
                )
              }

            case other: String =>
              throw new IllegalArgumentException(s"Unsupported schema type: $other")
          }
        }
    }

  // TODO: make this configurable
  private def getKnownFieldsFor(name: String, parents: ParentChain): List[Field] =
    if (name == "hyperparams" && parents.items == Chunk("FineTune")) {
      List(
        Field("batch_size", PrimitiveInteger, isRequired = false, isNullable = false, None),
        Field(
          "learning_rate_multiplier",
          PrimitiveNumber,
          isRequired = false,
          isNullable = false,
          None
        ),
        Field("n_epochs", PrimitiveInteger, isRequired = false, isNullable = false, None),
        Field("prompt_loss_weight", PrimitiveNumber, isRequired = false, isNullable = false, None)
      )
    } else {
      List.empty
    }
}
