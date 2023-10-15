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

  def verboseName: String = name.capitalize

  def scalaType(model: Model): ScalaType

  def transform(f: TypeDefinition => TypeDefinition): TypeDefinition =
    this match {
      case TypeDefinition.Object(directName, parentName, description, fields)               =>
        f(TypeDefinition.Object(directName, parentName, description, fields.map(_.transform(f))))
      case TypeDefinition.PrimitiveBoolean                                                  => f(this)
      case TypeDefinition.PrimitiveString                                                   => f(this)
      case TypeDefinition.ConstrainedString(
            directName,
            parentName,
            minLength,
            maxLength,
            description
          ) =>
        f(this)
      case TypeDefinition.Binary                                                            => f(this)
      case TypeDefinition.PrimitiveInteger                                                  => f(this)
      case TypeDefinition.ConstrainedInteger(directName, parentName, min, max, description) =>
        f(this)
      case TypeDefinition.PrimitiveNumber                                                   => f(this)
      case TypeDefinition.ConstrainedNumber(directName, parentName, min, max, description)  =>
        f(this)
      case TypeDefinition.Array(itemType)                                                   =>
        f(TypeDefinition.Array(itemType.transform(f)))
      case TypeDefinition.NonEmptyArray(itemType)                                           =>
        f(TypeDefinition.NonEmptyArray(itemType.transform(f)))
      case TypeDefinition.ConstrainedArray(itemType, min, max)                              =>
        f(TypeDefinition.ConstrainedArray(itemType.transform(f), min, max))
      case TypeDefinition.Alternatives(
            directName,
            parentName,
            alternatives,
            description,
            customCaseNames
          ) =>
        f(
          TypeDefinition.Alternatives(
            directName,
            parentName,
            alternatives.map(_.transform(f)),
            description,
            customCaseNames
          )
        )
      case TypeDefinition.Enum(name, directName, values, description)                       => f(this)
      case TypeDefinition.Ref(name)                                                         => f(this)
      case TypeDefinition.DynamicObject(directName, parentName, knownFields, description)   =>
        f(
          TypeDefinition.DynamicObject(
            directName,
            parentName,
            knownFields.map(_.transform(f)),
            description
          )
        )
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

  sealed trait SmartNewType extends TypeDefinition with NonPrimitive {
    def withoutParent: SmartNewType
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

  final case class ConstrainedString(
    directName: String,
    parentName: Option[String],
    minLength: Int,
    maxLength: Int,
    description: Option[String]
  ) extends TypeDefinition with NonPrimitive with SmartNewType {

    override def withoutParent: ConstrainedString =
      ConstrainedString(directName, None, minLength, maxLength, description)
  }

  final case object Binary extends TypeDefinition {
    override val name: String = "binary"
    override val description: Option[String] = None

    override def scalaType(model: Model): ScalaType =
      Types.file
  }

  final case object PrimitiveInteger extends TypeDefinition {
    override val name: String = "integer"
    override val description: Option[String] = None

    override def scalaType(model: Model): ScalaType =
      ScalaType.int
  }

  final case class ConstrainedInteger(
    val directName: String,
    parentName: Option[String],
    min: Int,
    max: Int,
    description: Option[String]
  ) extends TypeDefinition with NonPrimitive with SmartNewType {

    override def withoutParent: ConstrainedInteger =
      ConstrainedInteger(directName, None, min, max, description)
  }

  final case object PrimitiveNumber extends TypeDefinition {
    override val name: String = "number"
    override val description: Option[String] = None

    override def scalaType(model: Model): ScalaType =
      ScalaType.double
  }

  final case class ConstrainedNumber(
    directName: String,
    parentName: Option[String],
    min: Double,
    max: Double,
    description: Option[String]
  ) extends TypeDefinition with NonPrimitive with SmartNewType {

    override def withoutParent: ConstrainedNumber =
      ConstrainedNumber(directName, None, min, max, description)
  }

  final case class Array(itemType: TypeDefinition) extends TypeDefinition {
    override val name: String = "array"
    override val description: Option[String] = None

    override val verboseName: String = s"ArrayOf${itemType.verboseName}"

    override def scalaType(model: Model): ScalaType =
      Types.chunkOf(itemType.scalaType(model))
  }

  final case class NonEmptyArray(itemType: TypeDefinition) extends TypeDefinition {
    override val name: String = "array"
    override val description: Option[String] = None

    override val verboseName: String = s"ArrayOf${itemType.verboseName}"

    override def scalaType(model: Model): ScalaType =
      Types.nonEmptyChunkOf(itemType.scalaType(model))
  }

  final case class ConstrainedArray(itemType: TypeDefinition, min: Int, max: Int)
      extends TypeDefinition {
    override val name: String = "array"
    override val description: Option[String] = None

    override val verboseName: String = s"ArrayOf${itemType.verboseName}"

    override def scalaType(model: Model): ScalaType =
      Types.chunkOf(itemType.scalaType(model)) // TODO
  }

  final case class Alternatives(
    directName: String,
    parentName: Option[String],
    alternatives: List[TypeDefinition],
    description: Option[String],
    customCaseNames: List[String] = Nil
  ) extends TypeDefinition with NonPrimitive {

    lazy val caseNames: List[String] =
      if (customCaseNames != Nil) {
        customCaseNames
      } else {
        lazy val default = alternatives.indices.map(idx => s"Case$idx").toList

        if (
          alternatives.collectFirst { case np: NonPrimitive =>
            np
          }.nonEmpty
        ) {
          default
        } else {
          val typeNameBased = alternatives.map { typ =>
            typ.verboseName
          }.distinct

          if (typeNameBased.size == alternatives.size) {
            typeNameBased
          } else {
            default
          }
        }
      }

    def constructors(model: Model): List[(ScalaType, TypeDefinition)] = {
      val typ = scalaType(model)

      alternatives.zip(caseNames).map { case (alt, caseName) =>
        typ / caseName -> model.resolve(alt)
      }
    }
  }

  final case class Enum(
    directName: String,
    parentName: Option[String],
    values: List[String],
    description: Option[String]
  ) extends TypeDefinition with NonPrimitive {
    override def verboseName: String = directName.capitalize
  }

  final case class Ref(name: String) extends TypeDefinition {
    override val description: Option[String] = None

    val referencedName: String = name.stripPrefix("#/components/schemas/")
    override val verboseName: String = referencedName.capitalize

    override def scalaType(model: Model): ScalaType =
      model.types(referencedName).scalaType(model)
  }

  final case class DynamicObject(
    directName: String,
    parentName: Option[String],
    knownFields: List[Field],
    description: Option[String]
  ) extends TypeDefinition with NonPrimitive

  def from(
    parents: ParentChain,
    directName: String,
    schema: Schema[?]
  ): TypeDefinition =
    Option(schema.get$ref()) match {
      case Some(ref) => Ref(ref)
      case None      =>
        val oneOf = Option(schema.getOneOf).map(_.asScala).getOrElse(List.empty)
        val anyOf = Option(schema.getAnyOf).map(_.asScala).getOrElse(List.empty)

        if (oneOf.nonEmpty) {
          Alternatives(
            directName,
            parents.name,
            alternatives = oneOf.zipWithIndex.map { case (schema, idx) =>
              TypeDefinition.from(parents / directName, "CaseType" + idx, schema)
            }.toList,
            Option(schema.getDescription)
          )
        } else if (anyOf.nonEmpty) {
          anyOf match {
            case Seq(first, second)
                if first.getType == "string" && second.getType == "string" && first.getEnum == null && second.getEnum != null =>
              Alternatives(
                directName,
                parents.name,
                alternatives = List(
                  TypeDefinition.from(parents / directName, "Custom", first),
                  TypeDefinition.from(parents / directName, directName + "s", second)
                ),
                Option(schema.getDescription),
                customCaseNames = List(
                  "Custom",
                  "Predefined"
                )
              )
            case _ =>
              Alternatives(
                directName,
                parents.name,
                alternatives = anyOf.zipWithIndex.map { case (schema, idx) =>
                  TypeDefinition.from(parents / directName, "CaseType" + idx, schema)
                }.toList,
                Option(schema.getDescription)
              )
          }
        } else {
          Option(schema.getType).getOrElse("object") match {
            case "object" =>
              val props = Option(schema.getProperties).map(_.asScala).getOrElse(Map.empty)
              val reqd = Option(schema.getRequired).map(_.asScala).getOrElse(List.empty)

              if (props.isEmpty) {
                DynamicObject(
                  directName,
                  parents.name,
                  knownFields = getKnownFieldsFor(directName, parents),
                  Option(schema.getDescription)
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
                      isRequiredOverride(parents, directName, fieldName).getOrElse(
                        reqd.contains(fieldName)
                      ),
                      Option(fieldSchema.getNullable).exists(_.booleanValue()),
                      Option(fieldSchema.getDescription),
                      controlsStreamingResponse = isStreamingOverride(directName, fieldName)
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
                  Enum(directName, parents.name, enum, Option(schema.getDescription))
                } else {
                  val minLength = Option(schema.getMinLength).map(_.intValue())
                  val maxLength = Option(schema.getMaxLength).map(_.intValue())

                  if (minLength.isDefined || maxLength.isDefined)
                    ConstrainedString(
                      directName,
                      parents.name,
                      minLength.getOrElse(0),
                      maxLength.getOrElse(Int.MaxValue),
                      Option(schema.getDescription)
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
                  directName,
                  parents.name,
                  min.getOrElse(Int.MinValue),
                  max.getOrElse(Int.MaxValue),
                  Option(schema.getDescription)
                )
              else
                PrimitiveInteger

            case "number" =>
              val min = Option(schema.getMinimum).map(_.doubleValue())
              val max = Option(schema.getMaximum).map(_.doubleValue())

              if (min.isDefined || max.isDefined)
                ConstrainedNumber(
                  directName,
                  parents.name,
                  min.getOrElse(Double.MinValue),
                  max.getOrElse(Double.MaxValue),
                  Option(schema.getDescription)
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
        Field(
          "batch_size",
          PrimitiveInteger,
          isRequired = false,
          isNullable = false,
          None,
          controlsStreamingResponse = false
        ),
        Field(
          "learning_rate_multiplier",
          PrimitiveNumber,
          isRequired = false,
          isNullable = false,
          None,
          controlsStreamingResponse = false
        ),
        Field(
          "n_epochs",
          PrimitiveInteger,
          isRequired = false,
          isNullable = false,
          None,
          controlsStreamingResponse = false
        ),
        Field(
          "prompt_loss_weight",
          PrimitiveNumber,
          isRequired = false,
          isNullable = false,
          None,
          controlsStreamingResponse = false
        )
      )
    } else {
      List.empty
    }

  // TODO: make this configurable
  private def isRequiredOverride(
    parents: ParentChain,
    objName: String,
    fieldName: String
  ): Option[Boolean] =
    if (objName == "CreateEditResponse" && fieldName == "id") {
      Some(false)
    } else if (objName == "CreateEditResponse" && fieldName == "model") {
      Some(false)
    } else if (
      parents.items.lastOption.contains(
        "CreateCompletionResponse"
      ) && objName == "choices_item" && fieldName == "finish_reason"
    ) {
      Some(false)
    } else {
      None
    }

  // TODO: make this configurable
  private def isStreamingOverride(objName: String, fieldName: String): Boolean =
    fieldName == "stream"
}
