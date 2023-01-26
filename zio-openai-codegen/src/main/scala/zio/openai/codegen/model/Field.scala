package zio.openai.codegen.model

final case class Field(name: String, typ: TypeDefinition, isRequired: Boolean, isNullable: Boolean)
