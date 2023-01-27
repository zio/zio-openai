package zio.openai.codegen.model

import zio.openai.codegen.generator.Naming.toCamelCase

import scala.meta.Term

final case class Field(name: String, typ: TypeDefinition, isRequired: Boolean, isNullable: Boolean) {
  val scalaName: String = toCamelCase(name)
  val scalaNameTerm: Term.Name = Term.Name(scalaName)
}
