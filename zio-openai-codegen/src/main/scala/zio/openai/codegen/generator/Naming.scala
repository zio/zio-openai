package zio.openai.codegen.generator

object Naming {
  def toCamelCase(name: String): String = {
    val parts = name.split("""[_\-]""")
    parts.head + parts.tail.map(_.capitalize).mkString
  }

  def toPascalCase(name: String): String =
    toCamelCase(name).capitalize
}
