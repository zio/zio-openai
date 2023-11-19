package zio.openai.codegen.model

sealed trait UseCase

object UseCase {
  case object Field extends UseCase
  case object Request extends UseCase
  case object Response extends UseCase
}
