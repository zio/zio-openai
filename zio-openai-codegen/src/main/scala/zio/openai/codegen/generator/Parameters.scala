package zio.openai.codegen.generator

import zio.nio.file.Path

final case class Parameters(targetRoot: Path, scalaVersion: String)
