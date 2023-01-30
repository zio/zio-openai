package zio.openai.model

import zio.Chunk
import zio.nio.file.{ Files, Path }
import zio.schema.{ DeriveSchema, Schema }

import java.nio.charset.StandardCharsets

final case class File(data: Chunk[Byte], fileName: String)

object File {
  implicit val schema: Schema[File] = DeriveSchema.gen[File]

  def jsonl(data: Chunk[Byte]): File = File(data, "file.jsonl")
  def jsonl(data: String): File =
    jsonl(Chunk.fromArray(data.getBytes(StandardCharsets.UTF_8)))
  def png(data: Chunk[Byte]): File = File(data, "file.png")

  def read(path: Path) =
    Files.readAllBytes(path).map(File(_, path.filename.toString))
}
