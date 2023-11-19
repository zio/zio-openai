package zio.openai.examples

import zio.openai.Files
import zio.openai.model.CreateFileRequest.Purpose
import zio.openai.model.File
import zio.prelude.data.Optional
import zio.{ Console, ZIOAppDefault }

/** Based on https://beta.openai.com/docs/api-reference/files/list
  */
object ManagingFiles extends ZIOAppDefault {

  def example =
    for {
      initialFiles <- Files.listFiles()
      r1           <- Files.createFile(
                        File.jsonl("""{"prompt": "<prompt text>", "completion": "<ideal generated text>"}"""),
                        Purpose.`Fine-tune`
                      )
      _            <- Console.printLine(s"Upload status: ${r1.status}")
      middleFiles  <- Files.listFiles()
      r2           <- Files.downloadFile(r1.id)
      _            <- Console.printLine(s"Downloaded file's content: $r2")
      r3           <- Files.deleteFile(r1.id)
      _            <- Console.printLine(s"Delete status: ${r3.deleted}")
      finalFiles   <- Files.listFiles()
      _            <- Console.printLine(s"Files before: $initialFiles")
      _            <- Console.printLine(s"Files after uploading: $middleFiles")
      _            <- Console.printLine(s"Files after deleting: $finalFiles")
    } yield ()

  def run =
    example.provide(Files.default)
}
