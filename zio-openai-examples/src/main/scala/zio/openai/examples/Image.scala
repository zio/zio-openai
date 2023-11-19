package zio.openai.examples

import zio.nio.file.{ Files, Path }
import zio.{ Chunk, Console, ZIO, ZIOAppDefault }
import zio.openai.Images
import zio.openai.model.CreateImageRequest.Size
import zio.openai.model.{ N, ResponseFormat }

import java.util.Base64

/** Based on https://beta.openai.com/docs/api-reference/images
  */
object Image extends ZIOAppDefault {

  def createImageFromPrompt =
    for {
      response <- Images.createImage(
                    "A cute baby sea otter",
                    n = N(2),
                    size = Size.`1024x1024`,
                    responseFormat = ResponseFormat.B64_json
                  )
      _        <- ZIO.foreachDiscard(response.data.zipWithIndex) { case (data, idx) =>
                    val imageData = Base64.getDecoder.decode(data.b64Json.getOrElse(""))
                    val path = Path(s"result-image-$idx.png")

                    Files.writeBytes(path, Chunk.fromArray(imageData)) *> Console.printLine(
                      s"Image written to $path"
                    )
                  }
    } yield ()

  def run =
    createImageFromPrompt.provide(Images.default)
}
