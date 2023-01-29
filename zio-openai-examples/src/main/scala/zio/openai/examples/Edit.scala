package zio.openai.examples

import zio.{ Console, ZIOAppDefault }
import zio.openai.Edits

/**
 * Based on https://beta.openai.com/docs/api-reference/edits
 */
object Edit extends ZIOAppDefault {

  def createEdit =
    for {
      response <- Edits.createEdit(
                    model = "text-davinci-edit-001",
                    input = "What day of the wek is it?",
                    instruction = "Fix the spelling mistakes"
                  )
      _        <- Console.printLine(
                    response.choices.headOption.flatMap(_.text.toOption).getOrElse("No response")
                  )
    } yield ()

  def run =
    createEdit.provide(Edits.default)
}
