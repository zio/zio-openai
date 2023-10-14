package zio.openai.examples

import zio.json.ast.Json
import zio.{Console, ZIOAppDefault}
import zio.openai.Edits
import zio.openai.model.CreateEditRequest.Model

/** Based on https://beta.openai.com/docs/api-reference/edits
  */
object Edit extends ZIOAppDefault {

  def createEdit =
    for {
      response <- Edits.createEdit(
                    model = Model.Case1(Model.CaseType1.`Text-davinci-edit-001`),
                    input = "What day of the wek is it?",
                    instruction = "Fix the spelling mistakes"
                  )
      _        <- Console.printLine(
                    response.choices.headOption.map(_.text).getOrElse("No response")
                  )
    } yield ()

  def run =
    createEdit.provide(Edits.default)
}
