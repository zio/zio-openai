package zio.openai.examples

import zio.openai._
import zio.openai.model.CreateCompletionRequest.{ Model, Prompt }
import zio.openai.model.Temperature
import zio.{ Console, ZIOAppDefault }

/** Based on https://beta.openai.com/docs/quickstart/build-your-application
  */
object QuickstartStreaming extends ZIOAppDefault {

  def generatePrompt(animal: String): Prompt =
    Prompt.String {
      s"""Suggest three names for an animal that is a superhero.
         |
         |Animal: Cat
         |Names: Captain Sharpclaw, Agent Fluffball, The Incredible Feline
         |Animal: Dog
         |Names: Ruff the Protector, Wonder Canine, Sir Barks-a-Lot
         |Animal: ${animal.capitalize}
         |Names:""".stripMargin
    }

  def loop =
    for {
      animal      <- Console.readLine("Animal: ").orDie
      resultStream = Completions.createCompletionStreaming(
                       model = Model.Predefined(Model.Models.`Gpt-3.5-turbo-instruct`),
                       prompt = generatePrompt(animal),
                       temperature = Temperature(0.6)
                     )
      _           <- resultStream
                       .runForeach { chunk =>
                         Console
                           .print(
                             s"${chunk.choices.map(_.text).mkString(", ")}"
                           )
                           .orDie
                       }
                       .when(animal.nonEmpty)
      _           <- Console.printLine("")
    } yield animal.nonEmpty

  override def run =
    loop
      .repeatWhile(_ == true)
      .provide(
        Completions.default
      )

}
