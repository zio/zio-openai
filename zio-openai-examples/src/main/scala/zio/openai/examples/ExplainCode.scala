package zio.openai.examples

import zio.json.ast.Json
import zio.openai.Completions
import zio.openai.model.CreateCompletionRequest.Model.CaseType1
import zio.openai.model.CreateCompletionRequest.{MaxTokens, Model, Prompt, Stop}
import zio.openai.model.{FrequencyPenalty, PresencePenalty, Temperature, TopP}
import zio.{Console, ZIOAppDefault}

/** Based on https://beta.openai.com/examples/default-explain-code
  */
object ExplainCode extends ZIOAppDefault {

  def program =
    for {
      response <- Completions.createCompletion(
                    model = Model.Case1(CaseType1.`Gpt-3.5-turbo-instruct`),
                    prompt = Prompt.String(
                      """class Log:
                        |    def __init__(self, path):
                        |        dirname = os.path.dirname(path)
                        |        os.makedirs(dirname, exist_ok=True)
                        |        f = open(path, "a+")
                        |
                        |        # Check that the file is newline-terminated
                        |        size = os.path.getsize(path)
                        |        if size > 0:
                        |            f.seek(size - 1)
                        |            end = f.read(1)
                        |            if end != "\n":
                        |                f.write("\n")
                        |        self.f = f
                        |        self.path = path
                        |
                        |    def log(self, event):
                        |        event["_event_id"] = str(uuid.uuid4())
                        |        json.dump(event, self.f)
                        |        self.f.write("\n")
                        |
                        |    def state(self):
                        |        state = {"complete": set(), "last": None}
                        |        for line in open(self.path):
                        |            event = json.loads(line)
                        |            if event["type"] == "submit" and event["success"]:
                        |                state["complete"].add(event["id"])
                        |                state["last"] = event
                        |        return state
                        |
                        |'''
                        |Here's what the above class is doing:
                        |1.""".stripMargin
                    ),
                    temperature = Temperature(0.0),
                    maxTokens = MaxTokens(64),
                    topP = TopP(1.0),
                    frequencyPenalty = FrequencyPenalty(0.0),
                    presencePenalty = PresencePenalty(0.0),
                    stop = Stop.String("'''")
                  )
      _        <- Console.printLine(
                    response.choices.headOption.map(_.text).getOrElse("No response")
                  )
    } yield ()

  def run =
    program.provide(Completions.default)
}
