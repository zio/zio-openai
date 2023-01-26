package zio.openai.codegen

import sbt.*
import sbt.Keys.*
import zio.nio.file.Path
import zio.openai.codegen.OpenAIOpenAPIPlugin.autoImport.getOpenAIOpenAPI
import zio.*
import zio.openai.codegen.generator.{ Loader, OpenAIGenerator, Parameters }

object ZioOpenAICodegenPlugin extends AutoPlugin {
  object autoImport {
    lazy val generateSources =
      Def.task {
        val log = streams.value.log
        val runtime = zio.Runtime.default
        val sbtLogger = new ZLogger[String, Unit] {
          override def apply(
            trace: zio.Trace,
            fiberId: FiberId,
            logLevel: LogLevel,
            message: () => String,
            cause: Cause[Any],
            context: FiberRefs,
            spans: List[LogSpan],
            annotations: Map[String, String]
          ): Unit = {
            val sb = new StringBuilder()
            sb.append(message())

            if (cause != null && cause != Cause.empty) {
              sb.append("\n")
              sb.append(cause.prettyPrint)
            }

            log.log(
              logLevel match {
                case LogLevel.Info    => sbt.Level.Info
                case LogLevel.Warning => sbt.Level.Warn
                case LogLevel.Error   => sbt.Level.Error
                case LogLevel.Debug   => sbt.Level.Debug
                case LogLevel.Trace   => sbt.Level.Debug
                case LogLevel.Fatal   => sbt.Level.Error
                case _                => sbt.Level.Info
              },
              sb.toString()
            )
          }
        }

        val sourcesDir = (Compile / sourceManaged).value
        val ver = scalaVersion.value
        val scalaVer = scalaVersion.value

        val cachedFun = FileFunction.cached(
          streams.value.cacheDirectory / s"zio-openai-api-$ver",
          FileInfo.hash
        ) { input: Set[File] =>
          input.foldLeft(Set.empty[File]) { (result, openApi) =>
            Unsafe.unsafe { implicit u =>
              val fs = runtime.unsafe
                .run {
                  val task =
                    for {
                      model        <- Loader.loadModel(Path.fromJava(openApi.toPath))
                      modelFiles   <- OpenAIGenerator.generateModels(model)
                      serviceFiles <- OpenAIGenerator.generateServices(model)
                    } yield modelFiles ++ serviceFiles

                  task
                    .provide(
                      ZLayer.succeed(Parameters(Path.fromJava(sourcesDir.toPath), scalaVer)),
                      OpenAIGenerator.live,
                      zio.Runtime.removeDefaultLoggers,
                      zio.Runtime.addLogger(sbtLogger)
                    )
                    .tapError { generatorError =>
                      ZIO
                        .attempt(
                          log.error(s"Code generator failure: ${generatorError}")
                        )
                    }
                }
                .getOrThrowFiberFailure()

              result union fs.map(_.toFile)
            }
          }
        }

        val openAPI = getOpenAIOpenAPI.value
        cachedFun(Set(openAPI)).toSeq
      }
  }

  import autoImport._

  override val requires = OpenAIOpenAPIPlugin

  override def projectSettings: Seq[Def.Setting[?]] =
    Seq(
      Compile / sourceGenerators += generateSources.taskValue
    )
}
