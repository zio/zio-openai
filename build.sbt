import BuildHelper._

inThisBuild(
  List(
    organization  := "dev.zio",
    homepage      := Some(url("https://zio.dev/zio-flow/")),
    licenses      := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers    := List(
      Developer(
        "jdegoes",
        "John De Goes",
        "john@degoes.net",
        url("http://degoes.net")
      ),
      Developer(
        "vigoo",
        "Daniel Vigovszky",
        "daniel.vigovszky@gmail.com",
        url("https://vigoo.github.io/")
      )
    ),
    resolvers +=
      "Sonatype OSS Snapshots 01" at "https://s01.oss.sonatype.org/content/repositories/snapshots",
    pgpPassphrase := sys.env.get("PGP_PASSWORD").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc"),
    resolvers +=
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  )
)

addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
addCommandAlias(
  "check",
  "; scalafmtSbtCheck; scalafmtCheckAll"
)

lazy val root = (project in file("."))
  .settings(
    name               := "zio-openai",
    publish / skip     := true,
    crossScalaVersions := Nil
  )
  .aggregate(zioOpenAI, examples, docs)

lazy val zioOpenAI = Project("zio-openai", file("zio-openai"))
  .settings(stdSettings("zio-openai"))
  .settings(buildInfoSettings("zio.openai"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"                   % Dependencies.zio,
      "dev.zio" %% "zio-http"              % Dependencies.zioHttp,
      "dev.zio" %% "zio-json"              % Dependencies.zioJson,
      "dev.zio" %% "zio-nio"               % Dependencies.zioNio exclude ("org.scala-lang.modules", "scala-collection-compat_2.13"),
      "dev.zio" %% "zio-prelude"           % Dependencies.zioPrelude,
      "dev.zio" %% "zio-schema"            % Dependencies.zioSchema,
      "dev.zio" %% "zio-schema-json"       % Dependencies.zioSchema,
      "dev.zio" %% "zio-schema-derivation" % Dependencies.zioSchema,
      "dev.zio" %% "zio-test"              % Dependencies.zio % Test,
      "dev.zio" %% "zio-test-sbt"          % Dependencies.zio % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
  .enablePlugins(ZioOpenAICodegenPlugin)

lazy val examples = Project("zio-openai-examples", file("zio-openai-examples"))
  .settings(stdSettings("zio-openai-examples"))
  .settings(
    publish / skip := true
  )
  .dependsOn(zioOpenAI)

lazy val docs = project
  .in(file("zio-openai-docs"))
  .settings(stdSettings("zio-openai"))
  .settings(macroDefinitionSettings)
  .settings(
    scalaVersion                               := Scala213,
    publish / skip                             := true,
    moduleName                                 := "zio-openai-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    projectName                                := "ZIO OpenAI",
    mainModuleName                             := (zioOpenAI / moduleName).value,
    projectStage                               := ProjectStage.Development,
    docsPublishBranch                          := "main",
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(zioOpenAI)
  )
  .dependsOn(zioOpenAI)
  .enablePlugins(WebsitePlugin)
