ThisBuild / scalaVersion     := "2.13.10"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "dev.zio"
ThisBuild / organizationName := "zio"

lazy val root = (project in file("."))
  .settings(
    name           := "zio-openai",
    publish / skip := true
  )
  .aggregate(zioOpenAI)

lazy val zioOpenAI = Project("zio-openai", file("zio-openai"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"             % "2.0.6",
      "dev.zio" %% "zio-http"        % "0.0.4",
      "dev.zio" %% "zio-json"        % "0.4.2",
      "dev.zio" %% "zio-prelude"     % "1.0.0-RC16",
      "dev.zio" %% "zio-schema"      % "0.4.2",
      "dev.zio" %% "zio-schema-json" % "0.4.2"
    )
  )
  .enablePlugins(ZioOpenAICodegenPlugin)