sbtPlugin := true

organization := "dev.zio"
name         := "zio-openai-codegen"

scalaVersion := "2.12.17"

scalacOptions ++= Seq("-feature", "-deprecation")

libraryDependencies ++= Seq(
  "dev.zio"             %% "zio"              % "2.0.10",
  "dev.zio"             %% "zio-json"         % "0.4.2",
  "dev.zio"             %% "zio-nio"          % "2.0.1",
  "io.github.vigoo"     %% "metagen-core"     % "0.0.18",
  "io.swagger.parser.v3" % "swagger-parser"   % "2.1.11",
  "org.scalameta"       %% "scalameta"        % "4.7.3",
  "org.scalameta"       %% "scalafmt-dynamic" % "3.7.1"
)
