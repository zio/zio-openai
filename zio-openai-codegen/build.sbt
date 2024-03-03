sbtPlugin := true

organization := "dev.zio"
name         := "zio-openai-codegen"

scalaVersion := "2.12.19"

scalacOptions ++= Seq("-feature", "-deprecation")

libraryDependencies ++= Seq(
  "dev.zio"             %% "zio"              % "2.0.21",
  "dev.zio"             %% "zio-json"         % "0.6.2",
  "dev.zio"             %% "zio-nio"          % "2.0.2",
  "io.github.vigoo"     %% "metagen-core"     % "0.0.20",
  "io.swagger.parser.v3" % "swagger-parser"   % "2.1.14",
  "org.scalameta"       %% "scalameta"        % "4.8.11",
  "org.scalameta"       %% "scalafmt-dynamic" % "3.7.14"
)
