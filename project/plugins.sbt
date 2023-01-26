addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.6")

lazy val codegen = project
  .in(file("."))
  .dependsOn(ProjectRef(file("../zio-openai-codegen"), "zio-openai-codegen"))
