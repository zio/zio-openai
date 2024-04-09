addSbtPlugin("ch.epfl.scala"                     % "sbt-bloop"                     % "1.5.17")
addSbtPlugin("ch.epfl.scala"                     % "sbt-scalafix"                  % "0.11.0")
addSbtPlugin("com.eed3si9n"                      % "sbt-buildinfo"                 % "0.11.0")
addSbtPlugin("com.github.sbt"                    % "sbt-unidoc"                    % "0.5.0")
addSbtPlugin("com.github.sbt"                    % "sbt-ci-release"                % "1.5.12")
addSbtPlugin("com.github.cb372"                  % "sbt-explicit-dependencies"     % "0.3.1")
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings"              % "3.0.2")
addSbtPlugin("com.typesafe"                      % "sbt-mima-plugin"               % "1.1.2")
addSbtPlugin("de.heikoseeberger"                 % "sbt-header"                    % "5.10.0")
addSbtPlugin("org.portable-scala"                % "sbt-scala-native-crossproject" % "1.3.1")
addSbtPlugin("org.portable-scala"                % "sbt-scalajs-crossproject"      % "1.3.1")
addSbtPlugin("org.scala-js"                      % "sbt-scalajs"                   % "1.13.1")
addSbtPlugin("org.scala-native"                  % "sbt-scala-native"              % "0.4.17")
addSbtPlugin("org.scalameta"                     % "sbt-mdoc"                      % "2.5.2")
addSbtPlugin("org.scalameta"                     % "sbt-scalafmt"                  % "2.5.0")
addSbtPlugin("pl.project13.scala"                % "sbt-jcstress"                  % "0.2.0")
addSbtPlugin("pl.project13.scala"                % "sbt-jmh"                       % "0.4.7")
addSbtPlugin("com.timushev.sbt"                  % "sbt-updates"                   % "0.6.4")
addSbtPlugin("dev.zio"                           % "zio-sbt-website"               % "0.4.0-alpha.23")

libraryDependencies += "org.snakeyaml" % "snakeyaml-engine" % "2.6"

lazy val codegen = project
  .in(file("."))
  .dependsOn(ProjectRef(file("../zio-openai-codegen"), "zio-openai-codegen"))
