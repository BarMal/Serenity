import sbtassembly.AssemblyPlugin.autoImport.*
import sbtassembly.MergeStrategy

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.8.4"

ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

Compile / scalacOptions ++= Seq(
  "-Wunused:all",
  "-Wvalue-discard",
  "-Wnonunit-statement"
)

Test / scalacOptions --= Seq(
  "-Wunused:all",
  "-Wvalue-discard",
  "-Wnonunit-statement"
)

Test / scalacOptions ++= Seq(
  "-Wunused:imports"
)

Compile / run / fork := true

lazy val root = (project in file("."))
  .settings(
    name := "Serenity",
    Compile / mainClass := Some("Main"),
    assembly / mainClass := Some("Main"),
    assembly / assemblyJarName := "Serenity.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", xs @ _*) =>
        xs.map(_.toLowerCase) match {
          case "manifest.mf" :: Nil => MergeStrategy.discard
          case "index.list" :: Nil  => MergeStrategy.discard
          case "dependencies" :: Nil => MergeStrategy.discard
          case _ => MergeStrategy.discard
        }
      case PathList("module-info.class") => MergeStrategy.discard
      case x => (assembly / assemblyMergeStrategy).value(x)
    },
    Test / testOptions ++= Seq(
      Tests.Setup(() => System.setProperty("serenity.test.ephemeralSessions", "true")),
      Tests.Cleanup(() => System.clearProperty("serenity.test.ephemeralSessions"))
    )
  )

libraryDependencies ++= Seq(
  "org.typelevel"         %% "cats-effect"     % "3.7.0",
  "co.fs2"                %% "fs2-core"        % "3.13.0",
  "co.fs2"                %% "fs2-io"          % "3.13.0",
  "org.scalatest"         %% "scalatest"       % "3.2.19" % "test",
  "com.github.pureconfig" %% "pureconfig-core" % "0.17.9"
)

val circeVersion = "0.14.10"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core"    % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser"  % circeVersion
)

val log4CatsVersion = "2.7.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "log4cats-core"   % log4CatsVersion,
  "org.typelevel" %% "log4cats-slf4j"  % log4CatsVersion,
  "ch.qos.logback" % "logback-classic" % "1.5.18"
)

val commonMarkVersion = "0.27.0"

libraryDependencies ++= Seq(
  "org.commonmark"   % "commonmark"                   % commonMarkVersion,
  "org.commonmark"   % "commonmark-ext-gfm-tables"    % commonMarkVersion,
  "org.commonmark"   % "commonmark-ext-task-list-items" % commonMarkVersion,
  "org.xhtmlrenderer" % "flying-saucer-core"          % "10.2.2"
)
