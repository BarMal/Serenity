import sbtassembly.AssemblyPlugin.autoImport.*
import sbtassembly.MergeStrategy

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.8.3"

ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

scalacOptions ++= Seq(
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
      case x => (assembly / assemblyMergeStrategy).value(x)
    },
    Test / testOptions ++= Seq(
      Tests.Setup(() => System.setProperty("serenity.test.ephemeralSessions", "true")),
      Tests.Cleanup(() => System.clearProperty("serenity.test.ephemeralSessions"))
    )
  )

libraryDependencies ++= Seq(
  "org.typelevel"         %% "cats-effect"     % "3.7-4972921",
  "co.fs2"                %% "fs2-core"        % "3.13.0-M2",
  "co.fs2"                %% "fs2-io"          % "3.13.0-M2",
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
