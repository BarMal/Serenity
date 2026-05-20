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
    name := "Serenity"
  )

libraryDependencies ++= Seq(
  "org.typelevel"          %% "cats-effect"     % "3.7-4972921",
  "co.fs2"                 %% "fs2-core"        % "3.13.0-M2",
  "com.googlecode.lanterna" % "lanterna"        % "3.2.0-alpha1",
  "org.scalatest"          %% "scalatest"       % "3.2.19" % "test",
  "com.github.pureconfig"  %% "pureconfig-core" % "0.17.9"
)

val log4CatsVersion = "2.7.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "log4cats-core"   % log4CatsVersion,
  "org.typelevel" %% "log4cats-slf4j"  % log4CatsVersion,
  "ch.qos.logback" % "logback-classic" % "1.5.18"
)