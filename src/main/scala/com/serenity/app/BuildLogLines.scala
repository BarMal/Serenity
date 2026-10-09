package com.serenity.app

import java.lang.management.ManagementFactory

import scala.jdk.CollectionConverters.*

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.BuildInfo
import org.typelevel.log4cats.Logger

/** The startup lines that let a log say which build and JVM produced it. */
object BuildLogLines:

  final case class Build(version: String, commit: String, commitTime: String, channel: String)

  private val Unknown = "unknown"

  // Only JVM tuning flags are reported: -D values, agents and class paths can carry private paths or secrets.
  private val NotTuning = List("-Xbootclasspath", "-Xrun", "-Xdebug", "-Xnoagent")

  def buildLine(build: Build, property: String => Option[String]): String =
    def value(key: String): String = property(key).filter(_.nonEmpty).getOrElse(Unknown)
    val launcher                   = property("jpackage.app-path").filter(_.nonEmpty).getOrElse("java")
    s"[BUILD] Serenity ${build.version} (${build.commit}, ${build.commitTime}) ${build.channel}; " +
      s"${value("os.name")} ${value("os.version")}/${value("os.arch")}; " +
      s"Java ${value("java.runtime.version")} ${value("java.vm.vendor")}; launcher=$launcher"

  def jvmArgumentsLine(inputArguments: List[String]): String =
    val tuning = inputArguments.filter(arg => arg.startsWith("-X") && !NotTuning.exists(arg.startsWith))
    s"[JVM] arguments: ${if tuning.isEmpty then "none" else tuning.mkString(" ")}"

  def current(inputArguments: List[String]): List[String] =
    val build = Build(BuildInfo.version, BuildInfo.commit, BuildInfo.commitTime, BuildInfo.channel)
    List(buildLine(build, key => Option(System.getProperty(key))), jvmArgumentsLine(inputArguments))

  def announce(using logger: Logger[IO]): IO[Unit] =
    IO(ManagementFactory.getRuntimeMXBean.getInputArguments.asScala.toList)
      .flatMap(arguments => current(arguments).traverse_(logger.info(_)))
