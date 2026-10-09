package com.serenity.diagnostics

import java.io.{PrintWriter, StringWriter}
import java.nio.file.Path
import java.time.Instant

/** The text a person pastes into a bug report: build, machine, when, and what failed. */
object CrashReport:

  def render(
    identity: RuntimeIdentity,
    at: Instant,
    headline: String,
    error: Option[Throwable],
    logDirectory: Path
  ): String =
    (header(identity, at, logDirectory) ++ List("", headline) ++ error.map(stackTrace).toList).mkString("\n")

  /** For a run that left no crash file: killed, powered off, or crashed beyond the reach of the handlers. */
  def unrecordedExit(
    runningBuild: Option[String],
    identity: RuntimeIdentity,
    at: Instant,
    logDirectory: Path
  ): String =
    val previous = runningBuild.fold("The build that was running was not recorded.")(build => s"It was running: $build")
    (header(identity, at, logDirectory) ++ List(
      "",
      "The previous session ended without a clean exit, and no error was recorded.",
      previous
    )).mkString("\n")

  private def header(identity: RuntimeIdentity, at: Instant, logDirectory: Path): List[String] =
    List(s"Serenity crash report: ${identity.summary}", s"Time: $at") ++ identity.lines ++ List(s"Logs: $logDirectory")

  private def stackTrace(error: Throwable): String =
    val writer = StringWriter()
    error.printStackTrace(PrintWriter(writer, true))
    writer.toString.trim
