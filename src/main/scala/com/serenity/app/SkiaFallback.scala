package com.serenity.app

import com.serenity.app.RenderBackendSelection.{Decision, Probe}

/** The notice for the fallback a user did not ask for: Skia was wanted, its native library would not load, and the
  * window became the terminal.
  */
object SkiaFallback:

  private val MissingLibrary = """([\w.+-]+\.so(?:\.[\d.]+)?): cannot open shared object file""".r.unanchored

  def notice(decision: Decision): Option[String] =
    decision.skiaFailure.map { cause =>
      val lead = "Skia could not load, so Serenity is running in the terminal."
      val detail = missingLibrary(cause) match
        case Some(library) =>
          s"The dynamic loader cannot find $library. Put the directories that hold it on LD_LIBRARY_PATH before " +
            "launching (see \"Skia on NixOS\" in DEVELOPMENT.md)."
        case None => s"Cause: $cause. The [RENDER] line in the log has the same detail."
      s"$lead $detail"
    }

  /** Skiko reports a library the dynamic loader cannot resolve as a wrapper that names only its own file; the library
    * that is missing is in the `UnsatisfiedLinkError` it wraps. The causes ride in `detail`, apart from `cause`, which
    * is what the log line has always shown.
    */
  def failure(error: Throwable): Probe.Failed =
    val causes = Iterator
      .iterate(Option(error.getCause))(_.flatMap(next => Option(next.getCause)))
      .takeWhile(_.isDefined)
      .flatten
      .take(MaxCauses)
      .toList
      .filterNot(_ eq error)
    Probe.Failed(describe(error), Option.when(causes.nonEmpty)(causes.map(describe).mkString("; caused by ")))

  private val MaxCauses = 5

  private def describe(error: Throwable): String =
    s"${error.getClass.getSimpleName}: ${Option(error.getMessage).getOrElse("no message")}"

  def missingLibrary(cause: String): Option[String] =
    MissingLibrary.findFirstMatchIn(cause).map(_.group(1))
