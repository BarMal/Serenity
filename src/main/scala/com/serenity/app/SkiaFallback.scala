package com.serenity.app

import com.serenity.app.RenderBackendSelection.{Decision, Probe}

/** The notice for the one fallback a user cannot see: Skia was wanted, its native library would not load, and the
  * window quietly paints with Java2D.
  */
object SkiaFallback:

  private val MissingLibrary = """([\w.+-]+\.so(?:\.[\d.]+)?): cannot open shared object file""".r.unanchored

  def notice(decision: Decision): Option[String] =
    decision.skiaFailure.map { cause =>
      val lead = "Skia could not load, so Serenity is painting with Java2D."
      val detail = missingLibrary(cause) match
        case Some(library) =>
          s"The dynamic loader cannot find $library. Put the directories that hold it on LD_LIBRARY_PATH before " +
            "launching (see \"Skia on NixOS\" in DEVELOPMENT.md)."
        case None => s"Cause: $cause. The [RENDER] line in the log has the same detail."
      s"$lead $detail Set ui.render.backend = java2d to stop this notice."
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
