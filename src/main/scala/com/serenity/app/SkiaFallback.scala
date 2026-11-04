package com.serenity.app

import com.serenity.app.RenderBackendSelection.Decision

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

  def missingLibrary(cause: String): Option[String] =
    MissingLibrary.findFirstMatchIn(cause).map(_.group(1))
