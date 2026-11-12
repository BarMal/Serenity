package com.serenity.ui.skia

/** The window's Skia raster could not be made, though the startup probe's one-pixel surface could. Once the window
  * exists there is no other backend to fall back to, so this ends the run through the crash path, and the report names
  * the size that was asked for and why it failed.
  */
final class SkiaRasterUnavailable(val widthPx: Int, val heightPx: Int, cause: Throwable)
    extends RuntimeException(
      s"Skia could not make a ${widthPx}x$heightPx raster for the window: ${SkiaRasterUnavailable.describe(cause)}",
      cause
    )

object SkiaRasterUnavailable:

  private def describe(cause: Throwable): String =
    Option(cause.getMessage).filter(_.nonEmpty).getOrElse(cause.getClass.getSimpleName)
