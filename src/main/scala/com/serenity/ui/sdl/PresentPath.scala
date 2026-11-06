package com.serenity.ui.sdl

/** How a window puts frames on screen: through the GL context SDL made for it, or, when SDL could not make one, with
  * Skia's CPU raster into the window's own surface. The raster path itself is a later phase; this is only the choice.
  */
enum PresentPath[+C]:
  case Gpu(context: C)
  case RasterFallback(reason: SdlError)

  def describe: String = this match
    case Gpu(_)                  => "GL"
    case RasterFallback(failure) => s"Skia raster (no GL context: ${failure.describe})"

object PresentPath:

  def fromContextAttempt[C](attempt: Either[SdlError, C]): PresentPath[C] =
    attempt.fold(RasterFallback(_), Gpu(_))
