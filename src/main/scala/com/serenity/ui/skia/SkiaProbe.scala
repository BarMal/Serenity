package com.serenity.ui.skia

import cats.effect.IO
import com.serenity.app.RenderBackendSelection.Probe
import org.jetbrains.skia.Surface
import org.jetbrains.skiko.Library

/** Loads the Skiko natives and draws into a 1x1 raster surface, so a missing library fails here and not in a window.
  *
  * This is the one place outside the Skia surface that may name Skiko; it is only reached through `IO.defer`, which is
  * what keeps Skiko and Kotlin classes unloaded on every launch that never asks.
  */
object SkiaProbe:

  def run: IO[Probe] = IO.blocking(load())

  // A failed native load is an `UnsatisfiedLinkError`, a `LinkageError`: scala's `NonFatal` leaves those alone, so
  // `IO.attempt` would let one end the process. Catching `Throwable` here is what turns it into a fallback.
  private def load(): Probe =
    try
      Library.INSTANCE.load()
      Option(Surface.Companion.makeRasterN32Premul(1, 1)).fold[Probe](Probe.Failed("no raster surface")) { surface =>
        surface.close()
        Probe.Loaded
      }
    catch case error: Throwable => Probe.Failed(describe(error))

  private def describe(error: Throwable): String =
    s"${error.getClass.getSimpleName}: ${Option(error.getMessage).getOrElse("no message")}"
