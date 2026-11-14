package com.serenity.app

import com.serenity.app.RendererChoice.Candidate

/** The config settings that put the window in a renderer mode (#2618). */
object RendererSettings:

  def of(candidate: Candidate): List[(String, String)] = candidate match
    case Candidate.LayerImage   => List("ui.render.backend" -> "skia", "ui.render.skia_frames" -> "image")
    case Candidate.LayerPicture => List("ui.render.backend" -> "skia", "ui.render.skia_frames" -> "picture")
    case Candidate.SkiaRaster   => List("ui.render.backend" -> "skia-raster")

  /** As they read in config.conf, for a person to copy. */
  def line(candidate: Candidate): String = of(candidate).map((key, value) => s"$key = $value").mkString(", ")
