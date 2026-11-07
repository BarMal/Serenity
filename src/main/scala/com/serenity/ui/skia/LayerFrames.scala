package com.serenity.ui.skia

/** What a window hands its GPU layer for each frame. */
enum LayerFrames:
  /** The frame's ops, replayed by the layer on the event thread. */
  case Picture

  /** The frame's pixels, painted by Skia's CPU rasteriser off the event thread, which the layer then only copies. */
  case Image

object LayerFrames:

  val Property = "serenity.skia.layerFrames"

  def parse(setting: Option[String]): LayerFrames =
    setting.map(_.trim.toLowerCase) match
      case Some("image") => Image
      case _             => Picture

  def configured: LayerFrames = parse(Option(System.getProperty(Property)))
