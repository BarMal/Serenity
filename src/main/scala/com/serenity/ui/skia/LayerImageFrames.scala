package com.serenity.ui.skia

import org.jetbrains.skia.Surface

/** The raster surface a window's layer paints its image frames on, kept apart from the raster arm's [[RasterFrames]]
  * because that one also remembers the images it copied into, which a window that falls back to the raster arm must
  * not inherit.
  */
final private[skia] class LayerImageFrames:

  private val frames = new RasterFrames

  def surfaceFor(widthPx: Int, heightPx: Int, scale: Double): Surface = frames.surfaceFor(widthPx, heightPx, scale)

  def release(): Unit = frames.release()
