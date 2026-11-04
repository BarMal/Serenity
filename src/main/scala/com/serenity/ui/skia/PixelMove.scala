package com.serenity.ui.skia

import com.serenity.ui.renderer.DeviceArea
import org.jetbrains.skia.{ContentChangeMode, IRect, Pixmap, Surface}

/** Moves rows of a raster surface inside the surface's own memory. Snapshotting the region and drawing it back
  * allocates a copy of it for every move, which at 5120x2880 cost 41 ms against 2.7 ms in place.
  */
private[skia] object PixelMove:

  private val BandRows = 256

  /** `area` of `surface` moved down by `dyPx` device rows (up if negative), as if every source pixel were read before
    * any was written. Rows that would land outside the surface are dropped, and nothing outside the destination
    * changes.
    *
    * A band is copied row by row from the first row to the last, so a band can only move up over itself. Moving down,
    * the bands are no taller than the move and are taken from the bottom, so no band overlaps its own destination and
    * none overwrites a row a later one still has to read.
    */
  def inPlace(surface: Surface, area: DeviceArea, dyPx: Int): Unit =
    val left   = area.left.max(0)
    val right  = (area.left + area.width).min(surface.getWidth)
    val first  = area.top.max(0).max(-dyPx)
    val last   = (area.top + area.height).min(surface.getHeight).min(surface.getHeight - dyPx)
    val pixels = new Pixmap()
    try
      if dyPx != 0 && left < right && first < last then
        surface.notifyContentWillChange(ContentChangeMode.RETAIN)
        if surface.peekPixels(pixels) then
          val bandRows = if dyPx < 0 then BandRows else BandRows.min(dyPx)
          val starts   = (first until last by bandRows).toList
          (if dyPx < 0 then starts else starts.reverse).foreach { start =>
            copyBand(pixels, left, right - left, start, bandRows.min(last - start), dyPx)
          }
    finally pixels.close()

  private def copyBand(pixels: Pixmap, left: Int, width: Int, top: Int, rows: Int, dyPx: Int): Unit =
    val source = new Pixmap()
    val target = new Pixmap()
    try
      if pixels.extractSubset(source, IRect.Companion.makeXYWH(left, top, width, rows)) &&
          pixels.extractSubset(target, IRect.Companion.makeXYWH(left, top + dyPx, width, rows))
      then
        val _ = source.readPixels(target, 0, 0)
    finally
      source.close()
      target.close()
