package com.serenity.ui.skia

import java.awt.Font

import com.serenity.SkiaPixels
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, PixelRect}
import com.serenity.ui.renderer.{FontSpec, StyledFontCache}
import org.jetbrains.skia.Surface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `copyPixels` moves a region of the retained surface, and must leave every pixel it does not write as it was. Each
  * logical row is its own colour, so a row that lands in the wrong place is seen at once.
  */
class SkiaScrollMoveSpec extends AnyFlatSpec with Matchers:

  private val metrics = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
  private val width   = 120
  private val height  = 700

  private def rowColour(row: Int): RenderColor =
    RenderColor.fromRgba(40 + row * 7 % 200, 30 + row * 13 % 220, 20 + row * 29 % 230)

  final private class Window(scale: Double):

    val surface: Surface =
      Surface.Companion.makeRasterN32Premul(
        SkiaRenderSurface.deviceDimension(width, scale),
        SkiaRenderSurface.deviceDimension(height, scale)
      )

    val renderSurface: SkiaRenderSurface =
      SkiaRenderSurface.retaining(
        surface,
        metrics,
        FontSpec.fromAwt(new Font(Font.MONOSPACED, Font.PLAIN, 12)),
        width,
        height,
        scale,
        SkiaTextCaches.fresh(),
        StyledFontCache()
      )

    (0 until height).foreach(row => renderSurface.pixels.fillPixelRect(0, row, width, 1, rowColour(row)))
    (0 until height by 9).foreach(row => renderSurface.pixels.fillPixelRect(row % width, row, 3, 1, rowColour(row + 5)))

    def pixels: Array[Int] = SkiaPixels.read(surface)

    def move(from: PixelRect, dy: Int): Unit =
      renderSurface.pixelCopying.getOrElse(fail("expected pixel copying")).copyPixels(from, dy)

  /** What a move of `from` by `dy` leaves, in device pixels: the destination takes the source as it was, and the rest
    * of the surface is untouched.
    */
  private def moved(
    before: Array[Int],
    deviceWidth: Int,
    deviceHeight: Int,
    scale: Double,
    from: PixelRect,
    dy: Int,
    within: Option[(Int, Int, Int, Int)] = None
  ): Array[Int] =
    val left  = math.round(from.xPx * scale).toInt
    val top   = math.round(from.yPx * scale).toInt
    val w     = math.round(from.widthPx * scale).toInt
    val h     = math.round(from.heightPx * scale).toInt
    val shift = math.round(dy * scale).toInt
    val after = before.clone()
    for
      row <- 0 until h
      col <- 0 until w
      sourceY = top + row
      targetY = sourceY + shift
      sourceX = left + col
      if sourceY < deviceHeight && sourceX < deviceWidth && targetY >= 0 && targetY < deviceHeight
      if within.forall((l, t, r, b) => sourceX >= l && sourceX < r && targetY >= t && targetY < b)
    do after(targetY * deviceWidth + sourceX) = before(sourceY * deviceWidth + sourceX)
    after

  for scale <- List(1.0, 2.0) do
    val cases = List(
      "one row down"                      -> (PixelRect(10, 20, 80, 600), 1),
      "one row up"                        -> (PixelRect(10, 20, 80, 600), -1),
      "a few rows down"                   -> (PixelRect(10, 20, 80, 600), 7),
      "a few rows up"                     -> (PixelRect(10, 20, 80, 600), -7),
      "more rows down than a band"        -> (PixelRect(0, 10, 120, 300), 300),
      "more rows up than a band"          -> (PixelRect(0, 380, 120, 300), -330),
      "the whole surface down"            -> (PixelRect(0, 0, 120, 700), 40),
      "the whole surface up"              -> (PixelRect(0, 0, 120, 700), -40),
      "a region that runs off the bottom" -> (PixelRect(0, 500, 120, 200), 50),
      "a region that moves above the top" -> (PixelRect(5, 30, 100, 100), -60),
      "a narrow region"                   -> (PixelRect(37, 100, 3, 400), 21)
    )
    for (label, (from, dy)) <- cases do
      s"copyPixels at ${scale}x" should s"move $label as a snapshot would" in {
        val window = new Window(scale)
        val before = window.pixels
        window.move(from, dy)
        window.pixels shouldBe moved(before, window.surface.getWidth, window.surface.getHeight, scale, from, dy)
      }

    s"copyPixels at ${scale}x" should "stay inside a clip that is in force" in {
      val window = new Window(scale)
      val before = window.pixels
      window.renderSurface.withRectClip(2, 5, 8, 20) {
        window.move(PixelRect(0, 0, 120, 700), 30)
      }
      val clip = (
        math.round(14 * scale).toInt,
        math.round(65 * scale).toInt,
        math.round(70 * scale).toInt,
        math.round(325 * scale).toInt
      )
      window.pixels shouldBe moved(
        before,
        window.surface.getWidth,
        window.surface.getHeight,
        scale,
        PixelRect(0, 0, 120, 700),
        30,
        Some(clip)
      )
    }
