package com.serenity.ui.renderer

import java.awt.font.FontRenderContext
import java.awt.image.BufferedImage
import java.awt.{Color, Font}

import com.serenity.config.PostProcessingEffect
import com.serenity.ui.layout.PixelRect
import com.serenity.ui.theme.TextStyle

trait RenderSurface:

  /** Identity of the pixels this surface accumulates into, when it preserves what earlier frames drew.
    *
    * `Some(key)` promises two things: the surface still holds the pixels of the last frame flushed through a surface
    * reporting the same key, and [[clearViewportExcept]] genuinely preserves the rectangles it is handed. Renderers may
    * only skip redrawing unchanged content when a key is present. `None` — the default — means every frame starts from
    * unknown pixels and everything must be drawn.
    */
  def persistentContentKey: Option[AnyRef]         = None
  def setFont(font: Font): Unit                    = ()
  def fontRenderContext: Option[FontRenderContext] = None
  def setForegroundColor(color: Color): Unit
  def setBackgroundColor(color: Color): Unit
  def getBackgroundColor: Color

  def clearViewport(color: Color): Unit =
    setBackgroundColor(color)
    fillRect(0, 0, viewportWidth, viewportHeight, ' ')

  /** Clear the viewport to `color` while leaving the given logical-pixel rectangles untouched.
    *
    * The default clears everything, which is why callers must check [[persistentContentKey]] first: a surface without a
    * persistent key preserves nothing, so its caller has to redraw the content it would otherwise have skipped.
    */
  def clearViewportExcept(color: Color, preserved: List[PixelRect]): Unit =
    clearViewport(color)

  def putString(x: Int, y: Int, s: String): Unit
  def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit
  def enableStyle(style: TextStyle): Unit
  def disableStyle(style: TextStyle): Unit
  def setAlpha(alpha: Float): Unit                                                  = ()
  def blurRegion(x: Int, y: Int, width: Int, height: Int, radius: Float): Unit      = ()
  def applyPostProcessing(effect: PostProcessingEffect): Unit                       = ()
  def applyPostProcessing(effect: PostProcessingEffect, animationPhase: Long): Unit = applyPostProcessing(effect)
  def devicePixelScaleX: Double                                                     = 1.0
  def devicePixelScaleY: Double                                                     = 1.0

  def strokeRoundRect(
    x: Int,
    y: Int,
    width: Int,
    height: Int,
    arcPx: Int,
    color: Color,
    strokeWidth: Float = 1.5f
  ): Unit = ()

  /** Draw a soft shadow behind a rounded UI surface. */
  def drawRoundRectShadow(
    x: Int,
    y: Int,
    width: Int,
    height: Int,
    arcPx: Int,
    color: Color
  ): Unit = ()

  /** Restrict drawing performed by `render` to a rounded rectangle in cell coordinates. */
  def withRoundRectClip(
    x: Int,
    y: Int,
    width: Int,
    height: Int,
    arcPx: Int
  )(render: => Unit): Unit

  /** Translate drawing in device-independent logical pixels for fractional-cell floating geometry. */
  def withPixelTranslation(xPx: Double, yPx: Double)(render: => Unit): Unit = render

  def fillPixelRect(
    xPx: Int,
    yPx: Int,
    widthPx: Int,
    heightPx: Int,
    color: Color
  ): Unit = ()

  /** Draw a proportional text run at exact pixel coordinates.
    *
    * Fills background [xPx, xPx + bgWidthPx) × [yPx, yPx + lineHeightPx) with the current background color, then draws
    * s at (xPx, yPx + ascent) with the current foreground color. Callers set fg/bg colors before calling. Set
    * clipGlyphToRun when a styled overlay must not paint outside its measured run bounds.
    */
  def drawRunPx(
    xPx: Float,
    yPx: Int,
    bgWidthPx: Float,
    lineHeightPx: Int,
    ascentPx: Int,
    s: String,
    clipGlyphToRun: Boolean = false
  ): Unit = ()

  /** Render cell-addressed content for one row at its logical-pixel top edge. */
  def withLogicalPixelRow(cellRow: Int, pixelY: Int)(render: => Unit): Unit = render

  def drawImage(image: BufferedImage, x: Int, y: Int, width: Int, height: Int): Unit = ()

  def hideCursor(): Unit
  def viewportWidth: Int
  def viewportHeight: Int
  def flush(): Unit
