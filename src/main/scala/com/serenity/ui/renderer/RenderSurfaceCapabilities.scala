package com.serenity.ui.renderer

import java.awt.font.FontRenderContext
import java.awt.image.BufferedImage
import java.awt.{Color, Font}

import com.serenity.config.PostProcessingEffect

/** Character- and pixel-run text drawing. Every real [[RenderSurface]] implements this -- a surface that cannot draw
  * text cannot render Serenity's UI -- so [[RenderSurface.text]] exposes it directly rather than as an `Option`: the
  * type itself guarantees the capability instead of pushing a check onto every call site that draws a line of text.
  */
trait TextDrawing:
  def setFont(font: Font): Unit
  def fontRenderContext: Option[FontRenderContext]

  /** Draw a proportional text run at exact pixel coordinates.
    *
    * Fills background [xPx, xPx + bgWidthPx) x [yPx, yPx + lineHeightPx) with the current background color, then draws
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
  ): Unit

  /** Render cell-addressed content for one row at its logical-pixel top edge. */
  def withLogicalPixelRow(cellRow: Int, pixelY: Int)(render: => Unit): Unit

/** Pixel-addressed rect fills, image blits, and pixel-space coordinate translation. Like [[TextDrawing]], every real
  * surface implements this -- carets and Markdown preview images are drawn through it unconditionally -- so
  * [[RenderSurface.pixels]] exposes it directly rather than as an `Option`.
  */
trait PixelDrawing:
  def fillPixelRect(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: Color): Unit
  def drawImage(image: BufferedImage, x: Int, y: Int, width: Int, height: Int): Unit

  /** Translate drawing in device-independent logical pixels for fractional-cell floating geometry. */
  def withPixelTranslation(xPx: Double, yPx: Double)(render: => Unit): Unit

/** Alpha compositing, region blur, and CRT-style post-processing. Genuinely optional: a surface that can't do any of
  * this (or a headless test double) simply skips the polish rather than degrading a required drawing operation, so
  * [[RenderSurface.effects]] exposes it as an `Option` and callers decide whether skipping the effect is safe.
  */
trait Effects:
  def setAlpha(alpha: Float): Unit
  def blurRegion(x: Int, y: Int, width: Int, height: Int, radius: Float): Unit

  /** Apply `effect` to the whole surface. `animationPhase` drives time-varying effects (e.g. scanline scroll); it
    * defaults to a wall-clock tick so callers that don't care about a specific phase don't need to compute one.
    */
  def applyPostProcessing(effect: PostProcessingEffect, animationPhase: Long = System.nanoTime() / 50000000L): Unit

/** Rounded-rectangle chrome: borders, drop shadows, and clipping content to a rounded rect. Genuinely optional
  * decoration -- panels and overlays still read correctly with square corners and no border/shadow -- so
  * [[RenderSurface.roundedRects]] exposes it as an `Option`.
  */
trait RoundedRectDrawing:

  def strokeRoundRect(
    x: Int,
    y: Int,
    width: Int,
    height: Int,
    arcPx: Int,
    color: Color,
    strokeWidth: Float = 1.5f
  ): Unit

  /** Draw a soft shadow behind a rounded UI surface. */
  def drawRoundRectShadow(
    x: Int,
    y: Int,
    width: Int,
    height: Int,
    arcPx: Int,
    color: Color
  ): Unit

  /** Restrict drawing performed by `render` to a rounded rectangle in cell coordinates. */
  def withRoundRectClip(
    x: Int,
    y: Int,
    width: Int,
    height: Int,
    arcPx: Int
  )(render: => Unit): Unit
