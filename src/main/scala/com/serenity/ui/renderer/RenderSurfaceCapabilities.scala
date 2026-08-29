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

/** A fresh, independently-paintable surface shaped exactly like the surface this capability came from -- same cell
  * metrics, font, logical size and device scale -- for a layer (a pinned panel, a modal, a floating overlay) to own its
  * own persisted buffer instead of painting straight into the shared frame surface (#1100 stage 2). Genuinely optional:
  * a surface with no natural notion of an offscreen sub-buffer (a cell-addressed terminal, which has no sub-cell pixel
  * buffering the same way a raster surface does) simply has no capability to expose, so [[RenderSurface.layerBuffers]]
  * exposes this as an `Option` and callers fall back to painting the layer directly into the shared surface, same as
  * before this capability existed.
  */
trait LayerBufferSupport:

  /** A new surface painting into a blank, fully transparent buffer the same shape as the surface this capability came
    * from. `onFlush` receives the finished image once the caller's `flush()` completes -- compositing it onto the frame
    * surface (e.g. via `RenderSurface.pixels.drawImage`) is the caller's job, not this surface's; a layer surface never
    * publishes itself anywhere on its own.
    */
  def newLayerSurface(onFlush: BufferedImage => Unit): RenderSurface

/** A caret shape a real terminal's own cursor can be styled as via DECSCUSR (`CSI Ps SP q`). */
enum HardwareCursorShape:
  case Block, Underline, Bar

/** A DECSCUSR-expressible caret style: shape plus whether the terminal should blink it itself.
  *
  * There is no cursor-shape setting in [[com.serenity.config.CursorConfig]] today (only
  * [[com.serenity.config.CursorMode]]'s blink/breathe choice) -- callers that delegate the caret to the terminal
  * (#1170) currently always ask for a blinking block, the shape every terminal defaults to. `decscusrParam` is kept as
  * a total function of shape/blink regardless, so a future per-buffer shape setting has somewhere to plug in without
  * touching the escape-emission code.
  */
final case class HardwareCursorStyle(shape: HardwareCursorShape, blinking: Boolean):

  /** The DECSCUSR parameter for this shape/blink pair, per xterm's ctlseqs: `CSI Ps SP q` where `Ps` is 1/2 =
    * blinking/steady block, 3/4 = blinking/steady underline, 5/6 = blinking/steady bar.
    */
  def decscusrParam: Int =
    shape match
      case HardwareCursorShape.Block     => if blinking then 1 else 2
      case HardwareCursorShape.Underline => if blinking then 3 else 4
      case HardwareCursorShape.Bar       => if blinking then 5 else 6

/** Delegating the caret to a real hardware/terminal cursor instead of painting it as surface content. Genuinely
  * optional -- a surface with no native cursor to delegate to (the GUI canvas, which composites its own caret overlay)
  * simply keeps painting its own caret -- so [[RenderSurface.hardwareCursor]] exposes it as an `Option`.
  */
trait HardwareCursor:
  /** Move the terminal's own cursor to cell `(cellX, cellY)`, style it per `style`, and make it visible (`DECTCEM`
    * show). Called on every flush the caret is present for, so the terminal cursor tracks the caret's cell exactly,
    * including across scrolling and multi-cursor navigation.
    */
  def present(cellX: Int, cellY: Int, style: HardwareCursorStyle): Unit

  /** Hide the terminal's own cursor (`DECTCEM` hide) -- used when the caret is app-painted instead (breathe mode,
    * #1170's documented exception) or genuinely not visible this frame.
    */
  def hide(): Unit

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
