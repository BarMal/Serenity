package com.serenity.ui.renderer

import java.awt.font.FontRenderContext

/** Character- and pixel-run text drawing. Every real [[RenderSurface]] implements this -- a surface that cannot draw
  * text cannot render Serenity's UI -- so [[RenderSurface.text]] exposes it directly rather than as an `Option`: the
  * type itself guarantees the capability instead of pushing a check onto every call site that draws a line of text.
  */
trait TextDrawing:
  def setFont(font: FontSpec): Unit
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
  def fillPixelRect(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: RenderColor): Unit
  def drawImage(image: RenderImage, x: Int, y: Int, width: Int, height: Int): Unit

  /** Composite a whole-surface layer image (a modal/panel layer buffer, produced by
    * [[LayerBufferSupport.newLayerSurface]]) back onto this surface, covering it exactly.
    *
    * Distinct from [[drawImage]] because a layer buffer is already at this surface's own backing resolution: it must be
    * blitted one-for-one, not scaled through the cell grid and device transform that [[drawImage]]'s cell-addressed
    * geometry applies. Routing it through `drawImage(image, 0, 0, viewportWidth, viewportHeight)` snaps the destination
    * to `floor(logicalSize / cellSize) * cellSize` and then re-scales by the device factor, so a layer comes back very
    * slightly smaller than it left -- imperceptible in one frame, but the command runner re-composites on every
    * navigation keystroke, so the shrink compounds into a visible "zoom out" of the editor pane behind it until a full
    * clean repaint resets it.
    *
    * The only real surface with a layer-buffer capability is [[Java2DRenderSurface]], which overrides this to blit 1:1
    * in device pixels; every other surface either advertises no layer buffers at all (a terminal) or is a headless test
    * double. The default records the composite as an ordinary full-surface `drawImage` so those doubles keep observing
    * one blit per layer paint, without needing this surface's backing resolution.
    */
  def compositeFullSurfaceLayer(image: RenderImage): Unit =
    drawImage(image, 0, 0, image.widthPx, image.heightPx)

  /** Translate drawing in device-independent logical pixels for fractional-cell floating geometry. */
  def withPixelTranslation(xPx: Double, yPx: Double)(render: => Unit): Unit

/** Alpha compositing. Genuinely optional: a surface that can't do it (or a headless test double) simply skips the fade
  * rather than degrading a required drawing operation, so [[RenderSurface.effects]] exposes it as an `Option` and
  * callers decide whether skipping the effect is safe.
  */
trait Effects:
  def setAlpha(alpha: Float): Unit

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
    * surface (e.g. via `RenderSurface.pixels.compositeFullSurfaceLayer`) is the caller's job, not this surface's; a
    * layer surface never publishes itself anywhere on its own.
    *
    * Everything the layer's owner didn't paint stays transparent. That is what makes compositing the whole layer back
    * over a frame whose content has since changed correct (#1798) -- a layer seeded with a full copy of the frame would
    * paste that stale copy back over everything.
    *
    * `recycled` is a previous layer image the caller is done with; it is cleared and painted into instead of
    * allocating, when its size still matches.
    */
  def newLayerSurface(onFlush: RenderImage => Unit, recycled: Option[RenderImage] = None): RenderSurface

/** A caret shape a real terminal's own cursor can be styled as via DECSCUSR (`CSI Ps SP q`). */
enum HardwareCursorShape:
  case Block, Underline, Bar

/** A DECSCUSR-expressible caret style: shape plus whether the terminal should blink it itself.
  *
  * There is no cursor-shape setting in [[com.serenity.config.CursorConfig]] today (only
  * [[com.serenity.config.CursorMode]]'s blink mode) -- callers that delegate the caret to the terminal (#1170)
  * currently always ask for a blinking block, the shape every terminal defaults to. `decscusrParam` is kept as a total
  * function of shape/blink regardless, so a future per-buffer shape setting has somewhere to plug in without touching
  * the escape-emission code.
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

  /** Hide the terminal's own cursor (`DECTCEM` hide) -- used when the caret is not visible this frame.
    */
  def hide(): Unit

/** Panel chrome: a square border and clipping content to the panel's rect. Genuinely optional decoration -- panels
  * still read correctly without a border -- so [[RenderSurface.panelOutlines]] exposes it as an `Option`.
  */
trait PanelOutlineDrawing:

  def strokeRect(x: Int, y: Int, width: Int, height: Int, color: RenderColor, strokeWidth: Float): Unit

  /** Restrict drawing performed by `render` to a rectangle in cell coordinates. */
  def withRectClip(x: Int, y: Int, width: Int, height: Int)(render: => Unit): Unit
