package com.serenity.ui.renderer

import java.awt.font.FontRenderContext

import scala.annotation.unused

import com.serenity.config.CursorMode
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{PixelRect, RoundedCorners}

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

  /** Whether drawing `text` is better served by [[drawShapedRunPx]] than by a [[drawRunPx]] per run. A surface whose
    * font shapes nothing for the text has no shaping to share, so the default says no.
    */
  def shapesStretchesOf(text: String): Boolean = false

  /** As [[drawRunPx]] for `stretch.substring(from, until)`, where `stretch` is text drawn in one font across several
    * runs. A raster surface shapes the stretch once and draws each run from those glyphs, so ligatures and kerning
    * reach across run boundaries; the default draws the run's own text.
    */
  def drawShapedRunPx(
    xPx: Float,
    yPx: Int,
    bgWidthPx: Float,
    lineHeightPx: Int,
    ascentPx: Int,
    stretch: String,
    from: Int,
    until: Int,
    clipGlyphToRun: Boolean = false
  ): Unit =
    drawRunPx(xPx, yPx, bgWidthPx, lineHeightPx, ascentPx, stretch.substring(from, until), clipGlyphToRun)

  /** Runs `draw` with the surface font swapped for `family` at the current font's style, size and ligature setting, for
    * glyphs the current font has none of. A surface with one font for all text, such as a terminal, draws as it would
    * anyway. With `ligaturesOn` the glyphs are drawn with ligatures on whatever the surface font's setting.
    */
  def withFallbackFamily(@unused family: String, @unused ligaturesOn: Boolean = false)(draw: => Unit): Unit = draw

  /** As [[drawRunPx]], for a short label that repeats verbatim from frame to frame, such as a line number. A raster
    * surface may serve it from sprites of the rendered ink rather than laying the text out again; the default draws it
    * as any other run.
    */
  def drawLabelPx(
    xPx: Float,
    yPx: Int,
    bgWidthPx: Float,
    lineHeightPx: Int,
    ascentPx: Int,
    s: String
  ): Unit =
    drawRunPx(xPx, yPx, bgWidthPx, lineHeightPx, ascentPx, s)

  /** Render cell-addressed content for one row at its logical-pixel top edge. */
  def withLogicalPixelRow(cellRow: Int, pixelY: Int)(render: => Unit): Unit

/** Pixel-addressed rect fills, image blits, and pixel-space coordinate translation. Like [[TextDrawing]], every real
  * surface implements this -- carets and Markdown preview images are drawn through it unconditionally -- so
  * [[RenderSurface.pixels]] exposes it directly rather than as an `Option`.
  */
trait PixelDrawing:
  def fillPixelRect(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: RenderColor): Unit

  /** Fill each rect of `rects`, four ints (x, y, width, height) apiece, with `color`: the picture [[fillPixelRect]]
    * makes for each in turn. A surface where every fill is a call into native code draws them as one.
    */
  def fillPixelRects(rects: IArray[Int], color: RenderColor): Unit =
    (0 until rects.length by 4).foreach(i => fillPixelRect(rects(i), rects(i + 1), rects(i + 2), rects(i + 3), color))

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

  /** Composite a layer buffer made by [[LayerBufferSupport.newBoundedLayerSurface]] for `bounds` back where it was
    * painted. A surface whose bounded layers are whole-surface ones (the default) composites them as such.
    */
  def compositeLayer(image: RenderImage, @unused bounds: PixelRect): Unit =
    compositeFullSurfaceLayer(image)

  /** Translate drawing in device-independent logical pixels for fractional-cell floating geometry. */
  def withPixelTranslation(xPx: Double, yPx: Double)(render: => Unit): Unit

/** Moving pixels the surface already holds, rather than drawing them again. Genuinely optional: a surface that cannot
  * (a cell-addressed terminal, or a raster surface whose device scale is not a whole number, where logical pixels do
  * not land on device ones) simply draws every row, so [[RenderSurface.pixelCopying]] exposes it as an `Option`.
  */
trait PixelCopying:

  /** Copy `from`, in logical pixels, `dyPx` rows down (up when negative), overwriting whatever lies there. Source and
    * destination may overlap: the copy reads every source pixel as it was before it.
    */
  def copyPixels(from: PixelRect, dyPx: Int): Unit

/** What a rect of a surface's pixels held, to be put back exactly as they were. */
trait LiftedPixels

/** Drawing something over pixels that persist from frame to frame, and taking it off again: the pixels beneath are
  * lifted first and put back before the next frame builds on them. Genuinely optional, so
  * [[RenderSurface.pixelLifting]] exposes it as an `Option`.
  */
trait PixelLifting:

  /** The pixels of `area`, in logical pixels, with a device pixel's margin so that a fill of the same area is covered
    * whatever the scale rounds it to. `spare` is pixels lifted before and since given back, whose storage is reused
    * when it fits.
    */
  def lift(area: PixelRect, spare: Option[LiftedPixels]): LiftedPixels

  /** Overwrites the pixels `lifted` came from with what they held. */
  def putBack(lifted: LiftedPixels): Unit

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

  /** As [[newLayerSurface]], for a layer that paints only inside `bounds` (logical pixels): the surface still addresses
    * the whole window, so a renderer paints at its usual coordinates, but the image it hands back covers `bounds` alone
    * and goes back through [[PixelDrawing.compositeLayer]]. A capability with no smaller buffer to offer returns the
    * whole-surface layer, which that composite handles too.
    */
  def newBoundedLayerSurface(
    @unused bounds: PixelRect,
    onFlush: RenderImage => Unit,
    recycled: Option[RenderImage] = None
  ): RenderSurface =
    newLayerSurface(onFlush, recycled)

/** A caret shape a real terminal's own cursor can be styled as via DECSCUSR (`CSI Ps SP q`). */
enum HardwareCursorShape:
  case Block, Underline, Bar

/** A DECSCUSR-expressible caret style: shape plus whether the terminal should blink it itself.
  *
  * There is no cursor-shape setting in [[com.serenity.config.CursorConfig]] today (only
  * [[com.serenity.config.CursorMode]]'s blink or steady mode) -- callers that delegate the caret to the terminal
  * (#1170) always ask for a block, the shape every terminal defaults to. `decscusrParam` is kept as a total function of
  * shape/blink regardless, so a future per-buffer shape setting has somewhere to plug in without touching the
  * escape-emission code.
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

object HardwareCursorStyle:

  def forMode(mode: CursorMode): HardwareCursorStyle =
    HardwareCursorStyle(HardwareCursorShape.Block, blinking = mode == CursorMode.Blink)

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

  /** [[strokeRect]] with the `corners` rounded to `radiusPx` device pixels. A surface that cannot round draws the
    * square frame, and so does a zero radius or no rounded corner.
    */
  def strokeRoundRect(
    x: Int,
    y: Int,
    width: Int,
    height: Int,
    @unused corners: RoundedCorners,
    @unused radiusPx: Int,
    color: RenderColor,
    strokeWidth: Float
  ): Unit =
    strokeRect(x, y, width, height, color, strokeWidth)

  /** Fills the rectangle with `color`, the `corners` rounded to `radiusPx` device pixels, and says whether it did. It
    * does not when nothing rounds (a zero radius, no rounded corner, or a surface that cannot round): the caller fills
    * the cells as it always has. Unlike [[withRoundRectClip]] it keeps no clip alive, which is what makes it cheap
    * enough for the chrome drawn every frame.
    */
  def fillRoundRect(
    @unused x: Int,
    @unused y: Int,
    @unused width: Int,
    @unused height: Int,
    @unused corners: RoundedCorners,
    @unused radiusPx: Int,
    @unused color: RenderColor
  ): Boolean = false

  /** Restricts `render` to the rectangle with the `corners` rounded to `radiusPx` device pixels, so a panel's fill
    * takes the same shape as its outline. With nothing to round (a zero radius, no rounded corner, or a surface that
    * cannot round) it just renders: clipping a fill to its own rectangle changes nothing.
    */
  def withRoundRectClip(
    @unused x: Int,
    @unused y: Int,
    @unused width: Int,
    @unused height: Int,
    @unused corners: RoundedCorners,
    @unused radiusPx: Int
  )(render: => Unit): Unit =
    render
