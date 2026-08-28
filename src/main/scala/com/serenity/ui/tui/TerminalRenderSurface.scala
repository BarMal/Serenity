package com.serenity.ui.tui

import java.awt.Color
import java.awt.font.FontRenderContext
import java.awt.image.BufferedImage
import java.io.Writer
import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.layout.{CellMetrics, PixelRect}
import com.serenity.ui.renderer.*
import com.serenity.ui.theme.TextStyle

/** The [[RenderSurface]] a JLine [[TerminalShell]] exposes over a [[TerminalScreenBuffer]]: every drawing method
  * forwards straight onto the buffer (already shaped to match `RenderSurface`'s own `putString`/`fillRect`/colour-and-
  * style calls), and [[flush]] diffs the buffer's snapshot against the previous frame with [[TerminalAnsiDiff]] and
  * writes the result to `writer`.
  *
  * `cellMetrics` is used only to translate the pixel-space [[PixelRect]]s [[clearViewportExcept]] receives back into
  * this surface's cell coordinates -- `putString`/`fillRect` themselves are already cell-addressed, same as every other
  * `RenderSurface`.
  */
final class TerminalRenderSurface(width: Int, height: Int, writer: Writer, cellMetrics: CellMetrics)
    extends RenderSurface
    with TextDrawing
    with PixelDrawing
    with RoundedRectDrawing:

  private val screenBuffer     = new TerminalScreenBuffer(width, height)
  private val previousFrameRef = new AtomicReference[Option[TerminalFrame]](None)

  def text: TextDrawing                                 = this
  def pixels: PixelDrawing                              = this
  override def roundedRects: Option[RoundedRectDrawing] = Some(this)

  /** A terminal genuinely holds the previous frame's cells until something overwrites them, so the renderer's dirty-
    * region skip logic may treat this surface as persistent -- see [[clearViewportExcept]], which upholds the other
    * half of that promise by only clearing the cells outside `preserved`.
    */
  override def persistentContentKey: Option[SurfaceContentIdentity] =
    Some(SurfaceContentIdentity(screenBuffer))

  def setForegroundColor(color: Color): Unit = screenBuffer.setForegroundColor(color)
  def setBackgroundColor(color: Color): Unit = screenBuffer.setBackgroundColor(color)
  def getBackgroundColor: Color              = screenBuffer.getBackgroundColor

  override def clearViewportExcept(color: Color, preserved: List[PixelRect]): Unit =
    if preserved.isEmpty then clearViewport(color)
    else
      setBackgroundColor(color)
      val preservedCells = preserved.map(toCellRect)
      for
        row <- 0 until height
        col <- 0 until width
        if !preservedCells.exists(_.contains(col, row))
      do screenBuffer.fillRect(col, row, 1, 1, ' ')

  private def toCellRect(rect: PixelRect): TerminalRenderSurface.CellRect =
    TerminalRenderSurface.CellRect(
      cellMetrics.toCol(rect.xPx),
      cellMetrics.toRow(rect.yPx),
      cellMetrics.toCol(rect.rightPx),
      cellMetrics.toRow(rect.bottomPx)
    )

  def putString(x: Int, y: Int, s: String): Unit = screenBuffer.putString(x, y, s)
  def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit =
    screenBuffer.fillRect(x, y, width, height, char)
  def enableStyle(style: TextStyle): Unit  = screenBuffer.enableStyle(style)
  def disableStyle(style: TextStyle): Unit = screenBuffer.disableStyle(style)

  // The hardware cursor is hidden once, for the whole session, by TerminalShell's acquire -- what the renderer calls
  // "the cursor" is a glyph it paints as ordinary cell content (same as every other RenderSurface), so there is
  // nothing further for this call to do.
  def hideCursor(): Unit = ()

  def viewportWidth: Int  = width
  def viewportHeight: Int = height

  def flush(): Unit =
    val next = screenBuffer.snapshot
    val ansi = TerminalAnsiDiff.emit(previousFrameRef.getAndSet(Some(next)), next)
    writer.write(ansi)
    writer.flush()

  // -- TextDrawing --------------------------------------------------------------------------------------------------

  // No AWT font backs a terminal cell; typography controls are inert in cell space (accepted degradation, epic #1103).
  override def setFont(font: java.awt.Font): Unit = ()

  // `None` unconditionally: this is what drives #1105's cell-fallback path for callers that would otherwise measure
  // text with a FontRenderContext this surface does not have.
  override def fontRenderContext: Option[FontRenderContext] = None

  // Pixel-precise proportional text has no meaning on a fixed-cell surface -- real text drawing goes through
  // putString/fillRect, which every RenderSurface (including this one) implements for real. #1105 is what keeps
  // callers from reaching this method on a surface reporting fontRenderContext = None; not this issue's job.
  override def drawRunPx(
    xPx: Float,
    yPx: Int,
    bgWidthPx: Float,
    lineHeightPx: Int,
    ascentPx: Int,
    s: String,
    clipGlyphToRun: Boolean = false
  ): Unit = ()

  // putString already only ever accepts a cell row, never a pixel offset within it -- there is no sub-cell position
  // for this override to apply, so the row's own paint calls run unchanged.
  override def withLogicalPixelRow(cellRow: Int, pixelY: Int)(render: => Unit): Unit = render

  // -- PixelDrawing ---------------------------------------------------------------------------------------------------
  // Pixel-addressed fills and image blits have no meaningful target on a fixed-cell surface: #1012 made `pixels`
  // required specifically because every *other* real RenderSurface draws pixels (carets, Markdown preview images) --
  // its own PR notes call a terminal cell surface out as the one legitimate exception, deliberately degrading rather
  // than skipping this capability entirely.

  override def fillPixelRect(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: Color): Unit = ()

  override def drawImage(image: BufferedImage, x: Int, y: Int, width: Int, height: Int): Unit = ()

  override def withPixelTranslation(xPx: Double, yPx: Double)(render: => Unit): Unit = render

  // -- RoundedRectDrawing -----------------------------------------------------------------------------------------

  // Borders and drop shadows are pixel-only chrome with no sub-cell geometry to draw them with; genuinely optional
  // decoration per RenderSurface's own doc on `roundedRects`, so panels/overlays still read correctly without them.
  override def strokeRoundRect(
    x: Int,
    y: Int,
    width: Int,
    height: Int,
    arcPx: Int,
    color: Color,
    strokeWidth: Float = 1.5f
  ): Unit = ()

  override def drawRoundRectShadow(x: Int, y: Int, width: Int, height: Int, arcPx: Int, color: Color): Unit = ()

  /** The one piece of rounded-rect chrome a cell grid *can* express: a rectangular clip. The arc radius is ignored --
    * cells have no sub-cell geometry to round a corner with.
    */
  override def withRoundRectClip(x: Int, y: Int, width: Int, height: Int, arcPx: Int)(render: => Unit): Unit =
    screenBuffer.withClip(x, y, width, height)(render)

object TerminalRenderSurface:
  final private case class CellRect(x0: Int, y0: Int, x1: Int, y1: Int):
    def contains(col: Int, row: Int): Boolean = col >= x0 && col < x1 && row >= y0 && row < y1
