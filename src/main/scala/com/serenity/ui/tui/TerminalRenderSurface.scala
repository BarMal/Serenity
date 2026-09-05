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
    with RoundedRectDrawing
    with HardwareCursor:

  private val screenBuffer     = new TerminalScreenBuffer(width, height)
  private val previousFrameRef = new AtomicReference[Option[TerminalFrame]](None)

  // The caret this surface's own hardware cursor is currently asked to show -- `None` is hidden (DECTCEM hide).
  // Diffed against `lastEmittedCaretRef` on flush the same way screen content is diffed, so a flush with no caret
  // movement and no content damage writes nothing at all (#1170's zero-idle-wakeup contract).
  private val caretRef            = new AtomicReference[Option[TerminalRenderSurface.Caret]](None)
  private val lastEmittedCaretRef = new AtomicReference[Option[Option[TerminalRenderSurface.Caret]]](None)

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

  // Start-page frames have no caret at all; #1170's per-flush caret diffing already leaves the hardware cursor
  // hidden by default (`caretRef` starts `None`), so there is nothing further for this call to do.
  def hideCursor(): Unit = ()

  def viewportWidth: Int  = width
  def viewportHeight: Int = height

  // -- HardwareCursor (#1170: the caret delegated to the terminal's own cursor) -----------------------------------

  override def hardwareCursor: Option[HardwareCursor] = Some(this)

  def present(cellX: Int, cellY: Int, style: HardwareCursorStyle): Unit =
    caretRef.set(Some(TerminalRenderSurface.Caret(cellX, cellY, style)))

  def hide(): Unit = caretRef.set(None)

  /** Bracketing a flush's whole emission (content diff + caret escape) in DEC private mode 2026 tells a synchronized-
    * output-aware terminal (kitty, foot, wezterm) to buffer every write until `?2026l` and paint the result as one
    * atomic frame, instead of possibly sampling -- and the compositor possibly compositing -- mid-write on a large
    * diff. Emitted unconditionally rather than behind a capability probe: `CSI ? Pm h`/`l` with an unrecognized private
    * mode number is a documented no-op on every DEC-derived terminal (xterm's ctlseqs.txt: an unsupported
    * `DECSET`/`DECRST` parameter is simply ignored), so there is nothing to negotiate and no probe round-trip to wait
    * on -- this codebase has no existing machinery for reading a terminal's response to a query (`TerminalShell` never
    * reads from the terminal, only writes to it), so a DECRQM probe would add a new, unproven capability rather than
    * reuse an established one for what the issue itself frames as a small, self-contained change. Skipped on a flush
    * that writes nothing (`ansi` and the caret escape both empty) so an unchanged frame still costs zero bytes,
    * preserving #1170's zero-idle-wakeup contract.
    */
  def flush(): Unit =
    val next  = screenBuffer.snapshot
    val ansi  = TerminalAnsiDiff.emit(previousFrameRef.getAndSet(Some(next)), next)
    val caret = caretEscape(forceReassert = ansi.nonEmpty)
    if ansi.nonEmpty || caret.nonEmpty then
      writer.write(TerminalRenderSurface.BeginSyncUpdate)
      writer.write(ansi)
      writer.write(caret)
      writer.write(TerminalRenderSurface.EndSyncUpdate)
    writer.flush()

  /** The caret escape for this flush, or `""` when the caret's presented/hidden state hasn't changed since the last
    * flush that actually emitted one -- an unmoved, already-visible caret costs nothing on a content-unchanged flush,
    * same as the content diff itself.
    *
    * `forceReassert` (issue #1215) overrides that dedup whenever this same flush's content diff (`ansi`) is non-empty:
    * `TerminalAnsiDiff`'s own `CUP` writes leave the terminal's real cursor wherever the diff's last cell was drawn,
    * entirely independent of where the caret logically belongs -- a content-only flush (an animation tick, a status-bar
    * refresh, anything that doesn't itself move the caret) would otherwise silently drag the visible cursor away with
    * nothing here to pull it back, since the dedup only ever compared the caret's own target against its last-emitted
    * value. Re-asserting the caret on every content-changing flush costs nothing on a genuinely idle frame (`ansi`
    * empty keeps the old dedup exactly as before) and is what actually keeps the terminal's cursor pinned to the caret
    * instead of wherever content last happened to land.
    */
  private def caretEscape(forceReassert: Boolean): String =
    val current  = caretRef.get()
    val previous = lastEmittedCaretRef.getAndSet(Some(current))
    if !forceReassert && previous.contains(current) then ""
    else
      current match
        case Some(caret) =>
          s"${TerminalRenderSurface.Esc}[${caret.cellY + 1};${caret.cellX + 1}H" +
            s"${TerminalRenderSurface.Esc}[${caret.style.decscusrParam} q" +
            s"${TerminalRenderSurface.Esc}[?25h"
        case None =>
          s"${TerminalRenderSurface.Esc}[?25l"

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
  // `cellMetrics` here is always `CellMetrics.cellUnit` (charWidth = lineHeight = 1, see `TuiRuntime`), so every
  // "pixel" coordinate `RenderSurface` callers pass is already a cell coordinate -- no metrics conversion needed, in
  // contrast to `Java2DRenderSurface`'s real pixel scaling.
  //
  // `drawImage` renders through `HalfBlockImageRenderer` (notcurses/ratatui-image/Jexer's technique): the source image
  // is downsampled to twice the target cell grid's vertical resolution, and each cell becomes an upper-half-block
  // glyph (`▀`) whose foreground paints its top sub-pixel and whose background paints its bottom one, doubling the
  // effective vertical resolution a plain ANSI-color terminal can show. Sixel/Kitty/iTerm2 true-pixel protocols are a
  // distinct capability this does not attempt to negotiate.

  // fillPixelRect stays a no-op deliberately, unlike drawImage: Renderer's full-frame caret paint
  // (paintCursorOverlay/similar call sites) calls `pixels.fillPixelRect` with coordinates computed from real font
  // pixel geometry regardless of surface -- correct on `Java2DRenderSurface`, but on this cell surface (`cellMetrics`
  // is always `CellMetrics.cellUnit`) that same call lands on real character cells and, if it painted for real, would
  // silently overwrite live buffer text with a solid caret-colored block every full render (confirmed empirically: it
  // reliably blanked two characters of "alpha" immediately after the gutter on every frame). TUI's real caret is
  // `HardwareCursor` (`present`/`hide`, DECSCUSR/CUP) -- this call is simply never meant to paint cell content here,
  // so it must stay inert the way #1012 originally left it.
  override def fillPixelRect(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: Color): Unit = ()

  override def drawImage(image: BufferedImage, x: Int, y: Int, width: Int, height: Int): Unit =
    val previousForeground = screenBuffer.getForegroundColor
    val previousBackground = screenBuffer.getBackgroundColor
    val grid                = HalfBlockImageRenderer.render(image, width, height, fallback = previousBackground)
    grid.zipWithIndex.foreach { case (row, rowOffset) =>
      row.zipWithIndex.foreach {
        case (Some(cell), colOffset) =>
          screenBuffer.setForegroundColor(cell.foreground)
          screenBuffer.setBackgroundColor(cell.background)
          screenBuffer.putString(x + colOffset, y + rowOffset, HalfBlockImageRenderer.UpperHalfBlock.toString)
        case (None, _) => ()
      }
    }
    screenBuffer.setForegroundColor(previousForeground)
    screenBuffer.setBackgroundColor(previousBackground)

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

  final private case class Caret(cellX: Int, cellY: Int, style: HardwareCursorStyle)

  private val Esc: Char = 0x1b.toChar

  // -- #1172: DEC 2026 synchronized updates -------------------------------------------------------------------------
  private val BeginSyncUpdate: String = s"$Esc[?2026h"
  private val EndSyncUpdate: String   = s"$Esc[?2026l"
