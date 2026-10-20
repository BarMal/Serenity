package com.serenity.ui.tui

import java.awt.Color
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import scala.annotation.tailrec

import com.serenity.ui.layout.CharWidth
import com.serenity.ui.theme.TextStyle

/** A cell grid for the terminal backend: a glyph and a style per cell, filled via the same `putString`/`fillRect`/
  * colour-and-style-state shape `RenderSurface` already exposes, so `TerminalRenderSurface` (a later issue) can forward
  * straight onto it.
  *
  * The cells live in two [[CellGrid]]s, the live one and the one the terminal was last brought to (#1890, the shape
  * notcurses and ratatui use): [[flushDiff]] encodes only what differs between them and then copies just the rows that
  * were written to across, so a flush costs what changed rather than the size of the screen. Internally mutable,
  * encapsulated here -- callers see [[TerminalFrame]] values from [[snapshot]], never the backing grids.
  */
final class TerminalScreenBuffer(val width: Int, val height: Int):
  import TerminalScreenBuffer.Clip

  private val fgColorRef     = AtomicReference[Color](Color.WHITE)
  private val bgColorRef     = AtomicReference[Color](Color.BLACK)
  private val activeStyleRef = AtomicReference[TextStyle](TextStyle.normal)
  private val clipRef        = AtomicReference[Clip](Clip(0, 0, width, height))

  private val styles = new StyleTable
  // The style a write gets, re-interned whenever the colours or the active style change rather than once per cell.
  private val penStyleId = new AtomicInteger(styles.intern(Color.WHITE, Color.BLACK, TextStyle.normal))

  private val grid: CellGrid = new CellGrid(width, height, styles, penStyleId.get())

  // What the terminal currently shows -- `None` until the first flush has painted the whole screen.
  private val flushed = new AtomicReference[Option[CellGrid]](None)

  // Rows actually changed (not just written to -- see `writeCell`) since the last `consumeDirtyRows` call. Every
  // paint path (`putString`, `fillRect`, the wide-glyph orphan repair `writeCell` triggers) writes through here, so
  // this is a complete, always-correct record of "what changed" for `TerminalAnsiDiff` to scan (#1464) -- unlike the
  // renderer's own `FramePlan.dirtyRowsByPane`, which only ever covers editor pane content, never chrome, gutter,
  // panels or the modal layer. A row not in it still holds what it held at the last flush, which is what lets
  // `flushDiff` bring the flushed grid up to date by copying only the rows in it.
  private val dirtyRows: java.util.BitSet = new java.util.BitSet(height)

  def setForegroundColor(color: Color): Unit =
    fgColorRef.set(color)
    refreshPen()

  def setBackgroundColor(color: Color): Unit =
    bgColorRef.set(color)
    refreshPen()

  def getForegroundColor: Color = fgColorRef.get()
  def getBackgroundColor: Color = bgColorRef.get()

  def enableStyle(style: TextStyle): Unit =
    activeStyleRef.set(activeStyleRef.get().combine(style))
    refreshPen()

  def disableStyle(style: TextStyle): Unit =
    val current = activeStyleRef.get()
    activeStyleRef.set(
      TextStyle(
        isBold = current.isBold && !style.isBold,
        isItalic = current.isItalic && !style.isItalic,
        isUnderlined = current.isUnderlined && !style.isUnderlined,
        fontFamily = current.fontFamily,
        fontSize = current.fontSize
      )
    )
    refreshPen()

  /** Restrict [[putString]] and [[fillRect]] to a rectangular region in cell coordinates for the duration of `body`. */
  def withClip[A](x: Int, y: Int, w: Int, h: Int)(body: => A): A =
    val previous = clipRef.get()
    clipRef.set(Clip(x, y, x + w, y + h).intersect(previous))
    try body
    finally clipRef.set(previous)

  def putString(x: Int, y: Int, s: String): Unit = putFrom(s, 0, x, y, -1)

  /** Fills the rectangle with `char` in the current colours: one array fill per row, however many cells it covers. */
  def fillRect(x: Int, y: Int, w: Int, h: Int, char: Char): Unit =
    val clip = clipRef.get()
    val from = math.max(math.max(x, clip.x0), 0)
    val to   = math.min(math.min(x + w, clip.x1), width)
    if from < to then
      (math.max(math.max(y, clip.y0), 0) until math.min(math.min(y + h, clip.y1), height))
        .foreach(row => fillSpan(row, from, to, CellGrid.pack(char.toInt, CellSpan.Narrow)))

  def backgroundAt(x: Int, y: Int): Option[Color] = Option.when(inBounds(x, y))(grid.styleAt(x, y).bg)

  def snapshot: TerminalFrame = grid.toFrame

  /** The ANSI that brings the terminal from what the last call left it showing to what is in the buffer now (a full
    * repaint the first time), after which the buffer remembers the new state. Only the rows written to since the last
    * call are scanned, and only they are copied into the remembered grid.
    */
  def flushDiff(scrollRegions: Boolean, startPen: Option[SgrState]): TerminalAnsiDiff.Encoded =
    val touched  = consumeDirtyRows()
    val previous = flushed.get()
    val encoded  = TerminalAnsiDiff.encodeGrids(previous, grid, Some(touched), scrollRegions, startPen)
    previous match
      case Some(shown) => touched.foreach(shown.copyRowFrom(grid, _))
      case None        => flushed.set(Some(grid.duplicate))
    encoded

  /** Rows written with a value different from what they already held, since the last call to this method -- then reset
    * for the next one. See `dirtyRows`'s own doc for why this is the complete, correct source of "what changed" rather
    * than a re-derived or partial one.
    */
  def consumeDirtyRows(): Set[Int] =
    val touched = dirtyRows.stream().toArray.toSet
    dirtyRows.clear()
    touched

  private def refreshPen(): Unit =
    penStyleId.set(styles.intern(fgColorRef.get(), bgColorRef.get(), activeStyleRef.get()))

  private def blankStyleId: Int = styles.intern(fgColorRef.get(), bgColorRef.get(), TextStyle.normal)

  private def inBounds(x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height

  /** `base` is the column of the last character this call wrote, the one a combining mark composes onto, or -1 when
    * there is none (a mark with no base before it, or whose base was clipped away, is left out rather than borrowing a
    * cell another call wrote).
    */
  @tailrec private def putFrom(s: String, index: Int, col: Int, y: Int, base: Int): Unit =
    if index < s.length then
      val codePoint  = s.codePointAt(index)
      val glyphWidth = CharWidth.of(codePoint)
      val next       = index + Character.charCount(codePoint)
      if glyphWidth == 0 then
        if base >= 0 then attachMark(base, y, codePoint)
        putFrom(s, next, col, y, base)
      else
        if glyphWidth == 2 then
          writeCell(col, y, CellGrid.pack(codePoint, CellSpan.Wide), penStyleId.get())
          writeCell(col + 1, y, CellGrid.pack(' '.toInt, CellSpan.Continuation), penStyleId.get())
        else writeCell(col, y, CellGrid.pack(codePoint, CellSpan.Narrow), penStyleId.get())
        putFrom(s, next, col + glyphWidth, y, if inBounds(col, y) && clipRef.get().contains(col, y) then col else -1)

  private def attachMark(x: Int, y: Int, codePoint: Int): Unit =
    grid.appendMarks(x, y, String(Character.toChars(codePoint)))
    dirtyRows.set(y)

  /** Overwrite one cell, then repair whichever half of a wide glyph the overwrite just orphaned: if the cell replaced
    * was a [[CellSpan.Wide]] leader, its continuation to the right no longer has a leader; if it was a
    * [[CellSpan.Continuation]], its leader to the left no longer has a continuation. Either way the leftover half is
    * blanked rather than left to render a dangling glyph fragment.
    */
  private def writeCell(x: Int, y: Int, glyph: Int, styleId: Int): Unit =
    if inBounds(x, y) && clipRef.get().contains(x, y) then
      val replaced = grid.spanAt(x, y)
      val changed  = grid.set(x, y, glyph, styleId)
      if changed then dirtyRows.set(y)
      val span = CellGrid.spanOf(glyph)
      if replaced == CellSpan.Wide && span != CellSpan.Wide then blankIfContinuation(x + 1, y)
      if replaced == CellSpan.Continuation && span != CellSpan.Continuation then blankIfWideLeader(x - 1, y)

  /** The bulk form of [[writeCell]] for a run of narrow cells on one row: a wide glyph cut by either end of the run
    * loses its other half exactly as it would cell by cell; halves inside the run are overwritten with it.
    */
  private def fillSpan(row: Int, from: Int, to: Int, glyph: Int): Unit =
    val firstWasContinuation = grid.spanAt(from, row) == CellSpan.Continuation
    val lastWasWide          = grid.spanAt(to - 1, row) == CellSpan.Wide
    if grid.fillSpan(row, from, to, glyph, penStyleId.get()) then dirtyRows.set(row)
    if firstWasContinuation then blankIfWideLeader(from - 1, row)
    if lastWasWide then blankIfContinuation(to, row)

  // Routed through `writeCell` itself -- rather than writing `grid` directly -- so there is exactly one funnel point
  // for both dirty-row tracking and the active clip, instead of three hand-synchronized write sites.
  private def blankIfContinuation(x: Int, y: Int): Unit =
    if inBounds(x, y) && grid.spanAt(x, y) == CellSpan.Continuation then writeCell(x, y, CellGrid.Blank, blankStyleId)

  private def blankIfWideLeader(x: Int, y: Int): Unit =
    if inBounds(x, y) && grid.spanAt(x, y) == CellSpan.Wide then writeCell(x, y, CellGrid.Blank, blankStyleId)

object TerminalScreenBuffer:

  final private case class Clip(x0: Int, y0: Int, x1: Int, y1: Int):
    def contains(x: Int, y: Int): Boolean = x >= x0 && x < x1 && y >= y0 && y < y1
    def intersect(other: Clip): Clip =
      Clip(math.max(x0, other.x0), math.max(y0, other.y0), math.min(x1, other.x1), math.min(y1, other.y1))
