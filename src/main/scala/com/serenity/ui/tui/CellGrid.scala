package com.serenity.ui.tui

import java.awt.Color
import java.util.Arrays

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

import com.serenity.ui.theme.TextStyle

/** What a cell looks like apart from its glyph. Cells hold a small integer naming one of these in a [[StyleTable]], so
  * a screenful of cells shares a handful of colour and style objects instead of owning its own.
  */
final case class CellStyle(fg: Color, bg: Color, text: TextStyle):
  val sgr: SgrState = SgrState.of(fg, bg, text)

/** Interns [[CellStyle]]s: equal styles get one id for the life of the table, so two cells have the same look exactly
  * when their ids are equal. A buffer and the retained previous frame it is diffed against share one table.
  */
final class StyleTable:
  private val styles = mutable.ArrayBuffer.empty[CellStyle]
  private val ids    = mutable.HashMap.empty[CellStyle, Int]

  def intern(fg: Color, bg: Color, text: TextStyle): Int =
    val style = CellStyle(fg, bg, text)
    ids.getOrElseUpdate(style, register(style))

  def apply(id: Int): CellStyle = styles(id)

  private def register(style: CellStyle): Int =
    styles.append(style)
    styles.length - 1

/** The cells of a terminal screen as two flat arrays, row-major: a glyph word (codepoint, with the [[CellSpan]] in the
  * bits above it) and a style id. Writing, clearing a span and comparing rows touch only primitives, so a frame costs
  * what changed in it and a clear is an array fill. The glyph word is the slot a grapheme-cluster pool index would
  * occupy.
  *
  * Mutable by design, like the buffer that owns it (which is the only writer): callers outside this package see
  * [[TerminalFrame]] values built by [[toFrame]].
  */
final class CellGrid(val width: Int, val height: Int, val styles: StyleTable, blankStyleId: Int):
  import CellGrid.*

  private val glyphs   = new Array[Int](width * height)
  private val styleIds = new Array[Int](width * height)
  // The combining marks composed onto each cell's glyph, as strings: the one shared `NoMarks`, which nearly every cell
  // holds, for none, so that a cell without marks is known by its identity alone.
  private val marks = Array.fill[AnyRef](width * height)(NoMarks)
  Arrays.fill(glyphs, Blank)
  Arrays.fill(styleIds, blankStyleId)

  def glyphAt(x: Int, y: Int): Int       = glyphs(y * width + x)
  def styleIdAt(x: Int, y: Int): Int     = styleIds(y * width + x)
  def styleAt(x: Int, y: Int): CellStyle = styles(styleIdAt(x, y))
  def spanAt(x: Int, y: Int): CellSpan   = spanOf(glyphAt(x, y))
  def marksAt(x: Int, y: Int): String    = marks(y * width + x).toString

  /** Composes `extra` marks onto the glyph already in the cell. */
  def appendMarks(x: Int, y: Int, extra: String): Unit =
    val index = y * width + x
    marks(index) = if marks(index) eq NoMarks then extra else marks(index).toString + extra

  /** Stores the cell, marks cleared, and says whether that altered it. */
  def set(x: Int, y: Int, glyph: Int, styleId: Int): Boolean =
    val index   = y * width + x
    val changed = glyphs(index) != glyph || styleIds(index) != styleId || (marks(index) ne NoMarks)
    glyphs(index) = glyph
    styleIds(index) = styleId
    marks(index) = NoMarks
    changed

  /** Fills columns `from` until `until` of row `y`, and says whether any cell in it changed. */
  def fillSpan(y: Int, from: Int, until: Int, glyph: Int, styleId: Int): Boolean =
    val start   = y * width + from
    val end     = y * width + until
    val changed = !uniform(start, end, glyph, styleId)
    if changed then
      Arrays.fill(glyphs, start, end, glyph)
      Arrays.fill(styleIds, start, end, styleId)
      Arrays.fill(marks, start, end, NoMarks)
    changed

  def copyRowFrom(source: CellGrid, y: Int): Unit =
    System.arraycopy(source.glyphs, y * width, glyphs, y * width, width)
    System.arraycopy(source.styleIds, y * width, styleIds, y * width, width)
    System.arraycopy(source.marks, y * width, marks, y * width, width)

  def duplicate: CellGrid =
    val grid = new CellGrid(width, height, styles, blankStyleId)
    System.arraycopy(glyphs, 0, grid.glyphs, 0, glyphs.length)
    System.arraycopy(styleIds, 0, grid.styleIds, 0, styleIds.length)
    System.arraycopy(marks, 0, grid.marks, 0, marks.length)
    grid

  /** Whether row `y` of this grid shows exactly what row `otherY` of `other` does. Both must share a [[StyleTable]]. */
  def rowEquals(y: Int, other: CellGrid, otherY: Int): Boolean =
    val start      = y * width
    val otherStart = otherY * width
    Arrays.equals(glyphs, start, start + width, other.glyphs, otherStart, otherStart + width) &&
    Arrays.equals(styleIds, start, start + width, other.styleIds, otherStart, otherStart + width) &&
    Arrays.equals(marks, start, start + width, other.marks, otherStart, otherStart + width)

  def rowHash(y: Int): Int = hashFrom(y * width, (y + 1) * width, 1)

  /** The columns of row `y` that differ from row `otherY` of `other`, ascending. */
  def differingColumns(y: Int, other: CellGrid, otherY: Int): Seq[Int] =
    if rowEquals(y, other, otherY) then Nil
    else
      val columns = Array.newBuilder[Int]
      collectDifferences(y * width, other, otherY * width, 0, columns)
      ArraySeq.unsafeWrapArray(columns.result())

  def toFrame: TerminalFrame =
    TerminalFrame(width, height, Vector.tabulate(height)(y => Vector.tabulate(width)(x => cellAt(x, y))))

  def cellAt(x: Int, y: Int): TerminalCell =
    val style = styleAt(x, y)
    TerminalCell(codePointOf(glyphAt(x, y)), style.fg, style.bg, style.text, spanAt(x, y), marksAt(x, y))

  @tailrec private def uniform(index: Int, end: Int, glyph: Int, styleId: Int): Boolean =
    if index >= end then true
    else if glyphs(index) != glyph || styleIds(index) != styleId || (marks(index) ne NoMarks) then false
    else uniform(index + 1, end, glyph, styleId)

  @tailrec private def hashFrom(index: Int, end: Int, acc: Int): Int =
    if index >= end then acc
    else hashFrom(index + 1, end, 31 * (31 * (31 * acc + glyphs(index)) + styleIds(index)) + marks(index).hashCode)

  @tailrec private def collectDifferences(
    start: Int,
    other: CellGrid,
    otherStart: Int,
    column: Int,
    into: mutable.ArrayBuilder[Int]
  ): Unit =
    if column < width then
      if glyphs(start + column) != other.glyphs(otherStart + column) ||
          styleIds(start + column) != other.styleIds(otherStart + column) ||
          !marks(start + column).equals(other.marks(otherStart + column))
      then
        val _ = into.addOne(column)
      collectDifferences(start, other, otherStart, column + 1, into)

object CellGrid:

  /** The marks of a cell that has none. */
  private val NoMarks: String = ""

  private val SpanShift = 24

  def pack(codePoint: Int, span: CellSpan): Int = codePoint | (span.ordinal << SpanShift)

  def codePointOf(glyph: Int): Int = glyph & ((1 << SpanShift) - 1)

  def spanOf(glyph: Int): CellSpan = CellSpan.fromOrdinal(glyph >>> SpanShift)

  val Blank: Int = pack(' '.toInt, CellSpan.Narrow)

  def fromFrame(frame: TerminalFrame, styles: StyleTable): CellGrid =
    val blankStyle = styles.intern(Color.WHITE, Color.BLACK, TextStyle.normal)
    val grid       = new CellGrid(frame.width, frame.height, styles, blankStyle)
    for
      y <- 0 until frame.height
      x <- 0 until frame.width
    do
      val cell = frame(x, y)
      val _    = grid.set(x, y, pack(cell.codePoint, cell.span), styles.intern(cell.fg, cell.bg, cell.style))
      if cell.marks.nonEmpty then grid.appendMarks(x, y, cell.marks)
    grid
