package com.serenity.spike

import java.awt.Font as AwtFont

import com.serenity.ui.layout.{SpikeLayoutBridge, TextLayoutSnapshot}

/** One wrapped visual row of a logical line. */
final case class Row(start: Int, end: Int, text: String)

/** The editor geometry the spike draws, in logical pixels (the frame is drawn at `scale` device pixels per logical). */
object Geometry:
  val LogicalWidth: Int  = 1500
  val LogicalHeight: Int = 1000
  val GutterWidth: Float = 64f
  val TextLeft: Float    = GutterWidth + 24f
  val WrapWidthPx: Int   = (LogicalWidth - TextLeft - 40f).toInt
  /** Room right of `TextLeft` for a wrapped row; painted rows are narrower than the wrap width (see the parity notes). */
  val TextAreaWidth: Float = LogicalWidth - TextLeft
  val FontSize: Float    = 14f
  val LineHeight: Float  = 21f
  val Baseline: Float    = 15f

/** Immutable editor state: lines, their Serenity-wrapped rows, and a caret the viewport keeps vertically centred.
  * Wrapping uses Serenity's own `TextLayoutSnapshot.wrapRows` (Java2D `GlyphAdvances` measurement); Skia only paints.
  */
final case class EditorState(
    lines: Vector[String],
    rows: Vector[Vector[Row]],
    rowStarts: IArray[Int],
    caretLine: Int,
    caretColumn: Int,
    generation: Long,
    scrollRows: Int = 0
):
  def totalRows: Int = rowStarts(rowStarts.length - 1)

  def caretRowInLine: Int =
    val lineRows = rows(caretLine)
    val index    = lineRows.lastIndexWhere(_.start <= caretColumn)
    math.max(0, index)

  def caretGlobalRow: Int = rowStarts(caretLine) + caretRowInLine

  /** The global row drawn on the viewport's centre band: the caret's row, shifted by any scrolling since. */
  def anchorRow: Int = caretGlobalRow + scrollRows

  /** The view moved by `rows` (positive: down the document), clamped to the document; the caret stays where it is. */
  def scrolled(rows: Int): EditorState =
    val anchor = math.max(0, math.min(totalRows - 1, anchorRow + rows))
    copy(scrollRows = anchor - caretGlobalRow)

  /** The line and row index of a global visual row, by binary search over `rowStarts`. */
  def locate(globalRow: Int): (Int, Int) =
    @annotation.tailrec
    def lastLineStartingAtOrBefore(lo: Int, hi: Int): Int =
      if lo >= hi then lo
      else
        val mid = (lo + hi + 1) >>> 1
        if rowStarts(mid) <= globalRow then lastLineStartingAtOrBefore(mid, hi)
        else lastLineStartingAtOrBefore(lo, mid - 1)
    val line = lastLineStartingAtOrBefore(0, lines.length - 1)
    (line, globalRow - rowStarts(line))

  /** Insert `char` at the caret and re-wrap only the caret's line, as Serenity's incremental path would. */
  def insert(char: Char, wrap: (String, Int) => Vector[Row]): EditorState =
    val line    = lines(caretLine)
    val updated = line.substring(0, caretColumn) + char + line.substring(caretColumn)
    val newRows = rows.updated(caretLine, wrap(updated, caretLine))
    copy(
      lines = lines.updated(caretLine, updated),
      rows = newRows,
      rowStarts = EditorState.prefix(newRows),
      caretColumn = caretColumn + 1,
      generation = generation + 1
    )

object EditorState:

  def prefix(rows: Vector[Vector[Row]]): IArray[Int] =
    IArray.unsafeFromArray(rows.scanLeft(0)(_ + _.length).toArray)

  def initial(lines: Vector[String], wrap: (String, Int) => Vector[Row]): EditorState =
    val rows      = lines.zipWithIndex.map((text, index) => wrap(text, index))
    val caretLine = lines.indices.find(i => i >= lines.length / 2 && lines(i).nonEmpty).getOrElse(0)
    EditorState(lines, rows, prefix(rows), caretLine, lines(caretLine).length / 2, 0L)

/** Serenity's wrap with Serenity's prose font (`Serif` 14) and default render context. */
final class SerenityWrap:
  val font: AwtFont = AwtFont(AwtFont.SERIF, AwtFont.PLAIN, Geometry.FontSize.toInt)
  private val frc   = TextLayoutSnapshot.defaultFontRenderContext()

  def apply(text: String, line: Int): Vector[Row] =
    SpikeLayoutBridge
      .wrapLine(text, line, Geometry.WrapWidthPx, font, frc)
      .map(visual => Row(visual.startColumn, visual.endColumn, visual.text))
