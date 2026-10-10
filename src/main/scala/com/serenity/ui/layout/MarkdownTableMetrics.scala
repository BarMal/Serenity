package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.markdown.MarkdownBlockLens.TableRow
import com.serenity.markdown.MarkdownTableColumns.Columns
import com.serenity.markdown.{MarkdownBlockSpans, MarkdownTableColumns}
import com.serenity.rope.Rope
import com.serenity.ui.layout.TextCaretMeasurement.{
  LineFontResolver,
  markdownResolver,
  normalizeCollapsedCarets,
  rawMeasuredCaretXs
}

/** Measures the cells of Markdown pipe tables so their pipes can be lined up: the width of each column is the widest of
  * its cells in the fonts the rows are painted in, and a row's own padding follows from where its pipes fall.
  */
private[layout] object MarkdownTableMetrics:

  /** The columns of every table in `tables` (line ranges of `content`), keyed by the table's first line. */
  def columnsFor(
    content: Rope,
    tables: Vector[Range.Inclusive],
    font: Font,
    frc: FontRenderContext,
    baseIsMonospaced: Boolean
  ): Map[Int, Columns] =
    tables.map { table =>
      val rows = table.map(line => content.getLine(line).getOrElse("")).toVector
      table.start -> columnsOf(rows, font, frc, baseIsMonospaced)
    }.toMap

  private def columnsOf(rows: Vector[String], font: Font, frc: FontRenderContext, mono: Boolean): Columns =
    val measured = rows.zipWithIndex.collect {
      case (row, index) if index != 1 =>
        val resolver = hiddenMarkerResolver(row, if index == 0 then TableRow.Header else TableRow.Body, font, mono)
        cellWidths(row, resolver, frc)
    }
    val count = measured.map(_.length).maxOption.getOrElse(0)
    Columns(
      Vector.tabulate(count)(column => measured.flatMap(_.lift(column)).maxOption.getOrElse(0.0f)),
      MarkdownTableColumns.alignments(rows.lift(1).getOrElse(""))
    )

  private def hiddenMarkerResolver(row: String, role: TableRow, font: Font, mono: Boolean): LineFontResolver =
    markdownResolver(font, row.length, MarkdownBlockSpans.scan(row, role), hideMarkers = true, baseIsMonospaced = mono)

  /** The width of each cell of `line`, from the pipe before it to the pipe after it, as `resolver` measures it. */
  def cellWidths(line: String, resolver: LineFontResolver, frc: FontRenderContext): Vector[Float] =
    if line.isEmpty then Vector.empty
    else
      val carets = normalizeCollapsedCarets(rawMeasuredCaretXs(line, 0, resolver, frc), resolver, 0)
      MarkdownTableColumns.cells(line).map(cell => carets(cell.end) - carets(cell.start))

  /** `resolver`, the font resolver of a table row, with each cell padded out to its column. */
  def padded(line: String, resolver: LineFontResolver, columns: Columns, frc: FontRenderContext): LineFontResolver =
    val extra =
      MarkdownTableColumns.extraAdvances(line, cellWidths(line, resolver, frc), columns, resolver.isHidden)
    if extra.isEmpty then resolver else resolver.copy(extraAdvances = extra)
