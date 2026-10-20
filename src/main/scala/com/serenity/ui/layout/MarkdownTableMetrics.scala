package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext
import java.util.concurrent.ConcurrentHashMap

import com.serenity.markdown.MarkdownBlockLens.TableRow
import com.serenity.markdown.MarkdownTableColumns.Columns
import com.serenity.markdown.{MarkdownBlockSpans, MarkdownTableColumns, TableColumns}
import com.serenity.rope.Rope
import com.serenity.ui.layout.TextCaretMeasurement.{
  LineFontResolver,
  markdownResolver,
  normalizeCollapsedCarets,
  rawMeasuredCaretXs
}
import com.serenity.ui.renderer.ShapedTextCache
import com.serenity.ui.theme.HeadingSettings

/** Measures the cells of Markdown pipe tables so their pipes can be lined up: the width of each column is the widest of
  * its cells in the fonts the rows are painted in, and a row's own padding follows from where its pipes fall.
  */
private[layout] object MarkdownTableMetrics:

  /** The columns of the tables of `content`, each measured when first asked for and then kept. */
  def columnsFor(
    content: Rope,
    font: Font,
    frc: FontRenderContext,
    baseIsMonospaced: Boolean,
    shapedText: ShapedTextCache
  ): TableColumns =
    val measured = new ConcurrentHashMap[Int, Columns]
    table =>
      Some(
        measured.computeIfAbsent(
          table.start,
          _ => columnsOfTable(content, table, font, frc, baseIsMonospaced, shapedText)
        )
      )

  private def columnsOfTable(
    content: Rope,
    table: Range.Inclusive,
    font: Font,
    frc: FontRenderContext,
    baseIsMonospaced: Boolean,
    shapedText: ShapedTextCache
  ): Columns =
    columnsOf(table.map(line => content.getLine(line).getOrElse("")).toVector, font, frc, baseIsMonospaced, shapedText)

  private def columnsOf(
    rows: Vector[String],
    font: Font,
    frc: FontRenderContext,
    mono: Boolean,
    shapedText: ShapedTextCache
  ): Columns =
    val measured = rows.zipWithIndex.collect {
      case (row, index) if index != 1 =>
        val resolver = hiddenMarkerResolver(row, if index == 0 then TableRow.Header else TableRow.Body, font, mono)
        cellWidths(row, resolver, frc, shapedText)
    }
    val count = measured.map(_.length).maxOption.getOrElse(0)
    Columns(
      Vector.tabulate(count)(column => measured.flatMap(_.lift(column)).maxOption.getOrElse(0.0f)),
      MarkdownTableColumns.alignments(rows.lift(1).getOrElse(""))
    )

  private def hiddenMarkerResolver(row: String, role: TableRow, font: Font, mono: Boolean): LineFontResolver =
    markdownResolver(
      font,
      row.length,
      MarkdownBlockSpans.scan(row, role),
      hideMarkers = true,
      baseIsMonospaced = mono,
      headings = HeadingSettings.default
    )

  /** The width of each cell of `line`, from the pipe before it to the pipe after it, as `resolver` measures it. */
  def cellWidths(
    line: String,
    resolver: LineFontResolver,
    frc: FontRenderContext,
    shapedText: ShapedTextCache
  ): Vector[Float] =
    if line.isEmpty then Vector.empty
    else
      val carets = normalizeCollapsedCarets(rawMeasuredCaretXs(line, 0, resolver, frc, shapedText), resolver, 0)
      MarkdownTableColumns.cells(line).map(cell => carets(cell.end) - carets(cell.start))

  /** `resolver`, the font resolver of a table row, with each cell padded out to its column. */
  def padded(
    line: String,
    resolver: LineFontResolver,
    columns: Columns,
    frc: FontRenderContext,
    shapedText: ShapedTextCache
  ): LineFontResolver =
    val extra =
      MarkdownTableColumns.extraAdvances(
        line,
        cellWidths(line, resolver, frc, shapedText),
        columns,
        resolver.isHidden
      )
    if extra.isEmpty then resolver else resolver.copy(extraAdvances = extra)
