package com.serenity.markdown

import com.serenity.markdown.MarkdownDocumentPreview.InlinePreviewLine

/** Renders a run of Markdown table source lines as a closed box-drawing table for the inline Lens, and identifies
  * which source lines make up such a run. Kept separate from the rest of the inline preview pipeline because table
  * layout (column widths, borders) is its own self-contained concern.
  */
private[markdown] object MarkdownInlineTablePreview:

  final case class InlineTableBlock(endIndex: Int, previewLines: Vector[InlinePreviewLine])

  def tableBlockAt(lines: Vector[String], index: Int): Option[InlineTableBlock] =
    Option
      .when(index + 1 < lines.length && isTableRow(lines(index)) && isTableSeparator(lines(index + 1))) {
        val rows = Iterator
          .iterate(index)(_ + 1)
          .takeWhile(lineIndex => lineIndex < lines.length && isTableRow(lines(lineIndex)))
          .toVector
        // rows always includes `index` itself (isTableRow(lines(index)) holds per the Option.when guard),
        // so lastOption is always Some; index is the safe fallback for the unreachable None branch.
        val endIndex     = rows.lastOption.getOrElse(index)
        val renderedRows = renderInlineTable(rows.map(lines))
        InlineTableBlock(endIndex, sourceMappedTableRows(rows, renderedRows))
      }
      .filter(_.previewLines.nonEmpty)

  private def sourceMappedTableRows(sourceRows: Vector[Int], renderedRows: Vector[String]): Vector[InlinePreviewLine] =
    renderedRows.zipWithIndex.map {
      case (text, 0) =>
        InlinePreviewLine(None, text)
      case (text, 1) =>
        InlinePreviewLine(sourceRows.headOption, text)
      case (text, 2) =>
        InlinePreviewLine(sourceRows.lift(1), text)
      case (text, rowIndex) if rowIndex == renderedRows.length - 1 =>
        InlinePreviewLine(None, text)
      case (text, rowIndex) =>
        InlinePreviewLine(sourceRows.lift(rowIndex - 1), text)
    }

  private def renderInlineTable(lines: Vector[String]): Vector[String] =
    val parsedRows = lines.map(parseTableCells)
    val contentRows =
      parsedRows.zipWithIndex.collect {
        case (cells, index) if index != 1 => cells.map(MarkdownDocumentPreview.normalizeInline)
      }
    val columnCount = contentRows.map(_.length).maxOption.getOrElse(0)
    if columnCount == 0 then Vector.empty
    else
      val widths = (0 until columnCount).map { column =>
        contentRows.flatMap(_.lift(column)).map(_.length).maxOption.getOrElse(0)
      }.toVector

      contentRows.zipWithIndex.flatMap {
        case (cells, 0) =>
          Vector(tableBorder(widths, "┌", "┬", "┐"), boxedTableRow(cells, widths))
        case (cells, _) =>
          Vector(boxedTableRow(cells, widths))
      } match
        case rows if rows.nonEmpty =>
          rows.take(2) ++
            Vector(tableBorder(widths, "├", "┼", "┤")) ++
            rows.drop(2) ++
            Vector(tableBorder(widths, "└", "┴", "┘"))
        case _ =>
          Vector.empty

  private def boxedTableRow(cells: Vector[String], widths: Vector[Int]): String =
    widths.zipWithIndex
      .map { case (width, index) => s" ${cells.lift(index).getOrElse("").padTo(width, ' ')} " }
      .mkString("│", "│", "│")

  private def tableBorder(widths: Vector[Int], left: String, separator: String, right: String): String =
    widths
      .map(width => "─" * (width + 2).max(3))
      .mkString(left, separator, right)

  private def parseTableCells(line: String): Vector[String] =
    val trimmed           = line.trim
    val withoutOuterPipes = trimmed.stripPrefix("|").stripSuffix("|")
    withoutOuterPipes.split("\\|", -1).toVector.map(_.trim)

  def isTableRow(line: String): Boolean =
    val trimmed = line.trim
    trimmed.contains("|") && parseTableCells(trimmed).length >= 2

  def isTableSeparator(line: String): Boolean =
    isTableRow(line) && parseTableCells(line).forall(cell => cell.matches(""":?-{3,}:?"""))
