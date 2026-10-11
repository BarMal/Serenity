package com.serenity.markdown

import com.serenity.markdown.MarkdownBlockLens.SetextHeading

/** One pass over a document's lines, as far as it has got: the fenced blocks, tables and setext headings found so far,
  * and the constructs still open at its edge. Whatever it records about the lines already read depends on them alone,
  * so a scan kept at a line can be resumed from there on another text that shares those lines.
  */
final private[markdown] case class BlockScan(
    closedFences: Vector[Range.Inclusive],
    openFence: Option[Int],
    tables: Vector[Range.Inclusive],
    header: Option[Int],
    tableStart: Option[Int],
    lineCount: Int,
    paragraphStart: Option[Int],
    setext: Vector[SetextHeading]
):

  /** The first line a construct still open at the scan's edge may yet take in; every line before it is settled. */
  def settledBefore: Int =
    Seq(openFence, tableStart, header, paragraphStart).flatten.minOption.getOrElse(lineCount)

  /** The scan once no more lines follow: a table running to the last line is closed there, a fence left open is not a
    * block.
    */
  def finished: BlockScan = closeTable(lineCount - 1)

  def closeTable(lastLine: Int): BlockScan =
    tableStart.fold(this)(start => copy(tables = tables :+ (start to lastLine), tableStart = None))

  def next(line: String): BlockScan =
    val index = lineCount
    val seen  = copy(lineCount = lineCount + 1)
    if MarkdownBlockLens.isFenceLine(line) then
      seen
        .closeTable(index - 1)
        .copy(
          closedFences = openFence.fold(closedFences)(open => closedFences :+ (open to index)),
          openFence = Option.when(openFence.isEmpty)(index),
          header = None,
          paragraphStart = None
        )
    else if openFence.isDefined then seen
    else if tableStart.isDefined && MarkdownInlineTablePreview.isTableRow(line) then seen
    else seen.closeTable(index - 1).candidate(line, index).paragraphs(line, index)

  private def paragraphs(line: String, index: Int): BlockScan =
    paragraphStart match
      case _ if tableStart.isDefined => copy(paragraphStart = None)
      case Some(start) if MarkdownBlockLens.isSetextUnderline(line) =>
        val level = if line.trim.startsWith("=") then 1 else 2
        copy(setext = setext :+ SetextHeading(start to index, level), paragraphStart = None)
      case _ if MarkdownBlockLens.isParagraphLine(line) => copy(paragraphStart = paragraphStart.orElse(Some(index)))
      case _                                            => copy(paragraphStart = None)

  private def candidate(line: String, index: Int): BlockScan =
    if line.indexOf('|') < 0 then copy(header = None)
    else if header.isDefined && MarkdownInlineTablePreview.isTableSeparator(line) then
      copy(tableStart = header, header = None)
    else copy(header = Option.when(MarkdownInlineTablePreview.isTableRow(line))(index))

private[markdown] object BlockScan:
  val empty: BlockScan = BlockScan(Vector.empty, None, Vector.empty, None, None, 0, None, Vector.empty)
