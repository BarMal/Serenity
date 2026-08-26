package com.serenity.state.manager

import com.serenity.lsp.config.LanguageId
import com.serenity.markdown.MarkdownBlockLens
import com.serenity.state.models.Buffer

/** The contiguous range of buffer lines focused-text-body dimming treats as "active" around a given line -- shared by
  * `Renderer` (which uses it to decide which rows to dim) and `DamageProducer` (which uses it to detect when the range
  * itself moves, so that a cursor crossing a paragraph boundary reports damage for every row whose dimmed state
  * flipped, not just the old and new cursor row). Kept as one implementation so the two can never drift apart.
  */
object FocusedTextBody:

  private val MarkdownFenceProbeWindow  = 512
  private val MarkdownFenceProbeMaximum = 8_192

  /** `None` means every line in the buffer is active (nothing would be dimmed) -- either there is no active line, or
    * the active line falls outside the buffer's current extent.
    */
  def activeRange(buffer: Buffer, activeLine: Option[Int]): Option[Range.Inclusive] =
    if buffer.language.contains(LanguageId.Markdown) then markdownRange(buffer, activeLine)
    else plainTextRange(buffer, activeLine)

  private def markdownRange(buffer: Buffer, activeLine: Option[Int]): Option[Range.Inclusive] =
    activeLine
      .filter(line => line >= 0 && line < buffer.content.lineCount)
      .map(line => markdownBlock(buffer, line))

  /** The markdown block surrounding `line`, exposed separately since callers with a definite line in hand (rather than
    * an `Option`, e.g. one already known to be visible) skip straight to this rather than going through
    * [[activeRange]].
    */
  def markdownBlock(buffer: Buffer, line: Int): Range.Inclusive =
    val bounded = MarkdownBlockLens.currentBlock(
      buffer.content.lineCount,
      buffer.content.getLine,
      line,
      fenceProbeWindow = MarkdownFenceProbeWindow
    )
    def resolve(window: Int, range: Range.Inclusive): Range.Inclusive =
      val firstProbeLine = (line - window).max(0)
      val lastProbeLine  = (line + window).min(buffer.content.lineCount - 1)
      val probeBounded =
        (firstProbeLine > 0 && range.start == firstProbeLine) ||
          (lastProbeLine < buffer.content.lineCount - 1 && range.end == lastProbeLine)
      if !probeBounded || window >= buffer.content.lineCount || window >= MarkdownFenceProbeMaximum then range
      else
        val nextWindow = (window * 2).min(buffer.content.lineCount).min(MarkdownFenceProbeMaximum)
        val expanded = MarkdownBlockLens.currentBlock(
          buffer.content.lineCount,
          buffer.content.getLine,
          line,
          fenceProbeWindow = nextWindow
        )
        resolve(nextWindow, expanded)

    resolve(MarkdownFenceProbeWindow, bounded)

  private def plainTextRange(buffer: Buffer, activeLine: Option[Int]): Option[Range.Inclusive] =
    activeLine
      .filter(line => line >= 0 && line < buffer.content.lineCount)
      .map { line =>
        val start = Iterator
          .iterate(line)(_ - 1)
          .takeWhile(index => index >= 0 && buffer.content.getLine(index).exists(_.trim.nonEmpty))
          .foldLeft(line)((_, index) => index)
        val end = Iterator
          .iterate(line + 1)(_ + 1)
          .takeWhile(index => index < buffer.content.lineCount && buffer.content.getLine(index).exists(_.trim.nonEmpty))
          .foldLeft(line)((_, index) => index)
        start to end
      }
