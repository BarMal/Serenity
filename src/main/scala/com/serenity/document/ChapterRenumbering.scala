package com.serenity.document

import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.Buffer

/** Live chapter-heading renumbering for Markdown prose, one of Neo's quality-of-life features: a heading already shaped
  * like "Chapter <number>" (optionally followed by ": Title") gets its number kept in sequence -- 1, 2, 3, ... in
  * document order -- as chapters are added, removed, or reordered. A heading that isn't already labelled as a chapter
  * is never turned into one; this only resequences numbers that are already there.
  *
  * Markdown-only for now (format-bound): rich-text (RTF/ODT/DOCX) heading paragraphs need their own edit path through
  * `RichTextDocument` rather than a plain-text offset, and are a separate follow-up.
  */
object ChapterRenumbering:

  private val HeadingLine = """^(#{1,6})\s+[Cc]hapter\s+(\d+)(.*)$""".r

  /** The `(startOffset, endOffset, replacementNumber)` edits needed to put every chapter heading's number back in
    * sequence, in document order, empty when nothing needs to change (already in sequence, no chapter headings, or the
    * buffer isn't Markdown). Only the digits themselves are ever replaced -- the `#` markers, the word "Chapter", and
    * any trailing title text are left exactly as they were.
    */
  def pendingRenumbers(buffer: Buffer): List[(Int, Int, String)] =
    if buffer.document.language.contains(LanguageId.Markdown) then
      chapterHeadingLines(buffer)
        .zip(LazyList.from(1))
        .collect {
          case ((line, numberStart, numberEnd, currentNumber), expected) if currentNumber != expected.toString =>
            val lineStart = buffer.document.content.lineColumnToOffset(line, 0)
            (lineStart + numberStart, lineStart + numberEnd, expected.toString)
        }
    else Nil

  /** Every line that looks like a chapter heading, in document order, as `(line, numberStartColumn, numberEndColumn,
    * currentNumberText)`. A fenced code block containing a line that merely looks like a heading is not filtered out
    * here (unlike `DocumentOutline`'s AST walk) -- true positives inside code fences are rare for a "Chapter N" line
    * specifically, and the cost of getting this wrong is a number that gets put back in sequence inside a code block,
    * not a lost heading.
    */
  private def chapterHeadingLines(buffer: Buffer): List[(Int, Int, Int, String)] =
    buffer.document.content
      .linesIteratorFrom(0)
      .flatMap {
        case (line, text) =>
          HeadingLine.findFirstMatchIn(text).map(m => (line, m.start(2), m.end(2), m.group(2)))
      }
      .toList
