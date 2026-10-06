package com.serenity.spellcheck

import scala.annotation.tailrec

import com.serenity.lsp.model.{Diagnostic, LspPosition, LspRange}
import com.serenity.rope.{Balance, Rope, RopeDiff}
import com.serenity.state.models.*

/** Checks a document by reading again only the lines an edit touched.
  *
  * `RopeDiff` names the span of the edited rope that may differ from the one last checked without reading either's
  * text, and the lines in it are read from the rope one at a time. Reading stops at the first line past the edit that
  * opens in the region it opened in before: the lines from there on are the same text in the same region, so what was
  * found in them is carried over, moved to its new line number. An edit that opens or closes a fence or front matter
  * changes the region of everything until the next one, and those lines are read too.
  *
  * The result is the same as checking the whole text with [[SpellChecker.analyzeText]], which is how the specs hold it.
  */
private[spellcheck] object ParagraphSpellCheck:

  private def unchecked(dictionary: DictionaryContext)(using Balance): SpellCheckAnalysis =
    SpellCheckAnalysis(Rope.empty, Vector.empty, Nil, dictionary)

  /** `prior` is only used while it was found against `dictionary` itself, since a different dictionary may judge the
    * same words differently.
    */
  def analyze(content: Rope, prior: Option[SpellCheckAnalysis], dictionary: DictionaryContext): SpellCheckAnalysis =
    given Balance = Balance.default
    val base      = prior.filter(_.dictionary eq dictionary).getOrElse(unchecked(dictionary))
    RopeDiff.changedOffsetRange(base.content, content) match
      case None               => base.copy(content = content)
      case Some((start, end)) => reanalyze(content, base, start, end, dictionary)

  private def reanalyze(
    content: Rope,
    base: SpellCheckAnalysis,
    start: Int,
    end: Int,
    dictionary: DictionaryContext
  ): SpellCheckAnalysis =
    val lineShift  = content.newlineCount - base.content.newlineCount
    val firstLine  = content.offsetToLineColumn(start)._1
    val lastEdited = content.offsetToLineColumn(end)._1
    val window = reread(
      content.linesIteratorFrom(firstLine).map(_._2),
      Reading(firstLine, regionOpeningAt(base.regionChanges, firstLine), Vector.empty, Nil),
      lastEdited,
      base.regionChanges,
      lineShift,
      dictionary
    )
    val resumedOld = window.line - lineShift
    SpellCheckAnalysis(
      content = content,
      regionChanges = base.regionChanges.takeWhile(_.afterLine < firstLine) ++ window.changes ++
        base.regionChanges
          .filter(_.afterLine >= resumedOld)
          .map(change => change.copy(afterLine = change.afterLine + lineShift)),
      unknownWords = base.unknownWords.takeWhile(lineOf(_) < firstLine) ++ window.words.reverse ++
        movedBy(lineShift, base.unknownWords.dropWhile(lineOf(_) < resumedOld)),
      dictionary = dictionary
    )

  /** The lines read so far: `line` is the next one to read and `region` the region it opens in. */
  final private case class Reading(
      line: Int,
      region: ProseRegion,
      changes: Vector[ProseRegionChange],
      words: List[Diagnostic]
  )

  @tailrec
  private def reread(
    lines: Iterator[String],
    reading: Reading,
    lastEdited: Int,
    oldChanges: Vector[ProseRegionChange],
    lineShift: Int,
    dictionary: DictionaryContext
  ): Reading =
    val resumes =
      reading.line > lastEdited && reading.region == regionOpeningAt(oldChanges, reading.line - lineShift)
    if resumes || !lines.hasNext then reading
    else
      val text          = lines.next()
      val (next, words) = ProseTokenizer.step(reading.region, text, reading.line)
      val found         = SpellChecker.lineDiagnostics(ProseLine(reading.line, text, words), dictionary)
      val read = Reading(
        reading.line + 1,
        next,
        if next == reading.region then reading.changes
        else reading.changes :+ ProseRegionChange(reading.line, next),
        found.reverse ::: reading.words
      )
      reread(lines, read, lastEdited, oldChanges, lineShift, dictionary)

  /** The region `line` opens in: the one the last change before it left behind. */
  private def regionOpeningAt(changes: Vector[ProseRegionChange], line: Int): ProseRegion =
    changes.takeWhile(_.afterLine < line).lastOption.fold(ProseRegion.Prose)(_.region)

  private def lineOf(diagnostic: Diagnostic): Int = diagnostic.range.start.line

  private def movedBy(lineShift: Int, diagnostics: List[Diagnostic]): List[Diagnostic] =
    if lineShift == 0 then diagnostics
    else
      diagnostics.map(diagnostic =>
        diagnostic.copy(range =
          LspRange(
            LspPosition(diagnostic.range.start.line + lineShift, diagnostic.range.start.character),
            LspPosition(diagnostic.range.end.line + lineShift, diagnostic.range.end.character)
          )
        )
      )

end ParagraphSpellCheck
