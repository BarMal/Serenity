package com.serenity.state.models

import com.serenity.lsp.model.{Diagnostic, LspPosition, LspRange}
import com.serenity.rope.{Balance, ChangeSet, MapMode, Rope}

/** Carries published diagnostics across edits, so a squiggle or gutter mark stays on its text until the server or the
  * next spell check says something new, as a decoration in VS Code or an extmark in Neovim does (#1924, #1929).
  */
object DiagnosticsEditRemap:

  // Carrying costs a pass over the document's diagnostics on every commit, which a document with thousands of them
  // (a whole book in the wrong language) cannot afford per keystroke. Those are left where they stand: the next spell
  // check finishes within a second of typing and publishes them already moved over the edits made meanwhile.
  val MaxCarried: Int = 2000

  /** `next` with the diagnostics of every buffer whose text differs from its text in `previous` moved with the edit. A
    * commit that leaves the buffers map alone costs nothing, and an edit pays only for the buffer it touched.
    */
  def carried(previous: AppState, next: AppState): AppState =
    val held = next.runtime.languageService.diagnosticsState
    if held.diagnostics.isEmpty || (previous.persisted.buffers eq next.persisted.buffers) then next
    else
      val remapped = next.persisted.buffers.valuesIterator.foldLeft(held) { (state, buffer) =>
        previous.persisted.buffers
          .get(buffer.id)
          .filter(before => before.document.content ne buffer.document.content)
          .fold(state)(before => remappedFor(next, state, before, buffer))
      }
      if remapped eq held then next
      else
        next.copy(runtime =
          next.runtime.copy(languageService = next.runtime.languageService.copy(diagnosticsState = remapped))
        )

  private def remappedFor(next: AppState, state: DiagnosticsState, before: Buffer, after: Buffer): DiagnosticsState =
    val uri = next.runtime.bufferIndexMemos.uriFor(after)
    state.diagnostics.get(uri).filter(_.lengthIs <= MaxCarried).fold(state) { found =>
      val change = after.document
        .changeFrom(before.document)
        .getOrElse(ChangeSet.fromDiff(before.document.content, after.document.content)(using Balance.default))
      val (moved, lines) = throughWithLines(found, change, before.document.content, after.document.content)
      if moved eq found then state
      else
        state.copy(
          diagnostics = if moved.isEmpty then state.diagnostics - uri else state.diagnostics.updated(uri, moved),
          lastMoves = state.lastMoves.updated(uri, DiagnosticsMove(found, moved, lines))
        )
    }

  def through(diagnostics: List[Diagnostic], change: ChangeSet, before: Rope, after: Rope): List[Diagnostic] =
    throughWithLines(diagnostics, change, before, after)._1

  /** `diagnostics` of `before` as they stand in `after`, the text `change` made of it, with the rows of those that
    * moved or went. A range keeps its text when text is typed against either end; one whose text the edit removed is
    * dropped, so deleting a flagged word clears its squiggle at once. A diagnostic without a range of text (a
    * dictionary that failed to load) stays where it was. What the edit cannot have moved -- everything ending before
    * it, and everything below it when it adds no line -- is neither converted nor copied past the last diagnostic that
    * did move, and the very list comes back when that is everything.
    */
  private def throughWithLines(
    diagnostics: List[Diagnostic],
    change: ChangeSet,
    before: Rope,
    after: Rope
  ): (List[Diagnostic], Set[Int]) =
    change.oldEnvelope match
      case Some((from, to)) if diagnostics.nonEmpty =>
        val (startLine, startColumn) = before.offsetToLineColumn(from)
        val endLine                  = before.offsetToLineColumn(to)._1
        val addsLines                = after.newlineCount != before.newlineCount
        def untouched(diagnostic: Diagnostic): Boolean =
          val range = diagnostic.range
          range.start == range.end || range.end.line < startLine ||
          (range.end.line == startLine && range.end.character <= startColumn) ||
          (!addsLines && range.start.line > endLine)
        val lastTouched = diagnostics.lastIndexWhere(!untouched(_))
        if lastTouched < 0 then (diagnostics, Set.empty)
        else
          val (head, unmoved) = diagnostics.splitAt(lastTouched + 1)
          val carried = head.foldRight(unmoved) { (diagnostic, rest) =>
            if untouched(diagnostic) then diagnostic :: rest
            else moved(diagnostic, change, before, after).fold(rest)(_ :: rest)
          }
          val lines = head.iterator
            .filterNot(untouched)
            .flatMap { diagnostic =>
              AnnotationLineIndex.spannedLines(diagnostic) ++
                moved(diagnostic, change, before, after).toList.flatMap(AnnotationLineIndex.spannedLines)
            }
            .toSet
          (carried, lines)
      case _ => (diagnostics, Set.empty)

  private def moved(diagnostic: Diagnostic, change: ChangeSet, before: Rope, after: Rope): Option[Diagnostic] =
    val range = diagnostic.range
    if range.start == range.end then Some(diagnostic)
    else
      val start = change.mapPos(offsetOf(before, range.start), MapMode.AnchorStart)
      val end   = change.mapPos(offsetOf(before, range.end), MapMode.AnchorEnd)
      Option.when(end > start)(diagnostic.copy(range = LspRange(positionOf(after, start), positionOf(after, end))))

  private def offsetOf(text: Rope, position: LspPosition): Int =
    text.lineColumnToOffset(position.line, position.character)

  private def positionOf(text: Rope, offset: Int): LspPosition =
    val (line, column) = text.offsetToLineColumn(offset)
    LspPosition(line, column)
