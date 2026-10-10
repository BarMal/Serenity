package com.serenity.state.models

import com.serenity.lsp.client.DocumentUri
import com.serenity.lsp.model.{Diagnostic, LspPosition, LspRange}
import com.serenity.rope.{Balance, ChangeSet, MapMode, Rope}

/** Carries published diagnostics across edits, so a squiggle or gutter mark stays on its text until the server or the
  * next spell check says something new, as a decoration in VS Code or an extmark in Neovim does (#1924, #1929).
  */
object DiagnosticsEditRemap:

  /** `next` with the diagnostics of every buffer whose text differs from its text in `previous` moved with the edit. A
    * commit that leaves the buffers map alone costs nothing, and an edit pays only for the buffer it touched.
    */
  def carried(previous: AppState, next: AppState): AppState =
    val held = next.runtime.languageService.diagnosticsState
    if held.diagnostics.isEmpty || (previous.persisted.buffers eq next.persisted.buffers) then next
    else
      val remapped = next.persisted.buffers.valuesIterator.foldLeft(held.diagnostics) { (diagnostics, buffer) =>
        previous.persisted.buffers
          .get(buffer.id)
          .filter(before => before.document.content ne buffer.document.content)
          .fold(diagnostics)(before => remappedFor(next, diagnostics, before, buffer))
      }
      if remapped eq held.diagnostics then next
      else
        next.copy(runtime =
          next.runtime.copy(languageService =
            next.runtime.languageService.copy(diagnosticsState = held.copy(diagnostics = remapped))
          )
        )

  private def remappedFor(
    next: AppState,
    diagnostics: Map[DocumentUri, List[Diagnostic]],
    before: Buffer,
    after: Buffer
  ): Map[DocumentUri, List[Diagnostic]] =
    val uri = next.runtime.bufferIndexMemos.uriFor(after)
    diagnostics.get(uri).fold(diagnostics) { found =>
      val change = after.document
        .changeFrom(before.document)
        .getOrElse(ChangeSet.fromDiff(before.document.content, after.document.content)(using Balance.default))
      val moved = through(found, change, before.document.content, after.document.content)
      if moved.isEmpty then diagnostics - uri else diagnostics.updated(uri, moved)
    }

  /** `diagnostics` of `before` as they stand in `after`, the text `change` made of it. A range keeps its text when text
    * is typed against either end; one whose text the edit removed is dropped, so deleting a flagged word clears its
    * squiggle at once. A diagnostic without a range of text (a dictionary that failed to load) stays where it was.
    */
  def through(diagnostics: List[Diagnostic], change: ChangeSet, before: Rope, after: Rope): List[Diagnostic] =
    if change.isIdentity then diagnostics
    else diagnostics.flatMap(moved(_, change, before, after))

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
