package com.serenity.lsp.model

/** One `TextEdit` from a `textDocument/rename` response's `WorkspaceEdit` (LSP 3.17 §3.17.9): replace `range` with
  * `newText`.
  */
final case class LspTextEdit(range: LspRange, newText: String)

object LspTextEdit:

  private given Ordering[LspPosition] = Ordering.by(position => (position.line, position.character))

  /** Whether any two of `edits` overlap. Edits that only touch -- one ending where the next begins, or insertions at
    * one position -- do not.
    */
  def overlap(edits: List[LspTextEdit]): Boolean =
    val ranges = edits.map(_.range).sortBy(range => (range.start, range.end))
    val furthestEndBefore =
      ranges.scanLeft(Option.empty[LspPosition])((furthest, range) =>
        Some(furthest.fold(range.end)(Ordering[LspPosition].max(_, range.end)))
      )
    ranges.zip(furthestEndBefore).exists((range, furthest) => furthest.exists(Ordering[LspPosition].gt(_, range.start)))
