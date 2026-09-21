package com.serenity.lsp.model

/** One `TextEdit` from a `textDocument/rename` response's `WorkspaceEdit` (LSP 3.17 §3.17.9): replace `range` with
  * `newText`.
  */
final case class LspTextEdit(range: LspRange, newText: String)
