package com.serenity.state.models

import com.serenity.lsp.model.SemanticToken

/** The most recent `textDocument/semanticTokens/full` result per document, keyed the same way as
  * [[DiagnosticsState.diagnostics]] (a document's real file URI, since only buffers with an active LSP binding ever
  * receive one). Absence of a key -- not an empty list -- is what tells the renderer a document has no LSP-derived
  * highlighting to show at all (issue #859/#1177's "visually report the LSP is unavailable" requirement);
  * [[AppState.semanticTokensIndexByBuffer]] is where that distinction is read back out.
  */
final case class SemanticTokensState(byUri: Map[String, List[SemanticToken]] = Map.empty)
