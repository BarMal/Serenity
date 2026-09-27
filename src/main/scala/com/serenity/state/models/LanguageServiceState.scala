package com.serenity.state.models

/** The LSP-derived state one grab-bag category of `Runtime` used to hold directly (issue #1693): diagnostics and
  * semantic tokens, both keyed by [[com.serenity.lsp.client.DocumentUri]] and both written from `SystemEventReducer`'s
  * handling of `LspEvent`.
  */
final case class LanguageServiceState(
    diagnosticsState: DiagnosticsState = DiagnosticsState(),
    semanticTokensState: SemanticTokensState = SemanticTokensState()
)
