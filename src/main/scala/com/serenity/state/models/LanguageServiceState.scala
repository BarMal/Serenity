package com.serenity.state.models

import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.LspProgressTask

/** The LSP-derived state one grab-bag category of `Runtime` used to hold directly (issue #1693): diagnostics and
  * semantic tokens, both keyed by [[com.serenity.lsp.client.DocumentUri]] and both written from `SystemEventReducer`'s
  * handling of `LspEvent`, plus the work each language's server reports as running (#1847).
  */
final case class LanguageServiceState(
    diagnosticsState: DiagnosticsState = DiagnosticsState(),
    semanticTokensState: SemanticTokensState = SemanticTokensState(),
    progress: Map[LanguageId, List[LspProgressTask]] = Map.empty
)
