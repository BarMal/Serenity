package com.serenity.state.models

import com.serenity.lsp.model.Diagnostic

/** LSP diagnostics and the spell-check cache derived from them share a single production write site. */
final case class DiagnosticsState(
    diagnostics: Map[String, List[Diagnostic]] = Map.empty,
    spellCheckCache: Map[String, SpellCheckCacheEntry] = Map.empty
)
