package com.serenity.state.models

import com.serenity.lsp.client.DocumentUri
import com.serenity.lsp.model.Diagnostic

/** LSP diagnostics and the spell-check cache derived from them share a single production write site. Keyed by
  * [[DocumentUri]] (issue #1693), the same document identifier the LSP client layer uses, rather than a bare `String` a
  * caller could accidentally cross with some other kind of string key.
  */
final case class DiagnosticsState(
    diagnostics: Map[DocumentUri, List[Diagnostic]] = Map.empty,
    spellCheckCache: Map[DocumentUri, SpellCheckCacheEntry] = Map.empty
)
