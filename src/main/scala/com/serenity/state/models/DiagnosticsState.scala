package com.serenity.state.models

import com.serenity.lsp.client.DocumentUri
import com.serenity.lsp.model.Diagnostic

/** LSP diagnostics and the spell-check cache derived from them share a single production write site. Keyed by
  * [[DocumentUri]] (issue #1693), the same document identifier the LSP client layer uses, rather than a bare `String` a
  * caller could accidentally cross with some other kind of string key.
  */
final case class DiagnosticsState(
    diagnostics: Map[DocumentUri, List[Diagnostic]] = Map.empty,
    spellCheckCache: Map[DocumentUri, SpellCheckCacheEntry] = Map.empty,
    spellIgnores: SpellIgnores = SpellIgnores.empty,
    lastMoves: Map[DocumentUri, DiagnosticsMove] = Map.empty
)

/** What the last edit did to one document's diagnostics, for whoever compares the lists before and after it: the rows
  * it changed are known without walking either list.
  */
final case class DiagnosticsMove(before: List[Diagnostic], after: List[Diagnostic], lines: Set[Int])
