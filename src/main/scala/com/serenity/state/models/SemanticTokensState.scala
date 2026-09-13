package com.serenity.state.models

import com.serenity.lsp.model.SemanticToken

/** The most recent `textDocument/semanticTokens/full` result per document, keyed the same way as
  * [[DiagnosticsState.diagnostics]] (a document's real file URI, since only buffers with an active LSP binding ever
  * receive one), plus which documents have been confirmed to have none coming (no server for this language at all, or a
  * connected server that never declared the capability). A URI absent from both `byUri` and `unavailableUris` is a
  * document whose request is still in flight (or hasn't been sent yet) -- distinct from `unavailableUris`, which is a
  * confirmed negative answer. [[AppState.semanticTokensIndexByBuffer]] is where that three-way distinction is read back
  * out as [[SemanticTokensAvailability]].
  */
final case class SemanticTokensState(
    byUri: Map[String, List[SemanticToken]] = Map.empty,
    unavailableUris: Set[String] = Set.empty
)

/** A document's semantic-tokens status, as read back out of [[SemanticTokensState]] via
  * [[AppState.semanticTokensIndexByBuffer]]. Modeling this as three cases -- rather than the earlier
  * `Option[List[SemanticToken]]`, where absence meant both "no answer yet" and "confirmed none" -- is what lets the
  * renderer tell "a request is still in flight, render this line as if it were plain (no alarming style yet)"
  * (`Pending`) apart from "confirmed there is nothing to show here" (`Unavailable`): collapsing them onto one signal
  * made every file open against a fully working, connected server flash the muted "unavailable" style for the length of
  * the semantic-tokens round-trip (issue #859/#1177 rendering-slice review finding).
  */
enum SemanticTokensAvailability:
  /** No confirmed answer yet for this document -- a request may be in flight, or none has been sent. Renders like a
    * connected document with no tokens on this line yet, not the muted "unavailable" style.
    */
  case Pending

  /** Confirmed: no server is connected for this document's language, or the connected server never declared the
    * `semanticTokensProvider` capability. Renders the muted "unavailable" style.
    */
  case Unavailable

  /** This document's most recently received tokens, grouped by line. `byLine` can itself be empty -- "connected, but no
    * visible line has a token" -- without that meaning `Unavailable`.
    */
  case Available(byLine: Map[Int, List[SemanticToken]])
