package com.serenity.lsp.model

/** Which `textDocument/semanticTokens` requests a server declared in its `semanticTokensProvider` (LSP 3.17 §3.17.7).
  * `delta` is only meaningful together with `full`: it is the `full.delta` flag.
  */
final case class SemanticTokensFeatures(full: Boolean, delta: Boolean, range: Boolean)

object SemanticTokensFeatures:

  /** What a connection that never read a real `initialize` result is assumed to offer: the one request this client sent
    * before the others were supported.
    */
  val FullOnly: SemanticTokensFeatures = SemanticTokensFeatures(full = true, delta = false, range = false)
