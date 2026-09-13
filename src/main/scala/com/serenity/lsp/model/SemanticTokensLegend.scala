package com.serenity.lsp.model

/** The token type/modifier vocabulary a server declares in its `semanticTokensProvider.legend` (LSP 3.17 §3.17.7.4).
  * `data`'s flat integer encoding indexes into these two lists, so a response can only be decoded against the legend
  * the *server* returned, not the client's own requested legend -- the two need not agree on ordering.
  */
final case class SemanticTokensLegend(tokenTypes: List[String], tokenModifiers: List[String])
