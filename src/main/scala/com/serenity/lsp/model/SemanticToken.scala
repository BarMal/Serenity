package com.serenity.lsp.model

/** One decoded entry from a `textDocument/semanticTokens/full` response, after resolving the wire format's
  * delta-encoded, legend-indexed integers (LSP 3.17 §3.17.7.4) to absolute positions and named strings.
  */
final case class SemanticToken(
    line: Int,
    startCharacter: Int,
    length: Int,
    tokenType: String,
    tokenModifiers: Set[String]
)
