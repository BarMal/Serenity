package com.serenity.lsp.model

final case class Diagnostic(
    range: LspRange,
    severity: Option[DiagnosticSeverity],
    message: String,
    source: Option[String] = None,
    code: Option[String] = None
)
