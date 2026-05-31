package com.serenity.lsp.model

case class Diagnostic(
  range:    LspRange,
  severity: Option[DiagnosticSeverity],
  message:  String,
  source:   Option[String] = None,
  code:     Option[String] = None
)
