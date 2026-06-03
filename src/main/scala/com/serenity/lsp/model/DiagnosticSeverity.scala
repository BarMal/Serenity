package com.serenity.lsp.model

enum DiagnosticSeverity(val code: Int):
  case Error       extends DiagnosticSeverity(1)
  case Warning     extends DiagnosticSeverity(2)
  case Information extends DiagnosticSeverity(3)
  case Hint        extends DiagnosticSeverity(4)

object DiagnosticSeverity:
  def fromCode(code: Int): Option[DiagnosticSeverity] =
    values.find(_.code == code)
