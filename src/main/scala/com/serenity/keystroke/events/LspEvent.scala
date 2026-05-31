package com.serenity.keystroke.events

import com.serenity.lsp.model.Diagnostic

enum LspEvent extends SystemEvent:
  case LspDiagnosticsReceived(uri: String, diagnostics: List[Diagnostic])
