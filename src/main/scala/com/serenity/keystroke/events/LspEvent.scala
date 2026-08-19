package com.serenity.keystroke.events

import com.serenity.lsp.model.{Diagnostic, LspPosition}
import com.serenity.state.models.CursorPosition

enum LspEvent:
  case LspDiagnosticsReceived(uri: String, diagnostics: List[Diagnostic])
  case LspHoverReceived(text: String, anchor: CursorPosition)
  case LspCompletionReceived(items: List[String], anchor: CursorPosition)
  case LspDefinitionReceived(symbol: String, uri: String, position: LspPosition, anchor: CursorPosition)
