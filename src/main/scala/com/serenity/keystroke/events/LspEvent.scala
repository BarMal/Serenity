package com.serenity.keystroke.events

import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.{Diagnostic, LspPosition, LspProgress, LspTextEdit, SemanticTokenData, TextChangeDiff}
import com.serenity.state.models.CursorPosition

enum LspEvent:
  case LspDiagnosticsReceived(uri: String, diagnostics: List[Diagnostic])
  case LspHoverReceived(text: String, anchor: CursorPosition)
  case LspCompletionReceived(items: List[String], anchor: CursorPosition)
  case LspDefinitionReceived(symbol: String, uri: String, position: LspPosition, anchor: CursorPosition)
  case LspReferencesReceived(symbol: String, locations: List[(String, LspPosition)], anchor: CursorPosition)
  case LspRenameReceived(edits: Map[String, List[LspTextEdit]], anchor: CursorPosition)
  case LspSemanticTokensReceived(uri: String, tokens: SemanticTokenData)

  /** A range result: `tokens` replace those held for lines `firstLine` to `lastLine`, and no others. */
  case LspSemanticTokensRangeReceived(uri: String, firstLine: Int, lastLine: Int, tokens: SemanticTokenData)

  /** The document changed by `change` since its tokens were computed; they move with the text until fresh ones arrive.
    */
  case LspSemanticTokensEdited(uri: String, change: TextChangeDiff.Change)
  case LspSemanticTokensUnavailable(uri: String)

  /** Work a server reports with `$/progress`, shown in the status line until it ends. */
  case LspProgressReceived(languageId: LanguageId, token: String, progress: LspProgress)

  /** The language's server is gone, so whatever it still reported as running is not. */
  case LspServerStopped(languageId: LanguageId)

  /** A server's `workspace/applyEdit`, keyed by document uri; applied to every open buffer it names. */
  case LspWorkspaceEditRequested(edits: Map[String, List[LspTextEdit]])
