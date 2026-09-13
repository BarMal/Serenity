package com.serenity.lsp

import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.CursorPosition

enum LspEffect:
  case FileOpened(uri: String, languageId: LanguageId, text: String)
  case FileChanged(uri: String, languageId: LanguageId, text: String, version: Int)
  case FileClosed(uri: String, languageId: LanguageId)
  case HoverRequested(uri: String, languageId: LanguageId, line: Int, character: Int, anchor: CursorPosition)
  case CompletionRequested(uri: String, languageId: LanguageId, line: Int, character: Int, anchor: CursorPosition)

  case DefinitionRequested(
      uri: String,
      languageId: LanguageId,
      line: Int,
      character: Int,
      anchor: CursorPosition,
      symbol: String
  )

  /** Requests semantic tokens for the whole document, not a cursor position -- there is no line/character to give,
    * unlike [[HoverRequested]]/[[CompletionRequested]]/[[DefinitionRequested]].
    */
  case SemanticTokensRequested(uri: String, languageId: LanguageId)
