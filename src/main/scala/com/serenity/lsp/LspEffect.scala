package com.serenity.lsp

import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.CursorPosition

enum LspEffect:
  case FileOpened(uri: String, languageId: LanguageId, text: String)
  case FileChanged(uri: String, languageId: LanguageId, text: String, version: Int)
  case FileClosed(uri: String, languageId: LanguageId)
  case HoverRequested(uri: String, languageId: LanguageId, line: Int, character: Int, anchor: CursorPosition)

  case DefinitionRequested(
      uri: String,
      languageId: LanguageId,
      line: Int,
      character: Int,
      anchor: CursorPosition,
      symbol: String
  )
