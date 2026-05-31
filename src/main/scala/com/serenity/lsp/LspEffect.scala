package com.serenity.lsp

import com.serenity.lsp.config.LanguageId

enum LspEffect:
  case FileOpened(uri: String, languageId: LanguageId, text: String)
  case FileChanged(uri: String, languageId: LanguageId, text: String, version: Int)
  case FileClosed(uri: String, languageId: LanguageId)
