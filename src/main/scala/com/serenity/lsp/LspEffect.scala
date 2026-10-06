package com.serenity.lsp

import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Rope
import com.serenity.state.models.CursorPosition

enum LspEffect:
  def uri: String

  // The text travels as the rope: the characters are only collected when a server is sent them.
  case FileOpened(uri: String, languageId: LanguageId, text: Rope)
  case FileChanged(uri: String, languageId: LanguageId, text: Rope, version: Int)
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

  case ReferencesRequested(
      uri: String,
      languageId: LanguageId,
      line: Int,
      character: Int,
      anchor: CursorPosition,
      symbol: String
  )

  case RenameRequested(
      uri: String,
      languageId: LanguageId,
      line: Int,
      character: Int,
      anchor: CursorPosition,
      newName: String
  )

  /** Requests semantic tokens for the whole document, not a cursor position -- there is no line/character to give,
    * unlike [[HoverRequested]]/[[CompletionRequested]]/[[DefinitionRequested]].
    */
  case SemanticTokensRequested(uri: String, languageId: LanguageId)
