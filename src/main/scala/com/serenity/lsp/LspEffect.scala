package com.serenity.lsp

import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Rope
import com.serenity.state.models.{CursorPosition, NoticePromptId}

enum LspEffect:

  /** The document this effect is about; `None` for one that concerns the editor rather than any file. */
  def documentUri: Option[String] =
    this match
      case FileOpened(uri, _, _)                   => Some(uri)
      case FileChanged(uri, _, _, _)               => Some(uri)
      case FileClosed(uri, _)                      => Some(uri)
      case HoverRequested(uri, _, _, _, _)         => Some(uri)
      case CompletionRequested(uri, _, _, _, _)    => Some(uri)
      case DefinitionRequested(uri, _, _, _, _, _) => Some(uri)
      case ReferencesRequested(uri, _, _, _, _, _) => Some(uri)
      case RenameRequested(uri, _, _, _, _, _)     => Some(uri)
      case SemanticTokensRequested(uri, _)         => Some(uri)
      case VisibleRangeChanged(uri, _, _, _)       => Some(uri)
      case MessageRequestAnswered(_, _)            => None

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

  /** The lines of the document the editor shows, first to last. Only a server that can answer for a range alone needs
    * them, and only to colour what is on screen; they are never a reason to re-request for one that sends it all.
    */
  case VisibleRangeChanged(uri: String, languageId: LanguageId, firstLine: Int, lastLine: Int)

  /** The user's answer to a question a server asked in a notice: the index of the action chosen, or `None` when it was
    * dismissed. Nothing to do with any document, so it never closes a pending edit.
    */
  case MessageRequestAnswered(prompt: NoticePromptId, choice: Option[Int])
