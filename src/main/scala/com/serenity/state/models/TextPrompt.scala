package com.serenity.state.models

import com.serenity.lsp.config.LanguageId
import com.serenity.ui.widget.TextField

/** Where a `textDocument/rename` (#1467) was invoked, captured when its prompt opens so the rename still targets that
  * symbol if focus or the cursor moves while the prompt is up.
  */
final case class RenameSite(uri: String, languageId: LanguageId, line: Int, character: Int, anchor: CursorPosition)

/** What a [[TextPrompt]]'s submitted text is for, which decides what submitting does. */
enum TextPromptPurpose:
  case GotoLine
  case SessionName(mode: SessionNamePromptMode)
  case RenameSymbol(site: RenameSite)

/** A one-field prompt: a label, the text being typed, and what the text is for. */
final case class TextPrompt(label: String, field: TextField, purpose: TextPromptPurpose):

  def input: String = field.text

  /** Go-to-line only takes digits; every other prompt takes any printable character. */
  def accepts(char: Char): Boolean =
    purpose match
      case TextPromptPurpose.GotoLine => char.isDigit
      case _                          => !char.isControl

object TextPrompt:

  def gotoLine(input: String = ""): TextPrompt =
    TextPrompt("Go to line", TextField.of(input), TextPromptPurpose.GotoLine)

  def sessionName(mode: SessionNamePromptMode, input: String = ""): TextPrompt =
    val label = mode match
      case SessionNamePromptMode.SaveAs    => "Save session as"
      case SessionNamePromptMode.Rename(_) => "Rename session"
    TextPrompt(label, TextField.of(input), TextPromptPurpose.SessionName(mode))

  def renameSymbol(site: RenameSite, input: String): TextPrompt =
    TextPrompt("Rename symbol", TextField.of(input), TextPromptPurpose.RenameSymbol(site))
