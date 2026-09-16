package com.serenity.state.models

import com.serenity.config.AppMode
import com.serenity.lsp.config.LanguageId

/** What kind of text the active buffer holds, as far as the interface needs to distinguish. */
enum BufferKind:
  case PlainText
  case Markdown
  case RichText
  case Code(language: LanguageId)

  def isProse: Boolean =
    this match
      case Code(_) => false
      case _       => true

/** Which shell the app is running in -- a fixed-cell terminal or the Swing canvas. */
enum Shell:
  case Gui
  case Tui

/** Everything mode-aware in the interface reads from here rather than consulting `config.appMode`, the active buffer's
  * language and `runtime.isTuiMode` separately: one derivation per frame, so the toolbar, the status line, the settings
  * filter and the palette cannot each answer "what is the user doing right now" differently.
  *
  * `mode` is the workspace's declared intent (code or prose), `buffer` is what the focused document actually is. The
  * two can legitimately disagree -- a Scala file open in a prose workspace -- and each consumer decides which it cares
  * about: code tooling wants the workspace's word, typography wants the buffer's.
  */
final case class EditingContext(
    mode: AppMode,
    buffer: Option[BufferKind],
    shell: Shell,
    focus: Focus
):

  def isCodeWorkspace: Boolean  = mode == AppMode.Code
  def isProseWorkspace: Boolean = mode == AppMode.Prose
  def isTui: Boolean            = shell == Shell.Tui

  /** Build/run/test/debug and language servers are offered only in a code workspace (#1294). */
  def hasCodeTooling: Boolean = isCodeWorkspace

  def bufferIsCode: Boolean =
    buffer.exists {
      case BufferKind.Code(_) => true
      case _                  => false
    }

object EditingContext:

  def of(state: AppState): EditingContext =
    EditingContext(
      mode = state.persisted.config.appMode,
      buffer = state.activeBuffer.map(bufferKind),
      shell = if state.runtime.isTuiMode then Shell.Tui else Shell.Gui,
      focus = state.persisted.focus
    )

  def bufferKind(buffer: Buffer): BufferKind =
    buffer.document.language match
      case Some(LanguageId.Markdown)                          => BufferKind.Markdown
      case Some(language)                                     => BufferKind.Code(language)
      case None if buffer.richText.richTextDocument.isDefined => BufferKind.RichText
      case None                                               => BufferKind.PlainText
