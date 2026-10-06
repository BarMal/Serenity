package com.serenity

import com.serenity.config.{AppConfig, SpellCheckConfig}
import com.serenity.rope.Balance
import com.serenity.spellcheck.{DictionaryContext, DictionarySnapshot, EnGbFixtureDictionary, SpellChecker}
import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** An editor holding `text`, analysed against the fixture en-GB dictionary, for specs of what the writer does with a
  * misspelling.
  */
object SpellingStateFixture:

  given Balance = Balance.default

  val paneId: PaneId     = PaneId(0)
  val bufferId: BufferId = BufferId(1)

  private val loaded = EnGbFixtureDictionary.load()

  val spellCheck: SpellCheckConfig  = loaded._1
  val dictionary: DictionaryContext = loaded._2

  def editor(text: String, cursor: CursorPosition = CursorPosition(0, 0)): AppState =
    val buffer = Buffer.fromString(bufferId, text).copy(editing = EditingState(List(cursor)))
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId),
        config = AppConfig.default.withSpellCheck(spellCheck)
      )
    )
    SpellChecker.refreshDiagnostics(state, DictionarySnapshot(dictionary, Nil))

  def spellCheckDisabled: AppState =
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(config =
        AppConfig.default.withSpellCheck(AppConfig.default.languageToolsConfig.spellCheck.copy(enabled = false))
      )
    )

  def textOf(state: AppState): String =
    state.persisted.buffers(bufferId).document.content.collect()

  def flaggedWords(state: AppState): List[String] =
    val buffer = state.persisted.buffers(bufferId)
    state.runtime.languageService.diagnosticsState.diagnostics
      .getOrElse(SpellChecker.diagnosticsUri(buffer), Nil)
      .filter(_.code.contains(SpellChecker.UnknownWordCode))
      .flatMap(d =>
        buffer.document.content
          .getLine(d.range.start.line)
          .map(_.substring(d.range.start.character, d.range.end.character))
      )
