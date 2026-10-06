package com.serenity.spellcheck

import com.serenity.config.{AppConfig, SpellCheckConfig}
import com.serenity.lsp.model.Diagnostic
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*

/** One buffer under spell check against a small fixed dictionary, for specs of what an edit costs and yields. */
object IncrementalSpellFixture:

  given Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  val config: SpellCheckConfig = SpellCheckConfig(enabled = true, languages = List("en"))

  val dictionary: DictionaryContext = DictionaryContext(
    words = Set("the", "cat", "sat", "on", "mat", "hello", "world", "code", "a", "and", "dog"),
    replacements = Map("teh" -> List("the")),
    failures = Nil
  )

  val snapshot: DictionarySnapshot = DictionarySnapshot(dictionary, Nil)

  val bufferId: BufferId = BufferId(1)

  def stateWith(content: Rope): AppState =
    val base = AppState.initial
    SpellChecker.refreshDiagnostics(
      base.copy(persisted =
        base.persisted.copy(
          buffers = Map(bufferId -> Buffer(bufferId, Document(content))),
          bufferOrder = List(bufferId),
          config = AppConfig.default.withSpellCheck(config)
        )
      ),
      snapshot
    )

  def edited(state: AppState, content: Rope): AppState =
    val buffer = state.persisted.buffers(bufferId)
    state.copy(persisted =
      state.persisted.copy(buffers = Map(bufferId -> buffer.copy(document = buffer.document.copy(content = content))))
    )

  def refreshed(state: AppState): AppState =
    SpellChecker.refreshDiagnostics(state, snapshot)

  def published(state: AppState): List[Diagnostic] =
    state.runtime.languageService.diagnosticsState.diagnostics
      .getOrElse(SpellChecker.diagnosticsUri(state.persisted.buffers(bufferId)), Nil)

  def fullAnalysis(content: Rope): List[Diagnostic] =
    SpellChecker.analyzeText(content.collect(), config, dictionary)
end IncrementalSpellFixture
