package com.serenity.state.reducers

import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.*

/** Dismisses misspellings for the session (#1939): one occurrence, or every occurrence of a word. */
object SpellingIgnoreReducer:

  /** `None` when the text at `line`/`start`..`end` is no longer `word`: the menu was offered for an earlier version of
    * the buffer, and ignoring whatever stands there now would hide a different mistake.
    */
  def ignoreOnce(state: AppState, line: Int, start: Int, end: Int, word: String): Option[AppState] =
    for
      (_, buffer) <- SpellingTarget.of(state)
      text        <- buffer.document.content.getLine(line)
      if start >= 0 && start < end && end <= text.length && text.substring(start, end) == word
    yield withIgnores(
      state,
      _.withOccurrence(SpellChecker.diagnosticsUri(buffer), IgnoredOccurrence(line, start, word))
    )

  def ignoreEverywhere(state: AppState, word: String): AppState =
    withIgnores(state, _.withWord(word))

  private def withIgnores(state: AppState, update: SpellIgnores => SpellIgnores): AppState =
    val diagnosticsState = state.runtime.languageService.diagnosticsState
    SpellChecker.withIgnoresApplied(
      state.copy(runtime =
        state.runtime.copy(languageService =
          state.runtime.languageService
            .copy(diagnosticsState = diagnosticsState.copy(spellIgnores = update(diagnosticsState.spellIgnores)))
        )
      )
    )
