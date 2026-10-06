package com.serenity.state.manager

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.command.SpellingIntent
import com.serenity.spellcheck.{Misspelling, SpellChecker}
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, SpellingIgnoreReducer, SpellingReplacementReducer}

/** The writer's responses to a misspelling (#1939): see suggestions, replace the word, add it to the dictionary, or
  * leave it alone once or everywhere. Suggestions are searched for here, on request, never by the background analysis.
  */
final private[manager] class StateManagerSpellingEffects(
    editor: EffectEditorPort,
    currentState: IO[AppState],
    interpretEffect: AppEffect => IO[Unit],
    addWordToDictionary: String => IO[Unit]
):
  import editor.{commitState, spellingSuggestions}

  private[manager] def interpret(intent: SpellingIntent): IO[Unit] =
    intent match
      case SpellingIntent.ShowSuggestions => showSuggestionsAtCursor
      case SpellingIntent.Replace(line, start, end, misspelled, replacement) =>
        commitReduction(SpellingReplacementReducer.replace(_, line, start, end, misspelled, replacement))
      case SpellingIntent.AddToDictionary(word) => addWordToDictionary(word)
      case SpellingIntent.IgnoreOnce(line, start, end, word) =>
        commitChange(SpellingIgnoreReducer.ignoreOnce(_, line, start, end, word))
      case SpellingIntent.IgnoreEverywhere(word) =>
        commitChange(state => Some(SpellingIgnoreReducer.ignoreEverywhere(state, word)))
      case SpellingIntent.IgnoreOnceAtCursor =>
        withMisspellingAtCursor(found =>
          commitChange(SpellingIgnoreReducer.ignoreOnce(_, found.line, found.start, found.end, found.word))
        )
      case SpellingIntent.IgnoreEverywhereAtCursor =>
        withMisspellingAtCursor(found =>
          commitChange(state => Some(SpellingIgnoreReducer.ignoreEverywhere(state, found.word)))
        )

  private def commitReduction(reduce: AppState => Option[com.serenity.state.reducers.ReducerResult]): IO[Unit] =
    currentState.flatMap { current =>
      reduce(current).traverse_(result =>
        commitState(result.state, current) >> result.effects.traverse_(interpretEffect)
      )
    }

  private def commitChange(change: AppState => Option[AppState]): IO[Unit] =
    currentState.flatMap(current => change(current).traverse_(commitState(_, current)))

  private def withMisspellingAtCursor(act: Misspelling => IO[Unit]): IO[Unit] =
    currentState.flatMap(state => misspellingAtCursor(state).traverse_((_, found) => act(found)))

  private def misspellingAtCursor(state: AppState): Option[(PaneId, Misspelling)] =
    for
      paneId <- state.persisted.layout.activeEditorPaneId
      buffer <- state.activeBuffer
      cursor <- state.activeCursorPosition
      found  <- SpellChecker.misspellingAt(state, buffer, cursor)
    yield (paneId, found)

  private def showSuggestionsAtCursor: IO[Unit] =
    currentState.flatMap { state =>
      misspellingAtCursor(state).traverse_ { (paneId, found) =>
        spellingSuggestions(found.word).flatMap { suggestions =>
          currentState.flatMap { current =>
            val menu = ContextMenu("spelling", Focus.EditorPane(paneId), SpellingMenu.items(found, suggestions))
            commitState(
              EditorContextMenuHitTesting.withMenu(current, menu, CursorPosition(found.line, found.start)),
              current
            )
          }
        }
      }
    }
