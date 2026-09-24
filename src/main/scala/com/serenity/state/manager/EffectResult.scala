package com.serenity.state.manager

import com.serenity.config.SpellCheckDictionaryFingerprint
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.{AppState, BufferId, FindResult, FindSearchRequest, SpellCheckFingerprint}
import com.serenity.state.reducers.ModalEventReducer

/** What a background lane job hands back to the dispatcher (#1697). Each result carries the version it was computed
  * against, and [[EffectResult.applyIfCurrent]] drops it once that version is no longer the live one: a lane's newer
  * job cancels an older one, but a result already posted before the cancel still reaches the dispatcher.
  */
private[manager] enum EffectResult:
  case FindSearchCompleted(request: FindSearchRequest, results: List[FindResult])
  case MarkdownPreviewSettled(bufferId: BufferId, generation: Long)

  case DocumentAnalysisCompleted(
      analyzed: AppState,
      expected: Map[String, SpellCheckFingerprint],
      dictionaryFingerprints: List[SpellCheckDictionaryFingerprint]
  )

private[manager] object EffectResult:

  def applyIfCurrent(state: AppState, result: EffectResult): AppState =
    result match
      case FindSearchCompleted(request, results) =>
        CursorViewport.ensureVisibleCursors(state, ModalEventReducer.applyFindSearchResults(state, request, results))
      case MarkdownPreviewSettled(bufferId, generation) =>
        state.persisted.buffers.get(bufferId).filter(_.markdownPreviewEditGeneration == generation).fold(state) {
          buffer =>
            state.copy(persisted =
              state.persisted.copy(buffers =
                state.persisted.buffers.updated(bufferId, buffer.copy(markdownPreviewCommittedGeneration = generation))
              )
            )
        }
      case DocumentAnalysisCompleted(analyzed, expected, dictionaryFingerprints) =>
        SpellChecker.applyIfCurrent(state, analyzed, expected, dictionaryFingerprints)
