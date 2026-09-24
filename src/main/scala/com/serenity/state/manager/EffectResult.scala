package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.config.SpellCheckDictionaryFingerprint
import com.serenity.rope.Rope
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.{AppState, Buffer, BufferId, FindResult, FindSearchRequest, SpellCheckFingerprint}
import com.serenity.state.reducers.ModalEventReducer
import com.serenity.ui.presets.UiPreset

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

  // File I/O (#1671, #1672): posted by StateManagerFilePersistence's file-lane jobs; see FileResults.
  case FileSaved(save: FileSave, saved: Buffer)
  case FileSaveFailed(save: FileSave, error: Throwable)
  case FileReloaded(bufferId: BufferId, path: Path, contentAtRequest: Rope, disk: Buffer)
  case FileLoaded(path: Path, loaded: Buffer)
  case FileLoadFailed(path: Path, error: Throwable)

  /** A preset store operation finished: `previews` is the store's listing afterwards, when it was read. Always applied,
    * so a failed write is still reported.
    */
  case UiPresetFeedback(previews: Option[List[UiPreset.Preview]], context: UiPresetContext)
  case UiPresetReviewReady(preset: UiPreset)
  case UiPresetApplyResolved(request: Long, resolution: UiPresetApplyResolution)

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

      case FileSaved(save, saved) => FileResults.saved(state, save, saved)
      case FileReloaded(bufferId, path, contentAtRequest, disk) =>
        FileResults.reloaded(state, bufferId, path, contentAtRequest, disk)
      case FileLoaded(path, loaded)                    => FileResults.loaded(state, path, loaded)
      case FileSaveFailed(_, _) | FileLoadFailed(_, _) => state

      case UiPresetFeedback(previews, context) =>
        UiPresetTransitions.withFeedback(state, previews, context)
      case UiPresetReviewReady(preset) =>
        UiPresetTransitions.openDiffReview(state, preset)
      case UiPresetApplyResolved(request, resolution) =>
        UiPresetTransitions.resolveApply(state, request, resolution)
