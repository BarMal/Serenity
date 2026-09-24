package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.command.CommandRegistry
import com.serenity.config.SpellCheckDictionaryFingerprint
import com.serenity.keystroke.events.RunnerBindingRecordingExpired
import com.serenity.rope.Rope
import com.serenity.session.SessionMetadata
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.{
  AppState,
  Buffer,
  BufferId,
  FileWorkflowState,
  FindResult,
  FindSearchRequest,
  SessionListPurpose,
  SpellCheckFingerprint,
  SurfaceId
}
import com.serenity.state.reducers.{
  CommandRunnerReducer,
  ModalEventReducer,
  PinnedPanelContentReducer,
  ReducerResult,
  ThemeStateReducer
}
import com.serenity.ui.layout.{DirEntry, PanelPosition}
import com.serenity.ui.presets.UiPreset
import com.serenity.ui.theme.Theme

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

  // ---- Explorer and theme results (#1697 Wave 3: explorer/theme lanes) ----
  case ExplorerRootListed(position: PanelPosition, root: Path, listing: List[DirEntry])
  case DirectoryListed(position: PanelPosition, path: Path, listing: List[DirEntry])
  case ExplorerFileMoved(source: Path)
  case ThemeLoaded(requestedName: String, theme: Theme)
  case ThemeReloaded(requestedName: String, theme: Theme)
  case ThemeNamesListed(names: List[String])
  // ---- end explorer and theme results ----

  // File dialog (#1697 Wave 3): posted by StateManagerFileWorkflow's lane jobs, each for the dialog `surfaceId` showing
  // the input `requested`; dropped once the dialog is gone or shows other input. See FileWorkflowTransitions.
  case FileWorkflowListed(surfaceId: SurfaceId, requested: FileWorkflowState, listing: FileWorkflowListing)
  case FileWorkflowTargetResolved(surfaceId: SurfaceId, requested: FileWorkflowState, target: FileWorkflowTarget)

  case FileWorkflowProjectRootResolved(
      surfaceId: SurfaceId,
      requested: FileWorkflowState,
      target: Path,
      isDirectory: Boolean
  )

  // Named sessions (#1390, #1697 Wave 3): posted by StateManagerWorkflowCapability's Session-lane jobs. A loaded session
  // applies only while the picker it was chosen from is open. See SessionWorkflowTransitions.
  case SessionsListed(purpose: SessionListPurpose, sessions: List[SessionMetadata])
  case NamedSessionLoaded(pickerId: SurfaceId, restored: Option[AppState])

  // Timers (#1697 Wave 3): posted by `LaneKey.Timer` jobs once their delay has run out.
  /** The double-tap window of the binding recorded at `recordedAtMillis` has closed. */
  case CommandRunnerBindingExpired(recordedAtMillis: Long)

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

      // ---- Explorer and theme results (#1697 Wave 3: explorer/theme lanes) ----
      case ExplorerRootListed(position, root, listing) =>
        PinnedPanelContentReducer.applyRootListing(position, root, listing, state)
      case DirectoryListed(position, path, listing) =>
        PinnedPanelContentReducer.applyDirectoryListing(position, path, listing, state)
      case ExplorerFileMoved(source) =>
        PinnedPanelContentReducer.forgetMovedFile(source, state).state
      case ThemeLoaded(requestedName, theme) =>
        ThemeStateReducer.applyRequestedTheme(requestedName, theme, state)
      case ThemeReloaded(requestedName, theme) =>
        ThemeStateReducer.replaceRequestedTheme(requestedName, theme, state)
      case ThemeNamesListed(names) =>
        ThemeStateReducer.withAvailableThemeNames(names, state).state
      // ---- end explorer and theme results ----

      case FileWorkflowListed(surfaceId, requested, listing) =>
        FileWorkflowTransitions.withListing(state, surfaceId, requested, listing)
      case FileWorkflowTargetResolved(surfaceId, requested, target) =>
        FileWorkflowTransitions.withTargetResolved(state, surfaceId, requested, target)
      case FileWorkflowProjectRootResolved(surfaceId, requested, target, isDirectory) =>
        FileWorkflowTransitions.withProjectRootResolved(state, surfaceId, requested, target, isDirectory)

      case SessionsListed(purpose, sessions) =>
        SessionWorkflowTransitions.withSessionPicker(state, purpose, sessions)
      case NamedSessionLoaded(pickerId, restored) =>
        SessionWorkflowTransitions.withNamedSessionLoaded(state, pickerId, restored)

      case expired: CommandRunnerBindingExpired => reduce(state, expired).state

  /** [[applyIfCurrent]] together with the effects its transition emits; only the cases matched here emit any. */
  def reduce(state: AppState, result: EffectResult): ReducerResult =
    result match
      case CommandRunnerBindingExpired(recordedAtMillis) =>
        CommandRunnerReducer.reduce(
          RunnerBindingRecordingExpired(recordedAtMillis),
          state,
          CommandRegistry.withToggleUI
        )
      case other => ReducerResult.noEffects(applyIfCurrent(state, other))
