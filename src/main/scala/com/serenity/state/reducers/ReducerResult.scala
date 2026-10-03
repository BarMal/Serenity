package com.serenity.state.reducers

import java.nio.file.Path

import cats.syntax.all.*
import com.serenity.command.Command
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.{AppState, BufferId, CloseScope, SurfaceId}
import com.serenity.state.undo.HistoryEntry
import com.serenity.ui.layout.PanelPosition
import com.serenity.ui.theme.config.ThemeConfig

enum ThemeEffect:
  case SwitchTheme(themeName: String)
  case ReloadTheme(themeName: String)
  case SaveThemeConfig(config: ThemeConfig)
  case RefreshThemeNames
  case ExportCurrentTheme

enum SurfaceEffect:
  case OpenThemePicker
  case OpenThemeCreator
  case OpenFileSearch

enum FileEffect:
  case SaveBuffer(bufferId: BufferId)
  case SaveBufferAs(bufferId: BufferId, path: Path)
  case DirectLoadFile(path: Path)

enum ExplorerEffect:
  case OpenRoot(position: PanelPosition, path: Path, size: Int)

enum WorkflowEffect:
  case RequestOpenFile
  case RequestSaveAs
  case RefreshFileWorkflow(surfaceId: SurfaceId)
  case RefreshFind(request: com.serenity.state.models.FindSearchRequest)
  case SubmitFileWorkflow(surfaceId: SurfaceId)
  case SubmitReplaceWorkflow(surfaceId: SurfaceId)
  // Starts the close workflow (`beginCloseAction`) for `scope`: closes clean buffers, prompts for dirty ones.
  case BeginClose(scope: CloseScope)
  case CreateFileWorkflowDirectories(surfaceId: SurfaceId)
  // Named sessions (issue #1390): submitting the name prompt (save-as or rename) needs IO (SessionManager calls),
  // unlike GotoLine's pure jump-to-line submit.
  case SubmitSessionNamePrompt(surfaceId: SurfaceId)
  // Opens the directory currently browsed in an Open dialog as a project root (issue #1525): validated in IO exactly
  // like `SubmitFileWorkflow` (the reducer can't know whether `path` is really a directory), then handed off to the
  // same `ExplorerEffect.OpenRoot` pin-panel machinery a UI preset's docked directory tree already uses.
  case OpenFileWorkflowAsProjectRoot(surfaceId: SurfaceId)

enum LspQueueEffect:
  case Enqueue(effect: LspEffect)
  case DocumentChanged(uri: String, languageId: LanguageId, text: String)

/** A reducer's own declaration that the change it just performed is undoable, carrying the [[HistoryEntry]] that
  * restores it -- see #1016. `groupable` marks whether this should coalesce into an already-open run of edits
  * (consecutive character/tab insertion) rather than becoming its own undo step; only ever true for a
  * `HistoryEntry.BufferEdit`. The entry captures state as it was immediately prior to this change -- carried in the
  * effect itself (rather than left for `UndoRecording` to infer from an event-type allowlist and a before/after diff)
  * because interpretation runs after `AppState` has already been updated to the post-change state.
  */
enum UndoEffect:
  case RecordBoundary(entry: HistoryEntry, groupable: Boolean)

enum AppEffect:
  case CompleteQuit
  case ExecuteCommand(command: Command)

  /** Runs `command` as [[ExecuteCommand]] does, but leaves it out of the recently-used ranking: for commands the user
    * did not choose, such as a picker's preview of its highlighted choice or its restore on dismiss.
    */
  case ExecuteCommandUnrecorded(command: Command)
  case ScheduleCommandRunnerBindingExpiry(recordedAtMillis: Long)
  case Theme(effect: ThemeEffect)
  case Surface(effect: SurfaceEffect)
  case File(effect: FileEffect)
  case Explorer(effect: ExplorerEffect)
  case Workflow(effect: WorkflowEffect)
  case LspQueue(effect: LspQueueEffect)
  case Undo(effect: UndoEffect)

final case class ReducerResult(
    state: AppState,
    effects: List[AppEffect] = Nil
)

object ReducerResult:
  def noEffects(state: AppState): ReducerResult =
    ReducerResult(state, Nil)

  /** Run a [[Transition]] from `initial` and collect it into the boundary type. The inverse direction, turning a result
    * back into a transition, is [[toTransition]].
    */
  def fromTransition(initial: AppState, transition: Transition[Unit]): ReducerResult =
    Transition.run(initial)(transition)

  def withEffect(state: AppState, effect: AppEffect): ReducerResult =
    ReducerResult(state, List(effect))

extension (result: ReducerResult)
  /** Lift an already-computed result back into a transition, so migrated and unmigrated reducers can compose while #993
    * and #994 are in progress.
    */
  def toTransition: Transition[Unit] =
    Transition.set(result.state) *> Transition.emitAll(result.effects)
