package com.serenity.state.reducers

import java.nio.file.Path

import cats.syntax.all.*
import com.serenity.animation.{AnimatedCell, AnimationOwner, CharacterKey, TextEdit}
import com.serenity.command.Command
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Rope
import com.serenity.state.models.{AppState, BufferId, SurfaceId}
import com.serenity.ui.layout.PanelPosition
import com.serenity.ui.theme.config.ThemeConfig

enum ThemeEffect:
  case SwitchTheme(themeName: String)
  case ReloadTheme(themeName: String)
  case SaveThemeConfig(config: ThemeConfig)

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
  case LoadDirectory(position: PanelPosition, path: Path)

enum WorkflowEffect:
  case RequestOpenFile
  case RequestSaveAs
  case RefreshFileWorkflow(surfaceId: SurfaceId)
  case RefreshFind(request: com.serenity.state.models.FindSearchRequest)
  case SubmitFileWorkflow(surfaceId: SurfaceId)
  case SubmitReplaceWorkflow(surfaceId: SurfaceId)
  case SubmitCloseWorkflow(surfaceId: SurfaceId)

enum LspQueueEffect:
  case Enqueue(effect: LspEffect)
  case DocumentChanged(uri: String, languageId: LanguageId, text: String)

/** `Buffer` carries no animation state (`#1001`) -- these are how a reducer that computed an animation change hands it
  * to the presentation layer that actually owns `AnimationState`, instead of writing it into the `AppState` it returns.
  * `RemapThroughEdits` carries the edits themselves, not a precomputed `AnimationState`, because the remap
  * (`AnimationState.remapThroughEdits`) needs the presentation layer's own current animations as input -- state the
  * pure reducer has no access to.
  */
enum AnimationEffect:
  case RemapThroughEdits(bufferId: BufferId, contentBefore: Rope, contentAfter: Rope, edits: List[TextEdit])
  case Merge(bufferId: BufferId, delta: Map[CharacterKey, AnimatedCell])
  case ClearAll(bufferId: BufferId)
  case ClearOwner(bufferId: BufferId, owner: AnimationOwner)

enum AppEffect:
  case CompleteQuit
  case ExecuteCommand(command: Command)
  case ScheduleCommandRunnerBindingExpiry(recordedAtMillis: Long)
  case Theme(effect: ThemeEffect)
  case Surface(effect: SurfaceEffect)
  case File(effect: FileEffect)
  case Explorer(effect: ExplorerEffect)
  case Workflow(effect: WorkflowEffect)
  case LspQueue(effect: LspQueueEffect)
  case Animation(effect: AnimationEffect)

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
