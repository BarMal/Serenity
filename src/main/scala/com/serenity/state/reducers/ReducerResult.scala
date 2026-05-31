package com.serenity.state.reducers

import java.nio.file.Path

import com.serenity.command.Command
import com.serenity.lsp.LspEffect
import com.serenity.state.models.{AppState, BufferId, SurfaceId}

enum AppEffect:
  case CompleteQuit
  case ExecuteCommand(command: Command)
  case SwitchTheme(themeName: String)
  case ReloadTheme(themeName: String)
  case SaveBuffer(bufferId: BufferId)
  case SaveBufferAs(bufferId: BufferId, path: Path)
  case RequestOpenFile
  case RequestSaveAs
  case DirectLoadFile(path: Path)
  case OpenThemePicker
  case OpenFileSearch
  case RefreshFileWorkflow(surfaceId: SurfaceId)
  case SubmitFileWorkflow(surfaceId: SurfaceId)
  case SubmitReplaceWorkflow(surfaceId: SurfaceId)
  case SubmitCloseWorkflow(surfaceId: SurfaceId)
  case EnqueueLspEffect(effect: LspEffect)

case class ReducerResult(
    state: AppState,
    effects: List[AppEffect] = Nil
)

object ReducerResult:
  def noEffects(state: AppState): ReducerResult =
    ReducerResult(state, Nil)

  def withEffect(state: AppState, effect: AppEffect): ReducerResult =
    ReducerResult(state, List(effect))
