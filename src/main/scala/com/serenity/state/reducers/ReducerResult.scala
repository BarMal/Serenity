package com.serenity.state.reducers

import java.nio.file.Path

import com.serenity.command.Command
import com.serenity.state.models.{AppState, BufferId}

enum AppEffect:
  case CompleteQuit
  case ExecuteCommand(command: Command)
  case SaveBuffer(bufferId: BufferId)
  case SaveBufferAs(bufferId: BufferId, path: Path)
  case RequestOpenFile

case class ReducerResult(
    state: AppState,
    effects: List[AppEffect] = Nil
)

object ReducerResult:
  def noEffects(state: AppState): ReducerResult =
    ReducerResult(state, Nil)

  def withEffect(state: AppState, effect: AppEffect): ReducerResult =
    ReducerResult(state, List(effect))
