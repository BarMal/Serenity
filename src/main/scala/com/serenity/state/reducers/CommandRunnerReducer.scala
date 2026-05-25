package com.serenity.state.reducers

import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.keystroke.events.*
import com.serenity.state.models.{AppState, Focus, PaneId}

object CommandRunnerReducer:

  def reduce(event: Event, state: AppState, registry: CommandRegistry): ReducerResult =
    event match
      case ToggleCommandRunner =>
        if state.commandRunner.isActive then
          ReducerResult.noEffects(deactivate(state))
        else
          ReducerResult.noEffects(activate(state, registry))

      case _ if state.commandRunner.isActive =>
        reduceActive(event, state, registry)

      case _ =>
        ReducerResult.noEffects(state)

  private def reduceActive(event: Event, state: AppState, registry: CommandRegistry): ReducerResult =
    event match
      case Escape =>
        ReducerResult.noEffects(deactivate(state))

      case Enter =>
        state.commandRunner.selectedCommand match
          case Some(command) =>
            val previousFocus = state.commandRunner.previousFocus.getOrElse(Focus.EditorPane(PaneId(0)))
            ReducerResult(
              state = state.copy(commandRunner = CommandRunner.empty, focus = previousFocus),
              effects = List(AppEffect.ExecuteCommand(command))
            )
          case None =>
            ReducerResult.noEffects(deactivate(state))

      case InsertChar(char) =>
        given CommandRegistry = registry
        val updatedRunner     = state.commandRunner.updateSearchTerm(state.commandRunner.searchTerm + char)
        ReducerResult.noEffects(state.copy(commandRunner = updatedRunner))

      case DeleteBackward =>
        if state.commandRunner.searchTerm.nonEmpty then
          given CommandRegistry = registry
          val updatedRunner = state.commandRunner.updateSearchTerm(state.commandRunner.searchTerm.dropRight(1))
          ReducerResult.noEffects(state.copy(commandRunner = updatedRunner))
        else ReducerResult.noEffects(state)

      case MoveUp =>
        ReducerResult.noEffects(state.copy(commandRunner = state.commandRunner.moveSelection(-1)))

      case MoveDown =>
        ReducerResult.noEffects(state.copy(commandRunner = state.commandRunner.moveSelection(1)))

      case _ =>
        ReducerResult.noEffects(state)

  private def activate(state: AppState, registry: CommandRegistry): AppState =
    val activatedRunner = CommandRunner.empty
      .activate(registry)
      .withPreviousFocus(state.focus)

    state.copy(
      commandRunner = activatedRunner,
      focus = Focus.CommandRunner
    )

  private def deactivate(state: AppState): AppState =
    val previousFocus = state.commandRunner.previousFocus.getOrElse(Focus.EditorPane(PaneId(0)))
    state.copy(
      commandRunner = CommandRunner.empty,
      focus = previousFocus
    )

