package com.serenity.state.components

import cats.effect.unsafe.implicits.global
import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.keystroke.events.*
import com.serenity.state.models.*

/** Component that handles command runner overlay functionality */
class CommandRunnerComponent(registry: CommandRegistry = CommandRegistry.default) extends FocusedComponent:

  def processEvent(event: Event, currentState: AppState): ComponentResult =
    event match
      case ToggleCommandRunner =>
        if currentState.commandRunner.isActive then deactivateCommandRunner(currentState)
        else activateCommandRunner(currentState)

      case _ if currentState.commandRunner.isActive =>
        processCommandRunnerEvent(event, currentState)

      case _ =>
        ComponentResult.noChange

  private def activateCommandRunner(state: AppState): ComponentResult =
    val activatedRunner = CommandRunner.empty
      .activate(registry)
      .withPreviousFocus(state.focus)

    ComponentResult.updateState { _ =>
      state.copy(
        commandRunner = activatedRunner,
        focus = Focus.CommandRunner
      )
    }

  private def deactivateCommandRunner(state: AppState): ComponentResult =
    val previousFocus = state.commandRunner.previousFocus.getOrElse(Focus.EditorPane(PaneId(0)))

    ComponentResult.updateState { _ =>
      state.copy(
        commandRunner = CommandRunner.empty,
        focus = previousFocus
      )
    }

  private def processCommandRunnerEvent(event: Event, state: AppState): ComponentResult =
    event match
      case Escape =>
        deactivateCommandRunner(state)

      case Enter =>
        executeSelectedCommand(state)

      case InsertChar(char) =>
        updateSearchTerm(state, state.commandRunner.searchTerm + char)

      case DeleteBackward =>
        if state.commandRunner.searchTerm.nonEmpty then
          val newTerm = state.commandRunner.searchTerm.dropRight(1)
          updateSearchTerm(state, newTerm)
        else ComponentResult.noChange

      case MoveUp =>
        moveSelection(state, -1)

      case MoveDown =>
        moveSelection(state, 1)

      case _ =>
        ComponentResult.noChange

  private def updateSearchTerm(state: AppState, newTerm: String): ComponentResult =
    given CommandRegistry = registry
    val updatedRunner     = state.commandRunner.updateSearchTerm(newTerm)

    ComponentResult.updateState(_ => state.copy(commandRunner = updatedRunner))

  private def moveSelection(state: AppState, delta: Int): ComponentResult =
    val updatedRunner = state.commandRunner.moveSelection(delta)

    ComponentResult.updateState(_ => state.copy(commandRunner = updatedRunner))

  private def executeSelectedCommand(state: AppState): ComponentResult =
    state.commandRunner.selectedCommand match
      case Some(command) =>
        val previousFocus = state.commandRunner.previousFocus.getOrElse(Focus.EditorPane(PaneId(0)))
        ComponentResult.updateState { s =>
          command.action(s).unsafeRunSync()
          s.copy(commandRunner = CommandRunner.empty, focus = previousFocus)
        }

      case None =>
        deactivateCommandRunner(state)
