package com.serenity.state.components

import com.serenity.command.CommandRegistry
import com.serenity.keystroke.events.*
import com.serenity.state.models.AppState
import com.serenity.state.reducers.{AppEffect, CommandRunnerReducer}

/** Component that handles command runner overlay functionality */
class CommandRunnerComponent(
    registry: CommandRegistry = CommandRegistry.default
) extends FocusedComponent:

  def processEvent(event: Event, currentState: AppState): ComponentResult =
    val result = CommandRunnerReducer.reduce(event, currentState, registry)
    result.effects match
      case List(AppEffect.ExecuteCommand(command)) =>
        ComponentResult.composite(
          ComponentResult.updateState(_ => result.state),
          ComponentResult.executeCommand(command)
        )
      case Nil =>
        if result.state == currentState then ComponentResult.noChange
        else ComponentResult.updateState(_ => result.state)
      case _ =>
        ComponentResult.updateState(_ => result.state)
