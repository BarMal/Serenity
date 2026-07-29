package com.serenity.state.components

import com.serenity.command.CommandRegistry
import com.serenity.keystroke.events.*
import com.serenity.state.models.AppState
import com.serenity.state.reducers.{CommandRunnerReducer, Reducer}

/** Component that handles command runner overlay functionality */
class CommandRunnerComponent(
    registry: CommandRegistry = CommandRegistry.default
) extends TypedFocusedComponent[CommandRunnerEvent]:
  private val reducer: Reducer[CommandRunnerEvent] = CommandRunnerReducer.reducer(registry)

  protected def decodeEvent(event: Event): Option[CommandRunnerEvent] =
    CommandRunnerEvent.fromEvent(event)

  protected def processTypedEvent(event: CommandRunnerEvent, currentState: AppState): ComponentResult =
    val result = reducer.reduce(event, currentState)
    ComponentResult.reducerResult(result)
