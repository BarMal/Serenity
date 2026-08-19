package com.serenity.state.components

import cats.Monoid
import com.serenity.command.Command
import com.serenity.state.models.{AppState, Focus}
import com.serenity.state.reducers.ReducerResult

enum ComponentResult:
  case StateChange(update: AppState => AppState)
  case ReducerUpdate(result: ReducerResult)
  case FocusTransfer(newFocus: Focus)
  case NoChange
  case Composite(results: List[ComponentResult])
  case Dismiss
  case ExecuteCommand(command: Command)

object ComponentResult:

  /** `combine` flattens rather than nesting, so associativity holds structurally rather than merely behaviourally. */
  given Monoid[ComponentResult] with
    def empty: ComponentResult = NoChange

    def combine(x: ComponentResult, y: ComponentResult): ComponentResult =
      (x, y) match
        case (NoChange, result)                  => result
        case (result, NoChange)                  => result
        case (Composite(left), Composite(right)) => Composite(left ++ right)
        case (Composite(left), result)           => Composite(left :+ result)
        case (result, Composite(right))          => Composite(result :: right)
        case (left, right)                       => Composite(List(left, right))

  def updateState(f: AppState => AppState): ComponentResult = StateChange(f)
  def reducerResult(result: ReducerResult): ComponentResult = ReducerUpdate(result)
  def transferFocus(focus: Focus): ComponentResult          = FocusTransfer(focus)
  def dismiss: ComponentResult                              = Dismiss
  def noChange: ComponentResult                             = NoChange
  def composite(results: ComponentResult*): ComponentResult = Composite(results.toList)
  def executeCommand(command: Command): ComponentResult     = ExecuteCommand(command)
