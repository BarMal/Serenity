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

  /** `NoChange` is an identity and `Composite` is a combine, so this algebra was already a monoid written longhand.
    * Naming the instance lets results be folded with the standard combinators instead of each call site special-casing
    * "nothing happened" and "several things happened".
    *
    * `combine` flattens rather than nesting: combining two composites yields one composite of both, so associativity
    * holds structurally -- `(a |+| b) |+| c` and `a |+| (b |+| c)` produce the same three-element composite rather than
    * differently-shaped trees that merely behave alike.
    */
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
