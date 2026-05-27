package com.serenity.keystroke.events

trait CommandRunnerEvent extends Event

case class RunnerInsertChar(char: Char) extends CommandRunnerEvent
case object RunnerDeleteBackward        extends CommandRunnerEvent
case class RunnerNavigate(direction: Direction) extends CommandRunnerEvent
case object RunnerNextCategory          extends CommandRunnerEvent
case object RunnerPreviousCategory      extends CommandRunnerEvent
case object RunnerSubmit                extends CommandRunnerEvent
case object RunnerDismiss               extends CommandRunnerEvent

object CommandRunnerEvent:

  def fromEvent(event: Event): Option[CommandRunnerEvent] =
    event match
      case InsertChar(char) => Some(RunnerInsertChar(char))
      case DeleteBackward   => Some(RunnerDeleteBackward)
      case MoveUp           => Some(RunnerNavigate(Direction.Up))
      case MoveDown         => Some(RunnerNavigate(Direction.Down))
      case MoveLeft         => Some(RunnerNavigate(Direction.Left))
      case MoveRight        => Some(RunnerNavigate(Direction.Right))
      case TabKey           => Some(RunnerNextCategory)
      case ReverseTabKey    => Some(RunnerPreviousCategory)
      case Enter | NewLine  => Some(RunnerSubmit)
      case Escape           => Some(RunnerDismiss)
      case runnerEvent: CommandRunnerEvent => Some(runnerEvent)
      case _               => None
