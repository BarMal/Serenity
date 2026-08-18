package com.serenity.keystroke.events

import com.serenity.command.CommandCategory
import com.serenity.keystroke.KeyStrokeInfo

sealed trait CommandRunnerEvent extends Event

final case class RunnerInsertChar(char: Char)                                extends CommandRunnerEvent
case object RunnerDeleteBackward                                             extends CommandRunnerEvent
case object RunnerDeleteForward                                              extends CommandRunnerEvent
case object RunnerDeleteWordBackward                                         extends CommandRunnerEvent
case object RunnerDeleteWordForward                                          extends CommandRunnerEvent
case object RunnerPaste                                                      extends CommandRunnerEvent
final case class RunnerNavigate(direction: Direction)                        extends CommandRunnerEvent
final case class RunnerSelectVisibleItem(index: Int)                         extends CommandRunnerEvent
final case class RunnerSelectSubmenuItem(index: Int)                         extends CommandRunnerEvent
final case class RunnerSelectPreviewSubmenuItem(groupId: String, index: Int) extends CommandRunnerEvent
final case class RunnerSelectCategory(category: CommandCategory)             extends CommandRunnerEvent
case object RunnerNextCategory                                               extends CommandRunnerEvent
case object RunnerPreviousCategory                                           extends CommandRunnerEvent
case object RunnerSubmit                                                     extends CommandRunnerEvent
case object RunnerDismiss                                                    extends CommandRunnerEvent
final case class RunnerBindingRecordingExpired(recordedAtMillis: Long)       extends CommandRunnerEvent

final case class RunnerRecordBinding(
    info: KeyStrokeInfo,
    recordedAtMillis: Long = System.currentTimeMillis()
) extends CommandRunnerEvent

object CommandRunnerEvent:

  /** Tab moves between command categories here; the runner is the only surface that accepts a paste. */
  given SurfaceInput[CommandRunnerEvent] with

    def fromIntent(intent: FocusIntent): Option[CommandRunnerEvent] =
      intent match
        case FocusIntent.Insert(char)        => Some(RunnerInsertChar(char))
        case FocusIntent.DeleteBackward      => Some(RunnerDeleteBackward)
        case FocusIntent.DeleteForward       => Some(RunnerDeleteForward)
        case FocusIntent.DeleteWordBackward  => Some(RunnerDeleteWordBackward)
        case FocusIntent.DeleteWordForward   => Some(RunnerDeleteWordForward)
        case FocusIntent.Paste               => Some(RunnerPaste)
        case FocusIntent.Navigate(direction) => Some(RunnerNavigate(direction))
        case FocusIntent.NextGroup           => Some(RunnerNextCategory)
        case FocusIntent.PreviousGroup       => Some(RunnerPreviousCategory)
        case FocusIntent.Submit              => Some(RunnerSubmit)
        case FocusIntent.Dismiss             => Some(RunnerDismiss)

  def fromEvent(event: Event): Option[CommandRunnerEvent] =
    event match
      case runnerEvent: CommandRunnerEvent => Some(runnerEvent)
      case other                           => SurfaceInput.translate[CommandRunnerEvent](other)
