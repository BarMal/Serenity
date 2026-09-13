package com.serenity.keystroke.events

sealed trait StartupPageEvent

case object StartupPageMoveUp                  extends StartupPageEvent
case object StartupPageMoveDown                extends StartupPageEvent
case object StartupPageSubmit                  extends StartupPageEvent
case object StartupPageDismiss                 extends StartupPageEvent
case object StartupPageResume                  extends StartupPageEvent
final case class StartupPageSelect(index: Int)   extends StartupPageEvent
final case class StartupPageShortcut(char: Char) extends StartupPageEvent

object StartupPageEvent:

  given SurfaceInput[StartupPageEvent] with

    def fromIntent(intent: FocusIntent): Option[StartupPageEvent] =
      intent match
        case FocusIntent.Navigate(Direction.Up)                      => Some(StartupPageMoveUp)
        case FocusIntent.Navigate(Direction.Down)                    => Some(StartupPageMoveDown)
        case FocusIntent.Navigate(Direction.Left | Direction.Right)  => None
        case FocusIntent.Submit                                      => Some(StartupPageSubmit)
        case FocusIntent.Dismiss                                     => Some(StartupPageDismiss)
        case FocusIntent.NextGroup                                   => Some(StartupPageResume)
        case FocusIntent.Insert(char) if char.isDigit && char != '0' => Some(StartupPageSelect(char.asDigit - 1))
        case FocusIntent.Insert(char) if char.isLetter               => Some(StartupPageShortcut(char))
        case FocusIntent.Insert(_)                                   => None
        case FocusIntent.DeleteBackward | FocusIntent.DeleteForward | FocusIntent.DeleteWordBackward |
            FocusIntent.DeleteWordForward | FocusIntent.Paste | FocusIntent.PreviousGroup =>
          None

  def fromEvent(event: Event): Option[StartupPageEvent] =
    event match
      case startupEvent: StartupPageEvent => Some(startupEvent)
      case other                          => SurfaceInput.translate[StartupPageEvent](other)
