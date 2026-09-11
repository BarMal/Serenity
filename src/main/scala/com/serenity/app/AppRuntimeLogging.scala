package com.serenity.app

import cats.effect.IO
import com.serenity.keystroke.events.{Event, UnhandledEvent}
import com.serenity.state.models.Focus
import org.typelevel.log4cats.Logger

/** Deciding what the input loop should log, and filtering the noisy events (system-generated `UnhandledEvent`s,
  * high-frequency navigation/typing events) it should not -- split out of [[AppRuntime]] purely to keep that file
  * within the architecture line-count ratchet; it carries no state of its own.
  */
private[app] object AppRuntimeLogging:

  def logSelectiveEvents(
    event: Event,
    currentFocus: Focus,
    logger: Logger[IO]
  ): IO[Unit] =
    event match
      case unhandled: UnhandledEvent[?] if !isSystemEvent(unhandled) =>
        logger.warn(s"[UNHANDLED] $event")
      case _: UnhandledEvent[?] =>
        logger.debug(s"[SYSTEM] $event")
      case _ if shouldLogFocusChange(event) =>
        logger.debug(s"[FOCUS] Event: $event, Focus: $currentFocus")
      case _ => IO.unit

  private def shouldLogFocusChange(event: Event): Boolean =
    event match
      case com.serenity.keystroke.events.MoveUp         => false
      case com.serenity.keystroke.events.MoveDown       => false
      case com.serenity.keystroke.events.MoveLeft       => false
      case com.serenity.keystroke.events.MoveRight      => false
      case com.serenity.keystroke.events.InsertChar(_)  => false
      case com.serenity.keystroke.events.DeleteBackward => false
      case com.serenity.keystroke.events.DeleteForward  => false
      case _                                            => true

  private def isSystemEvent(event: UnhandledEvent[?]): Boolean =
    import com.serenity.keystroke.InputKey
    event.info.keyType match
      case InputKey.EOF     => false
      case InputKey.Unknown => true
      case InputKey.Character =>
        event.info.character.exists { char =>
          char.toInt == 0 ||
          char.toInt == 4 ||
          char.toInt == 26
        }
      case _ => false
