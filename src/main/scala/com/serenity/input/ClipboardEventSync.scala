package com.serenity.input

import cats.effect.IO
import com.serenity.keystroke.events.*
import com.serenity.state.manager.{StateReader, StateUpdater}

object ClipboardEventSync:

  def beforeEvent(
    event: Event,
    stateManager: StateUpdater,
    systemClipboard: SystemClipboard[IO]
  ): IO[Unit] =
    event match
      case Paste =>
        systemClipboard.readText.flatMap {
          case Some(text) => stateManager.updateState(_.copy(clipboard = Some(text)))
          case None       => IO.unit
        }
      case _ =>
        IO.unit

  def afterEvent(
    event: Event,
    stateManager: StateReader,
    systemClipboard: SystemClipboard[IO]
  ): IO[Unit] =
    event match
      case Copy | Cut =>
        stateManager.getCurrentState.flatMap { state =>
          state.clipboard match
            case Some(text) => systemClipboard.writeText(text)
            case None       => IO.unit
        }
      case _ =>
        IO.unit
