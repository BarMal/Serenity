package com.serenity.input

import cats.effect.IO
import com.serenity.keystroke.events.*
import com.serenity.rope.LineEndings
import com.serenity.state.manager.{StateReader, StateUpdater}
import com.serenity.state.models.AppState

object ClipboardEventSync:

  /** Events that must read the system clipboard before they are applied. Copy and Cut need no isolation: whichever way
    * they ran (a key, or a menu command that enqueues them), [[exportIfCopied]] sees the batch they landed in.
    */
  def touchesSystemClipboard(event: Event): Boolean =
    event match
      case Paste => true
      case _     => false

  /** Writes the text a batch copied or cut to the system clipboard, once per Copy or Cut. */
  def exportIfCopied(before: AppState, after: AppState, systemClipboard: SystemClipboard[IO]): IO[Unit] =
    if after.runtime.clipboardExports == before.runtime.clipboardExports then IO.unit
    else after.runtime.clipboard.fold(IO.unit)(systemClipboard.writeText)

  def beforeEvent(
    event: Event,
    stateManager: StateUpdater,
    systemClipboard: SystemClipboard[IO]
  ): IO[Unit] =
    event match
      case Paste =>
        systemClipboard.readText.flatMap {
          case Some(text) =>
            val normalised = LineEndings.normalized(text)
            stateManager.updateStateValidated(state =>
              state.copy(runtime =
                state.runtime.copy(
                  clipboard = Some(normalised),
                  clipboardHistory = state.runtime.clipboardHistory.imported(normalised)
                )
              )
            )
          case None => IO.unit
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
          state.runtime.clipboard match
            case Some(text) => systemClipboard.writeText(text)
            case None       => IO.unit
        }
      case _ =>
        IO.unit
