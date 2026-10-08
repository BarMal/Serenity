package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.config.AutoSaveMode
import com.serenity.state.models.{AppState, BufferId, FileFailureNotice, Notice}
import org.typelevel.log4cats.Logger

/** Writes buffers through the same save as Ctrl+S, so a stale file, a refused write and the revision check all behave
  * as they do for a save the user asked for (#1992).
  *
  * The one difference is how a failure is reported: a notice and never a prompt, because a conflict dialog raised
  * mid-sentence would swallow the keystrokes that follow it.
  */
final private[manager] class StateManagerAutoSave(
    currentState: IO[AppState],
    isSaving: Path => IO[Boolean],
    submitSave: (BufferId, Throwable => IO[Unit]) => IO[Unit],
    showNotice: Notice => IO[Unit],
    logger: Logger[IO]
):

  /** Runs off the dispatcher: it only queues the write, which the file's own lane performs. The mode is re-read because
    * the setting may have been switched off while a delayed save was waiting.
    */
  def saveBuffer(bufferId: BufferId): IO[Unit] =
    currentState.flatMap { state =>
      state.persisted.buffers
        .get(bufferId)
        .flatMap(_.document.filePath)
        .filter(_ => state.persisted.config.autoSaveConfig.mode != AutoSaveMode.Off)
        .filter(_ => AutoSave.savableIn(state, bufferId))
        .traverse_(path => isSaving(path).ifM(IO.unit, submitSave(bufferId, failed(bufferId))))
    }

  /** The window has lost focus: both focus modes write everything unsaved. */
  def saveOnWindowFocusLost: IO[Unit] =
    currentState.flatMap { state =>
      state.persisted.config.autoSaveConfig.mode match
        case AutoSaveMode.OnFocusChange | AutoSaveMode.OnWindowChange =>
          AutoSave.savableBuffers(state).traverse_(saveBuffer)
        case AutoSaveMode.Off | AutoSaveMode.AfterDelay => IO.unit
    }

  private def failed(bufferId: BufferId)(error: Throwable): IO[Unit] =
    logger.warn(error)(s"[FILE] Auto-save of buffer $bufferId failed") >>
      currentState.flatMap(state => showNotice(FileFailureNotice.forBuffer(state, bufferId, error)))
