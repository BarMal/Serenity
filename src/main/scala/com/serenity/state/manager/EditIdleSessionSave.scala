package com.serenity.state.manager

import scala.concurrent.duration.FiniteDuration

import cats.effect.IO
import com.serenity.state.models.{AppState, Buffer, BufferMapChanges}

/** Saves the session once edits to unsaved buffers have paused for `idle`, so a crash does not lose their text (#1904).
  * A restored session already brings a dirty buffer back with its unsaved text, so the session is the backup.
  */
final private[manager] case class EditIdleSessionSave(idle: FiniteDuration, save: AppState => IO[Unit])

private[manager] object EditIdleSessionSave:

  /** Whether a commit changed the text of a buffer that only the session keeps: one with unsaved changes, or hidden. */
  def due(before: AppState, after: AppState): Boolean =
    BufferMapChanges.anyChanged(before.persisted.buffers, after.persisted.buffers)(
      added = onlySessionKeepsText,
      changed =
        (previous, buffer) => (previous.document.content ne buffer.document.content) && onlySessionKeepsText(buffer)
    )

  private def onlySessionKeepsText(buffer: Buffer): Boolean = buffer.hidden || buffer.hasUnsavedChanges
