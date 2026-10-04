package com.serenity.state.manager

import scala.concurrent.duration.FiniteDuration

import cats.effect.IO
import com.serenity.state.models.AppState

/** Saves the session once edits to unsaved buffers have paused for `idle`, so a crash does not lose their text (#1904).
  * A restored session already brings a dirty buffer back with its unsaved text, so the session is the backup.
  */
final private[manager] case class EditIdleSessionSave(idle: FiniteDuration, save: AppState => IO[Unit])

private[manager] object EditIdleSessionSave:

  /** Whether a commit changed the text of a buffer that only the session keeps: one with unsaved changes, or hidden. */
  def due(before: AppState, after: AppState): Boolean =
    (before.persisted.buffers ne after.persisted.buffers) &&
      after.persisted.buffers.exists { (id, buffer) =>
        val textChanged = !before.persisted.buffers.get(id).exists(_.document.content eq buffer.document.content)
        textChanged && (buffer.hidden || buffer.hasUnsavedChanges)
      }
