package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.{IO, Ref}
import com.serenity.io.FileManager
import com.serenity.lsp.LspEffect
import com.serenity.session.SessionPersistence
import com.serenity.state.models.*
import org.typelevel.log4cats.Logger

/** Owns file persistence state updates shared by command effects and file workflows. */
final private[manager] class StateManagerFilePersistence(
    stateRef: Ref[IO, AppState],
    fileManager: FileManager,
    sessionPersistence: SessionPersistence,
    logger: Logger[IO],
    lspQueue: LspEffectQueue
):

  def saveExistingBuffer(bufferId: BufferId): IO[Unit] =
    stateRef.get.flatMap { state =>
      state.persisted.buffers.get(bufferId).flatMap(_.document.filePath) match
        case Some(_) =>
          state.persisted.buffers.get(bufferId).fold(IO.unit) { buffer =>
            fileManager
              .saveBuffer(buffer)
              .flatMap(saved =>
                stateRef.update(current =>
                  current
                    .copy(persisted = current.persisted.copy(buffers = current.persisted.buffers + (bufferId -> saved)))
                )
              )
              .flatTap(_ => persistAfterSave)
          }
        case None => IO.unit
    }

  /** Re-saves the buffer bypassing the revision conflict check (#1623) -- the "Overwrite" choice on the reload
    * conflict prompt, after the user has explicitly confirmed they want their edits to win over the external change.
    * Clearing the buffer's captured revision before saving is what tells `FileManager` there's nothing to check
    * against, mirroring the "no expected revision" case a first-ever save already goes through.
    */
  def forceSaveExistingBuffer(bufferId: BufferId): IO[Unit] =
    stateRef.get.flatMap { state =>
      state.persisted.buffers.get(bufferId).fold(IO.unit) { buffer =>
        fileManager
          .saveBuffer(buffer.copy(document = buffer.document.copy(revision = None)))
          .flatMap(saved =>
            stateRef.update(current =>
              current
                .copy(persisted = current.persisted.copy(buffers = current.persisted.buffers + (bufferId -> saved)))
            )
          )
          .flatTap(_ => persistAfterSave)
      }
    }

  /** Re-reads the buffer's file from disk in place (#1623) -- the "Reload" choice on the reload conflict prompt, and
    * the silent path a clean (no unsaved edits) buffer takes on a focus-in re-check that finds the file changed.
    */
  def reloadBuffer(bufferId: BufferId): IO[Unit] =
    stateRef.get.flatMap { state =>
      state.persisted.buffers.get(bufferId).fold(IO.unit) { buffer =>
        fileManager
          .reloadBuffer(buffer)
          .flatMap(reloaded =>
            stateRef.update(current =>
              current
                .copy(persisted = current.persisted.copy(buffers = current.persisted.buffers + (bufferId -> reloaded)))
            )
          )
          .flatTap(_ => persistAfterSave)
      }
    }

  def saveBufferAs(bufferId: BufferId, path: Path): IO[Unit] =
    stateRef.get.flatMap { state =>
      state.persisted.buffers.get(bufferId).fold(IO.unit) { buffer =>
        fileManager
          .saveBuffer(buffer, path)
          .flatMap { saved =>
            stateRef.update(current =>
              current
                .copy(persisted = current.persisted.copy(buffers = current.persisted.buffers + (bufferId -> saved)))
            ) >>
              refreshLspBindingAfterSaveAs(buffer, saved)
          }
          .flatTap(_ =>
            stateRef.update(current =>
              current.copy(persisted =
                current.persisted.copy(
                  recentFiles = trackRecentFile(current.persisted.recentFiles, path),
                  recentFilesByMode = Persisted
                    .trackRecentFile(current.persisted.recentFilesByMode, current.persisted.config.appMode, path)
                )
              )
            )
          )
          .flatTap(_ => persistAfterSave)
      }
    }

  private def refreshLspBindingAfterSaveAs(before: Buffer, saved: Buffer): IO[Unit] =
    val previous = for
      path       <- before.document.filePath
      languageId <- before.document.language
    yield (path.toUri.toString, languageId)
    val next = for
      path       <- saved.document.filePath
      languageId <- saved.document.language
    yield (path.toUri.toString, languageId, saved.document.content.collect())
    val nextIdentity = next.map { case (uri, languageId, _) => (uri, languageId) }
    if previous == nextIdentity then IO.unit
    else
      previous.fold(IO.unit) {
        case (uri, languageId) =>
          lspQueue.enqueue(LspEffect.FileClosed(uri, languageId))
      } >>
        stateRef.get.flatMap { state =>
          if !state.editingContext.hasCodeTooling then IO.unit
          else
            next.fold(IO.unit) {
              case (uri, languageId, text) =>
                lspQueue.enqueue(LspEffect.FileOpened(uri, languageId, text))
            }
        }

  private def persistAfterSave: IO[Unit] =
    stateRef.get
      .flatMap(sessionPersistence.onBufferChange)
      .handleErrorWith(error => logger.error(error)("[SESSION] Auto-save after file save failed"))

  private def trackRecentFile(current: List[Path], path: Path): List[Path] =
    (path :: current.filterNot(_ == path)).take(20)
