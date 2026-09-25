package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.{Deferred, IO}
import cats.syntax.all.*
import com.serenity.io.{FileDialog, FileManager, FileUtils}
import com.serenity.lsp.LspEffect
import com.serenity.session.SessionPersistence
import com.serenity.state.effects.{EffectLanes, Lane, LaneKey, LanePolicy}
import com.serenity.state.models.*
import org.typelevel.log4cats.Logger

/** Where file work runs and how its results get back (#1697 Wave 3). */
private[manager] trait FileEffectLanes:
  def fileWrites: FileWriteLedger

  /** Fails with [[EffectLanes.Released]] once effects have shut down. */
  def submitToLane(lane: Lane.Scheduled, job: IO[Unit]): IO[Unit]

  /** Queues `update` for the dispatcher without waiting; safe from anywhere, including the dispatcher itself. */
  def post(update: IO[Unit]): IO[Unit]

  /** Runs `update` on the dispatcher and waits for it; never call it from the dispatcher. */
  def dispatchUpdate(update: IO[Unit]): IO[Unit]

/** Owns file reads and writes for buffers (#1671, #1672). The disk I/O runs on a `LaneKey.File` Sequential lane, one
  * per canonical path, and its result is merged into the state current when it lands -- never into the snapshot the
  * request was made from.
  *
  * Methods named `submit*` and [[loadFile]] return once the work is queued: callers on the dispatcher use them so the
  * dispatcher never waits on the disk. The others wait for the lane job and then apply its result on the caller's
  * fiber, for callers whose next step depends on the outcome (save-before-close, save-as dialogs, the reload prompt);
  * waiting on a lane job from the dispatcher is safe because file jobs only ever post back.
  */
final private[manager] class StateManagerFilePersistence(
    currentState: IO[AppState],
    commitState: (AppState, AppState) => IO[Unit],
    fileManager: FileManager,
    sessionPersistence: SessionPersistence,
    logger: Logger[IO],
    lspQueue: LspEffectQueue,
    lanes: FileEffectLanes
):
  import StateManagerFilePersistence.*

  private val writes = lanes.fileWrites

  /** Saves in the background; `onFailure` runs on the dispatcher if the save fails. */
  def submitSave(bufferId: BufferId, onFailure: Throwable => IO[Unit]): IO[Unit] =
    request(bufferId, SaveKind.Save, None).flatMap(_.fold(IO.unit) { save =>
      PendingSave.create.flatTap(writes.addPending(bufferId, _)).flatMap { pending =>
        val job = saveJob(save)
          .onCancel(pending.result.complete(EffectResult.FileSaveFailed(save, JobCancelled())).void)
          .flatTap(pending.result.complete)
          .flatMap(result => lanes.post(applyOnce(bufferId, pending, commitSave(result, onFailure))))
        lanes.submitToLane(fileLane(save.target), job).recoverWith {
          case _: EffectLanes.Released =>
            writes.settle(canonical(save.target)) >> writes.removePending(bufferId, pending)
        }
      }
    })

  /** Waits for the background saves of `bufferIds` and applies their results now, so a close deciding whether these
    * buffers are unsaved sees the outcome of a save the user already asked for: Ctrl+S then Ctrl+Q must not prompt. A
    * failed save leaves its buffer dirty, so the close goes on to prompt for it.
    */
  def settlePendingSaves(bufferIds: List[BufferId]): IO[Unit] =
    bufferIds.traverse_(bufferId =>
      writes
        .pendingFor(bufferId)
        .flatMap(
          _.traverse_(pending =>
            pending.result.get.flatMap(result =>
              applyOnce(
                bufferId,
                pending,
                commitSave(result, error => logger.warn(error)(s"[FILE] Save of buffer $bufferId failed before close"))
              )
            )
          )
        )
    )

  private def applyOnce(bufferId: BufferId, pending: PendingSave, apply: IO[Unit]): IO[Unit] =
    pending.claim.flatMap(claimed => if claimed then writes.removePending(bufferId, pending) >> apply else IO.unit)

  def saveExistingBuffer(bufferId: BufferId): IO[Unit] =
    saveAndWait(bufferId, SaveKind.Save, None)

  /** Re-saves the buffer bypassing the revision conflict check (#1623) -- the "Overwrite" choice on the reload conflict
    * prompt, after the user has explicitly confirmed they want their edits to win over the external change.
    */
  def forceSaveExistingBuffer(bufferId: BufferId): IO[Unit] =
    saveAndWait(bufferId, SaveKind.Force, None)

  def saveBufferAs(bufferId: BufferId, path: Path): IO[Unit] =
    saveAndWait(bufferId, SaveKind.SaveAs, Some(path))

  /** Runs `check` on `path`'s file lane and waits for it: a save-as reading, just before it writes, what the write
    * would have to create. Like the save that follows, it runs after any write already queued for `path`.
    */
  def inspectBeforeSave[A](path: Path, check: IO[A]): IO[A] = awaitLane(fileLane(path), check)

  /** Whether a save to `path` has not yet been applied -- until it has, the disk may hold this process's own write
    * while the buffer still records the revision before it.
    */
  def isSaving(path: Path): IO[Boolean] = writes.isSaving(canonical(path))

  /** Re-reads the buffer's file from disk in place (#1623) -- the "Reload" choice on the reload conflict prompt, and
    * the silent path a clean buffer takes when an external-change check finds the file changed.
    */
  def reloadBuffer(bufferId: BufferId): IO[Unit] =
    currentState.flatMap { state =>
      state.persisted.buffers.get(bufferId).flatMap(buffer => buffer.document.filePath.map(buffer -> _)) match
        case None => IO.unit
        case Some((buffer, path)) =>
          awaitLane(fileLane(path), fileManager.reloadBuffer(buffer)).flatMap(disk =>
            commit(EffectResult.FileReloaded(bufferId, path, buffer.document.content, disk)) >> persistAfterSave
          )
    }

  /** Opens `path` into a new focused buffer in the background. */
  def loadFile(path: Path): IO[Unit] =
    submitIgnoringShutdown(
      fileLane(path),
      loadJob(path).flatMap(_.fold(IO.unit)(result => lanes.post(commitLoad(result))))
    )

  /** Opens `path` and waits until the buffer is in the state -- for callers off the dispatcher, such as startup. */
  def openFile(path: Path): IO[Unit] =
    awaitLane(fileLane(path), loadJob(path)).flatMap(
      _.fold(IO.unit)(result => lanes.dispatchUpdate(commitLoad(result)))
    )

  /** Shows `dialog` off the dispatcher and opens what it returns. A second request while it is up is dropped. */
  def openFromDialog(dialog: FileDialog): IO[Unit] =
    submitIgnoringShutdown(
      DialogLane,
      FileUtils.getCurrentDirectory
        .flatMap(directory => dialog.chooseOpenFile(Some(directory)))
        .flatMap(_.fold(IO.unit)(loadFile))
        .handleErrorWith(error => logger.error(error)("[FILE] Native open-file dialog failed"))
    )

  private def request(bufferId: BufferId, kind: SaveKind, saveAsPath: Option[Path]): IO[Option[FileSave]] =
    currentState.flatMap { state =>
      state.persisted.buffers
        .get(bufferId)
        .flatMap(buffer => saveAsPath.orElse(buffer.document.filePath).map(buffer -> _)) match
        case None => IO.none
        case Some((buffer, target)) =>
          writes.begin(canonical(target)).map(written => Some(FileSave(bufferId, target, buffer, kind, written)))
    }

  private def saveAndWait(bufferId: BufferId, kind: SaveKind, saveAsPath: Option[Path]): IO[Unit] =
    request(bufferId, kind, saveAsPath).flatMap(
      _.fold(IO.unit)(save =>
        awaitLane(fileLane(save.target), saveJob(save))
          .onError(_ => writes.settle(canonical(save.target)))
          .flatMap(commitSave(_, IO.raiseError))
      )
    )

  /** Never fails: a failed write comes back as [[EffectResult.FileSaveFailed]]. */
  private def saveJob(save: FileSave): IO[EffectResult] =
    val path = canonical(save.target)
    val write =
      save.kind match
        case SaveKind.Force => fileManager.saveBuffer(withRevision(save.snapshot, None))
        case SaveKind.Save =>
          writes
            .expectedRevision(path, save.writtenAtSubmit, save.snapshot.document.revision)
            .flatMap(expected => fileManager.saveBuffer(withRevision(save.snapshot, expected)))
        case SaveKind.SaveAs => fileManager.saveBuffer(save.snapshot, save.target)
    write.attempt.flatMap {
      case Right(saved) => writes.wrote(path, saved.document.revision).as(EffectResult.FileSaved(save, saved))
      case Left(error)  => IO.pure(EffectResult.FileSaveFailed(save, error))
    }

  private def commitSave(result: EffectResult, onFailure: Throwable => IO[Unit]): IO[Unit] =
    result match
      case saved @ EffectResult.FileSaved(save, savedBuffer) =>
        writes.settle(canonical(save.target)) >> commit(saved) >> persistAfterSave >>
          (if save.kind == SaveKind.SaveAs then refreshLspBindingAfterSaveAs(save.snapshot, savedBuffer) else IO.unit)
      case EffectResult.FileSaveFailed(save, error) =>
        writes.settle(canonical(save.target)) >> onFailure(error)
      case _ => IO.unit

  /** `None` when `path` is not a readable file, which is logged and otherwise ignored, as it always has been. */
  private def loadJob(path: Path): IO[Option[EffectResult]] =
    IO.blocking(FileUtils.isReadableFile(path)).flatMap {
      case false => logger.warn(s"[FILE] Not a readable file: $path").as(None)
      case true =>
        fileManager
          .loadFile(path, PlaceholderBufferId)
          .attempt
          .map(loaded => Some(loaded.fold(EffectResult.FileLoadFailed(path, _), EffectResult.FileLoaded(path, _))))
    }

  private def commitLoad(result: EffectResult): IO[Unit] =
    result match
      case loaded @ EffectResult.FileLoaded(path, buffer) =>
        commit(loaded) >> announceOpenedToLsp(path, buffer)
      case EffectResult.FileLoadFailed(path, error) =>
        logger.error(error)(s"[FILE] Failed to load file at $path")
      case _ => IO.unit

  private def announceOpenedToLsp(path: Path, loaded: Buffer): IO[Unit] =
    loaded.document.language.fold(IO.unit) { languageId =>
      currentState.flatMap { state =>
        val opened = state.persisted.buffers.values.exists(_.document.filePath.contains(path))
        if opened && state.editingContext.hasCodeTooling then
          lspQueue.enqueue(LspEffect.FileOpened(path.toUri.toString, languageId, loaded.document.content.collect()))
        else IO.unit
      }
    }

  private def commit(result: EffectResult): IO[Unit] =
    currentState.flatMap(state => commitState(EffectResult.applyIfCurrent(state, result), state))

  private def awaitLane[A](lane: Lane.Scheduled, job: IO[A]): IO[A] =
    Deferred[IO, Either[Throwable, A]].flatMap { outcome =>
      val reported = job.attempt.flatMap(outcome.complete).void.onCancel(outcome.complete(Left(JobCancelled())).void)
      lanes.submitToLane(lane, reported) >> outcome.get.rethrow
    }

  // A request arriving after shutdown has nothing left to run on, and quitting does not want it anyway.
  private def submitIgnoringShutdown(lane: Lane.Scheduled, job: IO[Unit]): IO[Unit] =
    lanes.submitToLane(lane, job).recover { case _: EffectLanes.Released => () }

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
        currentState.flatMap { state =>
          if !state.editingContext.hasCodeTooling then IO.unit
          else
            next.fold(IO.unit) {
              case (uri, languageId, text) =>
                lspQueue.enqueue(LspEffect.FileOpened(uri, languageId, text))
            }
        }

  private def persistAfterSave: IO[Unit] =
    currentState
      .flatMap(sessionPersistence.onBufferChange)
      .handleErrorWith(error => logger.error(error)("[SESSION] Auto-save after file save failed"))

private[manager] object StateManagerFilePersistence:

  val DialogLane: Lane.Keyed = Lane.Keyed(LaneKey.Dialog, LanePolicy.DropIfBusy)

  def fileLane(path: Path): Lane.Keyed = Lane.Keyed(LaneKey.File(canonical(path)), LanePolicy.Sequential)

  def canonical(path: Path): Path = FileWriteLedger.canonical(path)

  /** `FileResults.loaded` assigns the real id when the load is applied. */
  private val PlaceholderBufferId = BufferId(-1)

  private def withRevision(buffer: Buffer, revision: Option[com.serenity.io.DocumentRevision]): Buffer =
    buffer.copy(document = buffer.document.copy(revision = revision))

  final class JobCancelled extends IllegalStateException("file job was cancelled before it finished")
