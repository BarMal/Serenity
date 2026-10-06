package com.serenity.state.models

import java.io.IOException
import java.nio.file.{
  AccessDeniedException,
  FileAlreadyExistsException,
  NoSuchFileException,
  NotDirectoryException,
  Path,
  ReadOnlyFileSystemException
}

import com.serenity.config.{AppConfig, HotkeyAction}
import com.serenity.io.{DocumentStorageError, FileManagerError}
import com.serenity.richtext.LossyRichTextOverwriteException

/** The notices a failed save, open or reload shows (#1717): the file by name, and the cause in words a user can act on.
  */
object FileFailureNotice:

  private val Dismiss = "esc dismiss"

  def fileSaveFailed(bufferId: BufferId, target: Path, error: Throwable, config: AppConfig): Notice =
    saveFailed(bufferId, fileName(target), error, config)

  /** [[fileSaveFailed]] for the buffer's own file, or its label when it has none yet. */
  def forBuffer(state: AppState, bufferId: BufferId, error: Throwable): Notice =
    val name = state.persisted.buffers
      .get(bufferId)
      .flatMap(_.document.filePath)
      .fold(s"Buffer ${bufferId.value}")(fileName)
    saveFailed(bufferId, name, error, state.persisted.config)

  private def saveFailed(bufferId: BufferId, name: String, error: Throwable, config: AppConfig): Notice =
    Notice(
      NoticeLevel.Error,
      s"Couldn't save $name: ${cause(error)}.",
      hint = Some(saveHint(config)),
      topic = Some(NoticeTopic.FileSave(bufferId))
    )

  def openFailed(path: Path, error: Throwable): Notice =
    Notice(NoticeLevel.Error, s"Couldn't open ${fileName(path)}: ${cause(error)}.", hint = Some(Dismiss))

  def notReadable(path: Path): Notice =
    Notice(
      NoticeLevel.Error,
      s"Couldn't open ${fileName(path)}: it doesn't exist or can't be read.",
      hint = Some(Dismiss)
    )

  def reloadFailed(path: Path, error: Throwable): Notice =
    Notice(NoticeLevel.Error, s"Couldn't reload ${fileName(path)}: ${cause(error)}.", hint = Some(Dismiss))

  val stayedInEditor: Notice =
    Notice(
      NoticeLevel.Warning,
      "Stayed in the editor because it changed while the session was saving.",
      hint = Some(Dismiss),
      topic = Some(NoticeTopic.SessionSave)
    )

  def sessionSaveFailed(error: Throwable): Notice =
    Notice(
      NoticeLevel.Error,
      s"Couldn't save the session: ${cause(error)}.",
      hint = Some(Dismiss),
      topic = Some(NoticeTopic.SessionSave)
    )

  /** The session is also the crash backup of unsaved edits (#1904), which is what this warns about. */
  def sessionBackupFailed(error: Throwable): Notice =
    Notice(
      NoticeLevel.Warning,
      s"Couldn't back up this session: ${cause(error)}. Unsaved changes may not survive a crash.",
      hint = Some(Dismiss),
      topic = Some(NoticeTopic.SessionSave)
    )

  /** Why a file operation failed, in plain words; the error's own message only when no plainer cause is known. */
  def cause(error: Throwable): String =
    error match
      case FileManagerError.StorageFailure(storageError) => storageCause(storageError)
      case FileManagerError.ExternalConflict(_)          => "the file changed on disk since it was opened"
      case FileManagerError.UnsupportedForSave(fileType) => s"${fileType.displayName} files can't be saved"
      case FileManagerError.UnsupportedForOpen(fileType) => s"${fileType.displayName} files can't be opened"
      case FileManagerError.BinaryContent(_)             => "it isn't a text file"
      case FileManagerError.NoFilePath()                 => "it has no file yet"
      case _: LossyRichTextOverwriteException            => "saving there would lose its formatting"
      case _: AccessDeniedException                      => "permission denied"
      case _: NoSuchFileException                        => "it no longer exists"
      case _: NotDirectoryException                      => "part of its path is not a folder"
      case _: FileAlreadyExistsException                 => "a file is in the way of its folder"
      case _: ReadOnlyFileSystemException                => "the disk is read-only"
      case io: IOException if isDiskFull(io.getMessage)  => "the disk is full"
      case other                                         => ownMessage(other)

  private def storageCause(error: DocumentStorageError): String =
    error match
      case DocumentStorageError.AccessDenied(_)                        => "permission denied"
      case DocumentStorageError.NotFound(_)                            => "it no longer exists"
      case DocumentStorageError.Conflict(_)                            => "the file changed on disk since it was opened"
      case DocumentStorageError.Offline(providerId)                    => s"$providerId is offline"
      case DocumentStorageError.AuthenticationFailed(id)               => s"signing in to $id failed"
      case DocumentStorageError.UnsupportedLocation(_)                 => "that location isn't supported"
      case DocumentStorageError.Cancelled                              => "it was cancelled"
      case DocumentStorageError.Failed(message) if isDiskFull(message) => "the disk is full"
      case DocumentStorageError.Failed(message)                        => message

  private def isDiskFull(message: String): Boolean =
    Option(message).exists(_.contains("No space left on device"))

  private def ownMessage(error: Throwable): String =
    Option(error.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)

  private def saveHint(config: AppConfig): String =
    val hotkeys = config.inputConfig.hotkeyConfig
    def keys(action: HotkeyAction, label: String): Option[String] =
      hotkeys.bindingsFor(action).headOption.map(trigger => s"${trigger.render} $label")
    (keys(HotkeyAction.Save, "retry").toList ++ keys(HotkeyAction.SaveAs, "save as").toList :+ Dismiss)
      .mkString(" · ")

  private def fileName(path: Path): String =
    Option(path.getFileName).fold(path.toString)(_.toString)
