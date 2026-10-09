package com.serenity.io

import java.io.IOException
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.DosFileAttributeView
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption, StandardOpenOption}
import java.util.Locale

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try, Using}

import cats.effect.IO

/** Failure raised when a durable replacement of a target file cannot be completed. */
final class AtomicFileWriteException(val path: Path, cause: Throwable)
    extends RuntimeException(s"Failed to write $path atomically", cause)

/** JDK filesystem operations used by [[AtomicFileWriter]]. */
private[serenity] trait AtomicFileSystem:
  def createDirectories(path: Path): Path
  def createTempFile(directory: Path, prefix: String, suffix: String): Path
  def exists(path: Path): Boolean
  def realPath(path: Path): Path

  /** `None` where the filesystem exposes no link count (anything without the `unix` attribute view). */
  def linkCount(path: Path): Option[Int]

  def copyAttributes(source: Path, target: Path): Path
  def copyFile(source: Path, target: Path): Path
  def readBytes(path: Path): Array[Byte]
  def write(path: Path, bytes: Array[Byte]): Path

  /** Truncates and rewrites `path`'s existing inode rather than replacing it, so its other hardlinks see the write. */
  def overwriteInPlace(path: Path, bytes: Array[Byte]): Path

  def syncFile(path: Path): Unit
  def syncDirectory(directory: Path): Unit
  def moveAtomically(source: Path, target: Path): Path
  def moveReplacing(source: Path, target: Path): Path
  def deleteIfExists(path: Path): Boolean

/** What a failed fsync of the parent directory, after the rename that replaced a file, means for the save. */
private[serenity] enum DirectorySyncPolicy:
  /** POSIX: without the directory fsync the rename itself may not survive a crash, so the save reports it. */
  case Propagate

  /** Windows cannot open a directory as a `FileChannel` at all, so there is no directory fsync to perform. */
  case IgnoreFailure

  def run(sync: () => Unit): Unit =
    this match
      case DirectorySyncPolicy.Propagate => sync()
      case DirectorySyncPolicy.IgnoreFailure =>
        try sync()
        catch case _: IOException => ()

private[serenity] object DirectorySyncPolicy:

  def forOs(osName: String): DirectorySyncPolicy =
    if osName.toLowerCase(Locale.ROOT).startsWith("windows") then IgnoreFailure else Propagate

  val current: DirectorySyncPolicy = forOs(System.getProperty("os.name", ""))

/** Writes complete files through a temporary sibling before replacing the target. */
object AtomicFileWriter:

  private object JdkFileSystem extends AtomicFileSystem:
    def createDirectories(path: Path): Path = Files.createDirectories(path)

    def createTempFile(directory: Path, prefix: String, suffix: String): Path =
      Files.createTempFile(directory, prefix, suffix)

    def exists(path: Path): Boolean = Files.exists(path)

    def realPath(path: Path): Path = path.toRealPath()

    def linkCount(path: Path): Option[Int] =
      try
        Files.getAttribute(path, "unix:nlink") match
          case count: Integer => Some(count.intValue)
          case _              => None
      catch case _: UnsupportedOperationException | _: IllegalArgumentException => None

    def copyFile(source: Path, target: Path): Path =
      Files.copy(source, target, StandardCopyOption.COPY_ATTRIBUTES, StandardCopyOption.REPLACE_EXISTING)

    def readBytes(path: Path): Array[Byte] = Files.readAllBytes(path)

    def overwriteInPlace(path: Path, bytes: Array[Byte]): Path =
      Files.write(path, bytes, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)

    def syncFile(path: Path): Unit =
      Using.resource(FileChannel.open(path, StandardOpenOption.WRITE))(_.force(true))

    def syncDirectory(directory: Path): Unit =
      DirectorySyncPolicy.current.run(() => forceDirectory(directory))

    private def forceDirectory(directory: Path): Unit =
      Using.resource(FileChannel.open(directory, StandardOpenOption.READ))(_.force(true))

    // Carries `source`'s POSIX permissions onto the freshly-created (empty) `target` temp file, so the atomic move
    // that later replaces `source` with it doesn't silently narrow the file down to the temp file's own default,
    // restrictive permissions. Reads only that attribute, not `source`'s content: `Files.copy(..., COPY_ATTRIBUTES)`
    // -- this method's previous implementation -- copies the whole file to carry attributes over, only for that
    // content to be overwritten a moment later by `write` (#1444's second, wasted read of `source`, on top of
    // `DocumentStorage.saveLocal`'s own read for its revision hash). Non-POSIX filesystems (Windows NTFS/FAT) have
    // no POSIX permission bits, so `getPosixFilePermissions` throws `UnsupportedOperationException` there; fall
    // back to carrying over DOS attributes (read-only/hidden/archive/system) instead, which the old
    // `Files.copy(..., COPY_ATTRIBUTES)` preserved on Windows -- still attribute-only, never touching content.
    def copyAttributes(source: Path, target: Path): Path =
      try Files.setPosixFilePermissions(target, Files.getPosixFilePermissions(source))
      catch
        case _: UnsupportedOperationException =>
          (
            Option(Files.getFileAttributeView(source, classOf[DosFileAttributeView])),
            Option(Files.getFileAttributeView(target, classOf[DosFileAttributeView]))
          ) match
            case (Some(sourceView), Some(targetView)) => copyDosAttributes(sourceView, targetView)
            case _                                    => ()
      target

    def write(path: Path, bytes: Array[Byte]): Path = Files.write(path, bytes)

    def moveAtomically(source: Path, target: Path): Path =
      Files.move(source, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)

    def moveReplacing(source: Path, target: Path): Path =
      Files.move(source, target, StandardCopyOption.REPLACE_EXISTING)

    def deleteIfExists(path: Path): Boolean = Files.deleteIfExists(path)

  /** The real JDK-backed [[AtomicFileSystem]] every public `writeString`/`writeBytes` overload uses -- exposed only so
    * tests can exercise its behavior (e.g. `copyAttributes`) directly, the same visibility as the `fileSystem`-taking
    * overloads below.
    */
  private[serenity] def defaultFileSystem: AtomicFileSystem = JdkFileSystem

  /** Carries DOS attribute flags from `source` to `target` -- the non-POSIX fallback `copyAttributes` uses on Windows
    * filesystems. Exposed at the same visibility as `defaultFileSystem` so it can be tested directly against fakes,
    * independent of any real NTFS/FAT filesystem.
    */
  private[serenity] def copyDosAttributes(source: DosFileAttributeView, target: DosFileAttributeView): Unit =
    val attributes = source.readAttributes()
    target.setReadOnly(attributes.isReadOnly)
    target.setHidden(attributes.isHidden)
    target.setArchive(attributes.isArchive)
    target.setSystem(attributes.isSystem)

  /** Atomically replace `path` with UTF-8 text, falling back when atomic moves are unsupported. */
  def writeString(path: Path, content: String): IO[Unit] =
    writeBytes(path, content.getBytes(StandardCharsets.UTF_8))

  /** Atomically replace `path` with bytes, falling back when atomic moves are unsupported, and trying again when the OS
    * briefly refuses the replace (see [[RenameRetryPolicy]]). A write that still fails fails with its last error.
    */
  def writeBytes(path: Path, bytes: Array[Byte]): IO[Unit] =
    writeBytes(path, bytes, JdkFileSystem, RenameRetryPolicy.default)

  /** Atomically replace `path` with bytes from an existing blocking boundary. */
  def writeBytesBlocking(path: Path, bytes: Array[Byte]): Unit =
    writeBytesBlocking(path, bytes, JdkFileSystem)

  /** Renames the already-durable `source` over `target`, then syncs their directory so the rename survives a crash.
    * Unlike the writes above, `target` is not resolved through symlinks or kept on its hardlinks: for files the caller
    * owns outright, such as staged session files.
    */
  private[serenity] def replaceWith(source: Path, target: Path): IO[Unit] =
    RenameRetryPolicy.default.run(IO.blocking {
      try
        val _ = JdkFileSystem.moveAtomically(source, target)
      catch
        case _: AtomicMoveNotSupportedException =>
          val _ = JdkFileSystem.moveReplacing(source, target)
      Option(target.toAbsolutePath.getParent).foreach(JdkFileSystem.syncDirectory)
    })

  private[serenity] def writeString(path: Path, content: String, fileSystem: AtomicFileSystem): IO[Unit] =
    writeBytes(path, content.getBytes(StandardCharsets.UTF_8), fileSystem)

  private[serenity] def writeBytes(
    path: Path,
    bytes: Array[Byte],
    fileSystem: AtomicFileSystem,
    retry: RenameRetryPolicy = RenameRetryPolicy.none
  ): IO[Unit] =
    retry.run(
      IO.blocking(writeBytesBlocking(path, bytes, fileSystem)),
      causeOf = {
        case failed: AtomicFileWriteException => Option(failed.getCause).getOrElse(failed)
        case other                            => other
      }
    )

  private def writeBytesBlocking(path: Path, bytes: Array[Byte], fileSystem: AtomicFileSystem): Unit =
    // This is the synchronous boundary both writeBytes (via IO.blocking, which converts a thrown
    // exception into a failed IO) and ConfigManagerTestSupport.saveConfig (plain try/catch) rely on -- so it must
    // keep raising on failure. Try#get raises for us instead of a literal `throw`, and Try already only
    // catches NonFatal, matching the two catch clauses this replaces.
    Try {
      // Writing beside the symlink's real target, not the link, keeps the link in place (#1881).
      val target    = if fileSystem.exists(path) then fileSystem.realPath(path) else path.toAbsolutePath.normalize
      val directory = Option(target.getParent).getOrElse(target)
      val _         = fileSystem.createDirectories(directory)
      if fileSystem.exists(target) && fileSystem.linkCount(target).exists(_ > 1) then
        overwriteHardLinkedTarget(target, directory, bytes, fileSystem)
      else replaceAtomically(target, directory, bytes, fileSystem)
    }.recoverWith {
      case error: AtomicFileWriteException => Failure(error)
      case NonFatal(error)                 => Failure(AtomicFileWriteException(path, error))
    }.get

  private def temporaryPrefix(target: Path): String = s".${target.getFileName.toString}."

  // The temp file is fsynced before the rename and the directory after it: without the first, a crash can leave the
  // renamed file empty (ext4 delayed allocation); without the second, the rename itself may be lost.
  private def replaceAtomically(
    target: Path,
    directory: Path,
    bytes: Array[Byte],
    fileSystem: AtomicFileSystem
  ): Unit =
    val temporary = fileSystem.createTempFile(directory, temporaryPrefix(target), ".tmp")
    try
      if fileSystem.exists(target) then
        val _ = fileSystem.copyAttributes(target, temporary)
      val _ = fileSystem.write(temporary, bytes)
      fileSystem.syncFile(temporary)
      try
        val _ = fileSystem.moveAtomically(temporary, target)
      catch
        case _: AtomicMoveNotSupportedException =>
          val _ = fileSystem.moveReplacing(temporary, target)
      fileSystem.syncDirectory(directory)
    finally deleteQuietly(temporary, fileSystem)

  // A rename would give `target` a fresh inode and split it from its other hardlinks, so it is overwritten in place
  // instead (Vim's `backupcopy`). The in-place write is not atomic, hence the synced backup to restore from; the
  // backup is kept on disk only if that restore fails too, since it is then the sole copy of the original content.
  private def overwriteHardLinkedTarget(
    target: Path,
    directory: Path,
    bytes: Array[Byte],
    fileSystem: AtomicFileSystem
  ): Unit =
    val backup   = fileSystem.createTempFile(directory, temporaryPrefix(target), ".bak")
    val backedUp = Try(copyDurably(target, backup, fileSystem))
    val written  = backedUp.flatMap(_ => Try(overwriteDurably(target, bytes, fileSystem)))
    val targetIntact =
      (backedUp, written) match
        case (Success(_), Failure(writeError)) =>
          Try(overwriteDurably(target, fileSystem.readBytes(backup), fileSystem)) match
            case Success(_) => true
            case Failure(restoreError) =>
              writeError.addSuppressed(restoreError)
              false
        case _ => true
    if targetIntact then deleteQuietly(backup, fileSystem)
    written.get

  private def copyDurably(source: Path, target: Path, fileSystem: AtomicFileSystem): Unit =
    val _ = fileSystem.copyFile(source, target)
    fileSystem.syncFile(target)

  private def overwriteDurably(target: Path, bytes: Array[Byte], fileSystem: AtomicFileSystem): Unit =
    val _ = fileSystem.overwriteInPlace(target, bytes)
    fileSystem.syncFile(target)

  private def deleteQuietly(path: Path, fileSystem: AtomicFileSystem): Unit =
    try fileSystem.deleteIfExists(path): Unit
    catch case NonFatal(_) => ()
