package com.serenity

import java.io.IOException
import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.net.URI
import java.nio.channels.SeekableByteChannel
import java.nio.file.attribute.{
  BasicFileAttributes,
  DosFileAttributeView,
  DosFileAttributes,
  FileAttribute,
  FileTime,
  PosixFileAttributeView,
  PosixFilePermission,
  UserPrincipalLookupService
}
import java.nio.file.spi.FileSystemProvider
import java.nio.file.{
  AccessMode,
  AtomicMoveNotSupportedException,
  CopyOption,
  DirectoryStream,
  FileStore,
  FileSystem,
  Files,
  LinkOption,
  OpenOption,
  Path,
  PathMatcher,
  StandardCopyOption,
  WatchService
}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}

import scala.jdk.CollectionConverters.*

import cats.effect.unsafe.implicits.global
import com.serenity.io.{AtomicFileSystem, AtomicFileWriteException, AtomicFileWriter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AtomicFileWriterSpec extends AnyFlatSpec with Matchers:

  /** Fake [[DosFileAttributes]] carrying only the four DOS attribute flags this spec cares about. */
  final private class FakeDosFileAttributes(
      readOnly: Boolean,
      hidden: Boolean,
      archive: Boolean,
      system: Boolean
  ) extends DosFileAttributes:
    def isReadOnly: Boolean          = readOnly
    def isHidden: Boolean            = hidden
    def isArchive: Boolean           = archive
    def isSystem: Boolean            = system
    def lastModifiedTime(): FileTime = FileTime.fromMillis(0)
    def lastAccessTime(): FileTime   = FileTime.fromMillis(0)
    def creationTime(): FileTime     = FileTime.fromMillis(0)
    def isRegularFile: Boolean       = true
    def isDirectory: Boolean         = false
    def isSymbolicLink: Boolean      = false
    def isOther: Boolean             = false
    def size(): Long                 = 0L
    def fileKey(): AnyRef            = null

  /** Fake [[DosFileAttributeView]] recording which flags were set on it, so a test can assert the fallback copied every
    * flag across without touching any file on disk.
    */
  final private case class ObservedDosFlags(
      readOnly: Option[Boolean] = None,
      hidden: Option[Boolean] = None,
      archive: Option[Boolean] = None,
      system: Option[Boolean] = None
  )

  final private class FakeDosFileAttributeView(private val attributes: FakeDosFileAttributes)
      extends DosFileAttributeView:
    private val observed = AtomicReference(ObservedDosFlags())

    def name(): String                                                                             = "dos"
    def readAttributes(): DosFileAttributes                                                        = attributes
    def setTimes(lastModifiedTime: FileTime, lastAccessTime: FileTime, createTime: FileTime): Unit = ()
    def setReadOnly(value: Boolean): Unit = observed.updateAndGet(_.copy(readOnly = Some(value))): Unit
    def setHidden(value: Boolean): Unit   = observed.updateAndGet(_.copy(hidden = Some(value))): Unit
    def setArchive(value: Boolean): Unit  = observed.updateAndGet(_.copy(archive = Some(value))): Unit
    def setSystem(value: Boolean): Unit   = observed.updateAndGet(_.copy(system = Some(value))): Unit

    def observedFlags: (Option[Boolean], Option[Boolean], Option[Boolean], Option[Boolean]) =
      val flags = observed.get
      (flags.readOnly, flags.hidden, flags.archive, flags.system)

  /** Wraps a real [[Path]] so every content-touching filesystem operation (`copy`, `newByteChannel`) it reaches through
    * is counted, while attribute-only operations (`readAttributes`, `getFileAttributeView`, `setAttribute`, ...) are
    * not -- letting a test prove `copyAttributes` never reads `source`'s content, rather than merely asserting on its
    * end-state (which can't distinguish "content never read" from "content read internally but discarded").
    */
  final private class ContentReadCountingProvider(delegate: FileSystemProvider) extends FileSystemProvider:
    val contentReads: AtomicInteger = AtomicInteger(0)

    private def unwrap(path: Path): Path =
      if path != null && Proxy.isProxyClass(path.getClass) then
        Proxy.getInvocationHandler(path) match
          case handler: CountingPathHandler => handler.real
          case _                            => path
      else path

    def getScheme: String                                                  = delegate.getScheme
    def newFileSystem(uri: URI, env: java.util.Map[String, ?]): FileSystem = delegate.newFileSystem(uri, env)
    def getFileSystem(uri: URI): FileSystem                                = delegate.getFileSystem(uri)
    def getPath(uri: URI): Path                                            = delegate.getPath(uri)

    def newByteChannel(
      path: Path,
      options: java.util.Set[? <: OpenOption],
      attrs: FileAttribute[?]*
    ): SeekableByteChannel =
      contentReads.incrementAndGet()
      delegate.newByteChannel(unwrap(path), options, attrs*)

    def newDirectoryStream(dir: Path, filter: DirectoryStream.Filter[? >: Path]): DirectoryStream[Path] =
      delegate.newDirectoryStream(unwrap(dir), filter)

    def createDirectory(dir: Path, attrs: FileAttribute[?]*): Unit = delegate.createDirectory(unwrap(dir), attrs*)

    def delete(path: Path): Unit = delegate.delete(unwrap(path))

    def copy(source: Path, target: Path, options: CopyOption*): Unit =
      contentReads.incrementAndGet()
      delegate.copy(unwrap(source), unwrap(target), options*)

    def move(source: Path, target: Path, options: CopyOption*): Unit =
      delegate.move(unwrap(source), unwrap(target), options*)

    def isSameFile(path: Path, path2: Path): Boolean = delegate.isSameFile(unwrap(path), unwrap(path2))

    def isHidden(path: Path): Boolean = delegate.isHidden(unwrap(path))

    def getFileStore(path: Path): FileStore = delegate.getFileStore(unwrap(path))

    def checkAccess(path: Path, modes: AccessMode*): Unit = delegate.checkAccess(unwrap(path), modes*)

    def getFileAttributeView[V <: java.nio.file.attribute.FileAttributeView](
      path: Path,
      `type`: Class[V],
      options: LinkOption*
    ): V = delegate.getFileAttributeView(unwrap(path), `type`, options*)

    def readAttributes[A <: BasicFileAttributes](path: Path, `type`: Class[A], options: LinkOption*): A =
      delegate.readAttributes(unwrap(path), `type`, options*)

    def readAttributes(path: Path, attributes: String, options: LinkOption*): java.util.Map[String, AnyRef] =
      delegate.readAttributes(unwrap(path), attributes, options*)

    def setAttribute(path: Path, attribute: String, value: AnyRef, options: LinkOption*): Unit =
      delegate.setAttribute(unwrap(path), attribute, value, options*)

  final private class ContentReadCountingFileSystem(delegate: FileSystem, countingProvider: ContentReadCountingProvider)
      extends FileSystem:
    def provider(): FileSystemProvider                              = countingProvider
    def close(): Unit                                               = delegate.close()
    def isOpen: Boolean                                             = delegate.isOpen
    def isReadOnly: Boolean                                         = delegate.isReadOnly
    def getSeparator: String                                        = delegate.getSeparator
    def getRootDirectories: java.lang.Iterable[Path]                = delegate.getRootDirectories
    def getFileStores: java.lang.Iterable[FileStore]                = delegate.getFileStores
    def supportedFileAttributeViews(): java.util.Set[String]        = delegate.supportedFileAttributeViews()
    def getPath(first: String, more: String*): Path                 = delegate.getPath(first, more*)
    def getPathMatcher(syntaxAndPattern: String): PathMatcher       = delegate.getPathMatcher(syntaxAndPattern)
    def getUserPrincipalLookupService(): UserPrincipalLookupService = delegate.getUserPrincipalLookupService()
    def newWatchService(): WatchService                             = delegate.newWatchService()

  final private class CountingPathHandler(val real: Path, fileSystem: ContentReadCountingFileSystem)
      extends InvocationHandler:

    def invoke(proxy: AnyRef, method: Method, args: Array[AnyRef]): AnyRef =
      if method.getName == "getFileSystem" && method.getParameterCount == 0 then fileSystem
      else method.invoke(real, Option(args).getOrElse(Array.empty[AnyRef])*)

  private def wrapCountingPath(real: Path, fileSystem: ContentReadCountingFileSystem): Path =
    Proxy
      .newProxyInstance(classOf[Path].getClassLoader, Array(classOf[Path]), CountingPathHandler(real, fileSystem))
      .asInstanceOf[Path]

  final private class RecordingFileSystem(
      failWrite: Boolean = false,
      rejectAtomicMove: Boolean = false
  ) extends AtomicFileSystem:
    private val replacementMoveUsed = AtomicBoolean(false)

    def usedReplacementMove: Boolean = replacementMoveUsed.get

    override def createDirectories(path: Path): Path = Files.createDirectories(path)

    override def createTempFile(directory: Path, prefix: String, suffix: String): Path =
      Files.createTempFile(directory, prefix, suffix)

    override def exists(path: Path): Boolean = Files.exists(path)

    override def copyAttributes(source: Path, target: Path): Path =
      Files.copy(source, target, StandardCopyOption.COPY_ATTRIBUTES, StandardCopyOption.REPLACE_EXISTING)

    override def write(path: Path, bytes: Array[Byte]): Path =
      if failWrite then throw IOException("disk full")
      else Files.write(path, bytes)

    override def moveAtomically(source: Path, target: Path): Path =
      if rejectAtomicMove then throw AtomicMoveNotSupportedException(source.toString, target.toString, "unsupported")
      else Files.move(source, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)

    override def moveReplacing(source: Path, target: Path): Path =
      replacementMoveUsed.set(true)
      Files.move(source, target, StandardCopyOption.REPLACE_EXISTING)

    override def deleteIfExists(path: Path): Boolean = Files.deleteIfExists(path)

  "AtomicFileWriter" should "replace a completed sibling file atomically when supported" in {
    val directory = Files.createTempDirectory("serenity-atomic-write")
    val target    = directory.resolve("document.txt")
    Files.writeString(target, "before")

    try
      AtomicFileWriter.writeString(target, "after").unsafeRunSync()

      Files.readString(target) shouldBe "after"
      Files.list(directory).toArray.map(_.asInstanceOf[Path].getFileName.toString) shouldBe Array("document.txt")
    finally Files.walk(directory).sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists)
  }

  it should "fall back to a replacement move when atomic moves are unsupported" in {
    val directory  = Files.createTempDirectory("serenity-atomic-fallback")
    val target     = directory.resolve("document.txt")
    val fileSystem = RecordingFileSystem(rejectAtomicMove = true)

    try
      AtomicFileWriter.writeString(target, "after", fileSystem).unsafeRunSync()

      fileSystem.usedReplacementMove shouldBe true
      Files.readString(target) shouldBe "after"
    finally Files.walk(directory).sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists)
  }

  it should "preserve existing POSIX permissions when replacing a target" in {
    val directory = Files.createTempDirectory("serenity-atomic-permissions")
    val target    = directory.resolve("executable.sh")
    val permissions = Set(
      PosixFilePermission.OWNER_READ,
      PosixFilePermission.OWNER_WRITE,
      PosixFilePermission.OWNER_EXECUTE,
      PosixFilePermission.GROUP_READ,
      PosixFilePermission.GROUP_EXECUTE
    )

    try
      assume(
        Files.getFileStore(directory).supportsFileAttributeView(classOf[PosixFileAttributeView]),
        "POSIX file attributes are unavailable"
      )
      Files.writeString(target, "before")
      Files.setPosixFilePermissions(target, permissions.asJava)
      AtomicFileWriter.writeString(target, "after").unsafeRunSync()

      Files.readString(target) shouldBe "after"
      Files.getPosixFilePermissions(target).asScala.toSet shouldBe permissions
    finally Files.walk(directory).sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists)
  }

  // #1444: DocumentStorage's revision-checked save path reads the target file once to hash it, then
  // AtomicFileWriter's own copyAttributes re-read the whole file again purely to seed the temp file's attributes
  // before it was immediately overwritten by the real content. copyAttributes must carry over permissions without
  // duplicating the file's content.
  it should "copy only attributes, not file content, so the target file is read once per save" in {
    val directory = Files.createTempDirectory("serenity-copy-attrs-only")
    val source    = directory.resolve("source.txt")
    val target    = directory.resolve("target.txt")

    try
      assume(
        Files.getFileStore(directory).supportsFileAttributeView(classOf[PosixFileAttributeView]),
        "POSIX file attributes are unavailable"
      )
      val permissions = Set(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE)
      Files.writeString(source, "original content that must not leak into target")
      Files.setPosixFilePermissions(source, permissions.asJava)
      Files.writeString(target, "") // the real temp file is empty at the point AtomicFileWriter seeds its attributes

      AtomicFileWriter.defaultFileSystem.copyAttributes(source, target)

      Files.getPosixFilePermissions(target).asScala.toSet shouldBe permissions
      Files.readString(target) shouldBe "" // content copy would have overwritten this with the source's content
    finally Files.walk(directory).sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists)
  }

  // The end-state assertions above can't distinguish "content never read" from "content read internally but
  // discarded" -- e.g. a regression back to `Files.copy(..., COPY_ATTRIBUTES)` on an empty target would still
  // pass them. Route source/target through a counting FileSystemProvider proxy that increments only on
  // content-touching operations (`copy`, `newByteChannel`) and prove that count stays at zero.
  it should "perform zero content-reading filesystem operations while copying attributes" in {
    val directory = Files.createTempDirectory("serenity-copy-attrs-no-content-read")
    val source    = directory.resolve("source.txt")
    val target    = directory.resolve("target.txt")

    try
      assume(
        Files.getFileStore(directory).supportsFileAttributeView(classOf[PosixFileAttributeView]),
        "POSIX file attributes are unavailable"
      )
      val permissions = Set(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE)
      Files.writeString(source, "original content that must never be read by copyAttributes")
      Files.setPosixFilePermissions(source, permissions.asJava)
      Files.writeString(target, "")

      val provider       = ContentReadCountingProvider(source.getFileSystem.provider())
      val countingFs     = ContentReadCountingFileSystem(source.getFileSystem, provider)
      val countingSource = wrapCountingPath(source, countingFs)
      val countingTarget = wrapCountingPath(target, countingFs)

      AtomicFileWriter.defaultFileSystem.copyAttributes(countingSource, countingTarget)

      provider.contentReads.get shouldBe 0
      Files.getPosixFilePermissions(target).asScala.toSet shouldBe permissions
    finally Files.walk(directory).sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists)
  }

  // #1502 review: on a non-POSIX filesystem (NTFS/FAT), `getPosixFilePermissions` throws
  // `UnsupportedOperationException`. copyAttributes must not silently swallow that -- it should fall back to
  // carrying over DOS attributes (read-only/hidden/archive/system), which the old `Files.copy(..., COPY_ATTRIBUTES)`
  // preserved on Windows.
  it should "copy DOS attribute flags across when the source view exposes them" in {
    val sourceAttributes = FakeDosFileAttributes(readOnly = true, hidden = true, archive = false, system = true)
    val sourceView       = FakeDosFileAttributeView(sourceAttributes)
    val targetView       = FakeDosFileAttributeView(FakeDosFileAttributes(false, false, false, false))

    AtomicFileWriter.copyDosAttributes(sourceView, targetView)

    targetView.observedFlags shouldBe (Some(true), Some(true), Some(false), Some(true))
  }

  it should "preserve an existing target and clean its temporary file when writing fails" in {
    val directory  = Files.createTempDirectory("serenity-atomic-failure")
    val target     = directory.resolve("document.txt")
    val fileSystem = RecordingFileSystem(failWrite = true)
    Files.writeString(target, "before")

    try
      val result = AtomicFileWriter.writeString(target, "after", fileSystem).attempt.unsafeRunSync()

      result.left.toOption match
        case Some(error: AtomicFileWriteException) =>
          error.path shouldBe target
          error.getCause.getMessage shouldBe "disk full"
        case other => fail(s"expected AtomicFileWriteException, got $other")
      Files.readString(target) shouldBe "before"
      Files.list(directory).toArray.map(_.asInstanceOf[Path].getFileName.toString) shouldBe Array("document.txt")
    finally Files.walk(directory).sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists)
  }
