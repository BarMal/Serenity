package com.serenity

import java.io.IOException
import java.nio.file.attribute.{PosixFileAttributeView, PosixFilePermission}
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import java.util.concurrent.atomic.AtomicBoolean

import scala.jdk.CollectionConverters.*

import cats.effect.unsafe.implicits.global
import com.serenity.io.{AtomicFileSystem, AtomicFileWriteException, AtomicFileWriter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AtomicFileWriterSpec extends AnyFlatSpec with Matchers:

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
