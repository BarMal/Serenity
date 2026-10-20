package com.serenity.io

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.TestTemp
import com.serenity.rope.Balance
import com.serenity.state.models.BufferId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1873: a revision carries the stat it was taken at, so confirming it costs a stat and a save hashes once. */
class DocumentRevisionStampSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  final private case class Counted(provider: DocumentStorageProvider, read: Ref[IO, Long], hashed: Ref[IO, Long]):
    def reset(): Unit     = (read.set(0L) >> hashed.set(0L)).unsafeRunSync()
    def bytesRead: Long   = read.get.unsafeRunSync()
    def bytesHashed: Long = hashed.get.unsafeRunSync()

  private def counted(clock: IO[FiniteDuration] = SettledClock.aMinuteAhead): Counted =
    val read   = Ref.unsafe[IO, Long](0L)
    val hashed = Ref.unsafe[IO, Long](0L)
    Counted(
      LocalDocumentStorageProvider(StorageIoProbe(n => read.update(_ + n), n => hashed.update(_ + n)), clock),
      read,
      hashed
    )

  private def bytes(text: String): Array[Byte] = text.getBytes(StandardCharsets.UTF_8)

  private def largeFile(megabytes: Int): Path =
    val path = TestTemp.file("document-revision-stamp", ".txt")
    Files.writeString(path, "0123456789abcde\n" * (megabytes * 65536))

  "Saving over a stamped revision" should "hash only the new content and read nothing back (#1873)" in {
    val path     = largeFile(4)
    val storage  = counted()
    val location = StorageLocation.Local(path)
    val opened   = storage.provider.open(location).unsafeRunSync().toOption.flatMap(_.revision)
    val update   = bytes("replaced")
    storage.reset()

    val saved = storage.provider.save(location, update, opened).unsafeRunSync()

    saved.isRight shouldBe true
    storage.bytesRead shouldBe 0L
    storage.bytesHashed shouldBe update.length.toLong
  }

  it should "read and compare the content once the stat has moved, accepting identical bytes" in {
    val path     = largeFile(1)
    val storage  = counted()
    val location = StorageLocation.Local(path)
    val opened   = storage.provider.open(location).unsafeRunSync().toOption.flatMap(_.revision)
    Files.setLastModifiedTime(path, FileTime.fromMillis(Files.getLastModifiedTime(path).toMillis + 5000L))
    val sizeBefore = Files.size(path)
    storage.reset()

    val saved = storage.provider.save(location, bytes("replaced"), opened).unsafeRunSync()

    saved.isRight shouldBe true
    storage.bytesRead shouldBe sizeBefore
  }

  it should "still report a conflict for a real external edit" in {
    val path     = largeFile(1)
    val storage  = counted()
    val location = StorageLocation.Local(path)
    val opened   = storage.provider.open(location).unsafeRunSync().toOption.flatMap(_.revision)
    Files.writeString(path, "edited elsewhere")

    storage.provider.save(location, bytes("mine"), opened).unsafeRunSync() shouldBe
      Left(DocumentStorageError.Conflict(location))
  }

  "revisionSince" should "confirm an unchanged file without reading it, and read one that changed (#1873)" in {
    val path    = largeFile(1)
    val storage = counted()
    val manager = new FileManager(storage.provider)
    val known   = manager.loadFile(path, BufferId(1)).unsafeRunSync().document.revision
    storage.reset()

    manager.revisionSince(path, known).unsafeRunSync() shouldBe known
    storage.bytesRead shouldBe 0L

    Files.writeString(path, "edited elsewhere")
    val changed = manager.revisionSince(path, known).unsafeRunSync()

    changed.exists(revision => known.exists(revision.sameContent)) shouldBe false
    storage.bytesRead shouldBe "edited elsewhere".length.toLong
  }

  it should "confirm a file this process just saved without reading it" in {
    val path    = largeFile(1)
    val storage = counted()
    val manager = new FileManager(storage.provider)
    val opened  = manager.loadFile(path, BufferId(1)).unsafeRunSync()
    val saved   = manager.saveBuffer(opened).unsafeRunSync()
    storage.reset()

    manager.revisionSince(path, saved.document.revision).unsafeRunSync() shouldBe saved.document.revision
    storage.bytesRead shouldBe 0L
    storage.bytesHashed shouldBe 0L
  }

  it should "not keep a stamp for a file whose mtime is not comfortably before the observation" in {
    val path     = largeFile(1)
    val storage  = counted(IO.realTime.map(_ - 1.minute))
    val location = StorageLocation.Local(path)

    val saved = storage.provider.save(location, bytes("replaced"), None).unsafeRunSync()

    saved.toOption.flatMap(_.revision).map(_.stamp) shouldBe Some(None)
  }

  "FileStamp.vouchesForContent" should "distrust a young stamp from a coarse-timestamp filesystem" in {
    val second = 1_000_000_000L
    val coarse = FileStamp(size = 3L, modifiedNanos = 100 * second, fileKey = None)
    val fine   = coarse.copy(modifiedNanos = 100 * second + 123_456L)

    FileStamp.vouchesForContent(coarse, observedAtNanos = 100 * second + 500_000_000L) shouldBe false
    FileStamp.vouchesForContent(coarse, observedAtNanos = 103 * second) shouldBe true
    FileStamp.vouchesForContent(fine, observedAtNanos = 100 * second + 1_000_000_000L) shouldBe true
  }

  it should "distrust a young unrounded stamp from a kernel-tick filesystem, and trust it once comfortably old" in {
    val mtime  = 100_000_000_000L + 123_457L
    val stamp  = FileStamp(size = 3L, modifiedNanos = mtime, fileKey = None)
    val tenMs  = 10_000_000L
    val margin = 20_000_000L

    FileStamp.vouchesForContent(stamp, observedAtNanos = mtime + 1_000L) shouldBe false
    FileStamp.vouchesForContent(stamp, observedAtNanos = mtime + tenMs) shouldBe false
    FileStamp.vouchesForContent(stamp, observedAtNanos = mtime + margin - 1L) shouldBe false
    FileStamp.vouchesForContent(stamp, observedAtNanos = mtime - 1L) shouldBe false
    FileStamp.vouchesForContent(stamp, observedAtNanos = mtime + margin) shouldBe true
    FileStamp.vouchesForContent(stamp, observedAtNanos = mtime + 5_000_000_000L) shouldBe true
  }

  it should "never vouch for a same-size rewrite inside one tick, at any observation time in that tick" in {
    val second    = 1_000_000_000L
    val tickStart = 50 * second
    // A write inside the tick [tickStart, tickStart + tick) leaves the stamp unchanged, so the stamp must not vouch
    // for any observation made before the tick ended.
    for
      tick <- List(1L * second, 2L * second, 1_000_000L)
      stamp = FileStamp(size = 3L, modifiedNanos = tickStart, fileKey = None)
      offset <- 0L until tick by (tick / 20L)
    do FileStamp.vouchesForContent(stamp, tickStart + offset) shouldBe false
  }

  "FileStamp.observe" should "read the clock before it stats, so a write between the two cannot be vouched for" in {
    val second  = 1_000_000_000L
    val path    = TestTemp.file("filestamp", ".txt")
    val settled = FileTime.from(100L * second, TimeUnit.NANOSECONDS)
    Files.write(path, "abc".getBytes(StandardCharsets.UTF_8))
    // The clock lands the file's mtime on a coarse tick: only a stat taken after the clock sees it.
    def clockAt(nanos: Long) =
      IO.blocking(Files.setLastModifiedTime(path, settled)).as(FiniteDuration(nanos, TimeUnit.NANOSECONDS))

    try
      val late  = FileStamp.observe(path, clockAt(103L * second)).unsafeRunSync()
      val early = FileStamp.observe(path, clockAt(101L * second + 900_000_000L)).unsafeRunSync()
      late.map(_.stamp.modifiedNanos) shouldBe Some(100L * second)
      late.map(_.vouches) shouldBe Some(true)
      early.map(_.vouches) shouldBe Some(false)
    finally Files.deleteIfExists(path)
  }
