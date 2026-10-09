package com.serenity.io

import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CheckedFileStampsSpec extends AnyFlatSpec with Matchers:

  private val file = Path.of("/notes/chapter.md")

  private val fineTick = 100_123_456L
  private val coarse   = 100_000_000L

  private val longEnoughToVouch = 5_000_000_000L

  /** A stamp taken `afterModified` nanoseconds after its file was last modified, the way `FileStamp.observe` makes one.
    */
  private def observed(size: Long, modifiedNanos: Long, afterModified: Long): Option[FileStamp.Observed] =
    val stamp = FileStamp(size, modifiedNanos, None)
    Some(FileStamp.Observed(stamp, FileStamp.vouchesForContent(stamp, modifiedNanos + afterModified)))

  private def stampsOver(onDisk: AtomicReference[Option[FileStamp.Observed]]): IO[CheckedFileStamps] =
    CheckedFileStamps.create(_ => IO(onDisk.get))

  "CheckedFileStamps" should "claim a file once until its size or modification time moves" in {
    val onDisk = new AtomicReference(observed(10L, fineTick, longEnoughToVouch))
    val program = for
      stamps <- stampsOver(onDisk)
      first  <- stamps.claimIfChanged(file)
      again  <- stamps.claimIfChanged(file)
      _      <- IO(onDisk.set(observed(10L, fineTick + 1L, longEnoughToVouch)))
      moved  <- stamps.claimIfChanged(file)
      _      <- IO(onDisk.set(observed(11L, fineTick + 1L, longEnoughToVouch)))
      grown  <- stamps.claimIfChanged(file)
    yield (first, again, moved, grown)

    program.unsafeRunSync() shouldBe (true, false, true, true)
  }

  it should "check a file again when its stamp was taken too soon after its modification to vouch for it" in {
    val onDisk = new AtomicReference(observed(10L, coarse, 1_000L))
    val program = for
      stamps  <- stampsOver(onDisk)
      first   <- stamps.claimIfChanged(file)
      again   <- stamps.claimIfChanged(file)
      _       <- IO(onDisk.set(observed(10L, coarse, longEnoughToVouch)))
      vouched <- stamps.claimIfChanged(file)
      settled <- stamps.claimIfChanged(file)
    yield (first, again, vouched, settled)

    program.unsafeRunSync() shouldBe (true, true, true, false)
  }

  it should "always claim a file it cannot read, and again once it reappears unchanged" in {
    val onDisk = new AtomicReference(observed(10L, fineTick, longEnoughToVouch))
    val program = for
      stamps  <- stampsOver(onDisk)
      _       <- stamps.claimIfChanged(file)
      _       <- IO(onDisk.set(None))
      missing <- stamps.claimIfChanged(file)
      gone    <- stamps.claimIfChanged(file)
      _       <- IO(onDisk.set(observed(10L, fineTick, longEnoughToVouch)))
      back    <- stamps.claimIfChanged(file)
    yield (missing, gone, back)

    program.unsafeRunSync() shouldBe (true, true, true)
  }
