package com.serenity.io

import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CheckedFileStampsSpec extends AnyFlatSpec with Matchers:

  private val file = Path.of("/notes/chapter.md")

  private def stampsOver(onDisk: AtomicReference[Option[(Long, Long)]]): IO[CheckedFileStamps] =
    CheckedFileStamps.create(_ => onDisk.get)

  "CheckedFileStamps" should "claim a file once until its size or modification time moves" in {
    val onDisk = new AtomicReference(Option((10L, 100L)))
    val program = for
      stamps <- stampsOver(onDisk)
      first  <- stamps.claimIfChanged(file)
      again  <- stamps.claimIfChanged(file)
      _      <- IO(onDisk.set(Some((10L, 101L))))
      moved  <- stamps.claimIfChanged(file)
      _      <- IO(onDisk.set(Some((11L, 101L))))
      grown  <- stamps.claimIfChanged(file)
    yield (first, again, moved, grown)

    program.unsafeRunSync() shouldBe (true, false, true, true)
  }

  it should "always claim a file it cannot read, and again once it reappears unchanged" in {
    val onDisk = new AtomicReference(Option((10L, 100L)))
    val program = for
      stamps  <- stampsOver(onDisk)
      _       <- stamps.claimIfChanged(file)
      _       <- IO(onDisk.set(None))
      missing <- stamps.claimIfChanged(file)
      gone    <- stamps.claimIfChanged(file)
      _       <- IO(onDisk.set(Some((10L, 100L))))
      back    <- stamps.claimIfChanged(file)
    yield (missing, gone, back)

    program.unsafeRunSync() shouldBe (true, true, true)
  }
