package com.serenity.diagnostics

import java.nio.file.{Files, Path}
import java.time.Instant

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CrashRecordSpec extends AnyFlatSpec with Matchers:

  private val identity = RuntimeIdentity.of("1.2.3", "abc1234", _ => None)
  private val at       = Instant.parse("2026-10-06T08:30:00Z")

  private def record(): CrashRecord = CrashRecord(Files.createTempDirectory("serenity-crash-record").resolve("logs"))

  "PreviousRun.decide" should "be clean with neither a running marker nor a crash file" in {
    PreviousRun.decide(None, None, identity, at, Path.of("logs")) shouldBe PreviousRun.Clean
  }

  it should "report the crash file's text when one was left" in {
    PreviousRun.decide(Some("Serenity 1.0"), Some("the report"), identity, at, Path.of("logs")) shouldBe
      PreviousRun.Abnormal("the report")
  }

  it should "build a report of its own when only the running marker was left" in {
    PreviousRun.decide(Some("Serenity 1.0"), None, identity, at, Path.of("logs")) match
      case PreviousRun.Abnormal(report) => report should include("Serenity 1.0")
      case PreviousRun.Clean            => fail("a left-behind running marker is an abnormal exit")
  }

  "CrashRecord" should "find the previous run clean after a clean exit" in {
    val store = record()
    val run = for
      _        <- store.markRunning(identity, at)
      _        <- store.markCleanExit
      previous <- store.previousRun(identity, at)
    yield previous

    run.unsafeRunSync() shouldBe PreviousRun.Clean
  }

  it should "find the previous run clean when nothing was ever written" in {
    record().previousRun(identity, at).unsafeRunSync() shouldBe PreviousRun.Clean
  }

  it should "find the previous run abnormal when the marker was never removed" in {
    val store = record()
    val run = for
      _        <- store.markRunning(identity, at)
      previous <- store.previousRun(identity, at)
    yield previous

    run.unsafeRunSync() match
      case PreviousRun.Abnormal(report) => report should include(identity.summary)
      case PreviousRun.Clean            => fail("the run never marked a clean exit")
  }

  it should "find the previous run abnormal when a crash was recorded, even after a clean shutdown" in {
    val store = record()
    val run = for
      _        <- store.markRunning(identity, at)
      written  <- store.recordCrash("report text")
      _        <- store.markCleanExit
      previous <- store.previousRun(identity, at)
    yield (written, previous)

    val (written, previous) = run.unsafeRunSync()
    written shouldBe Some(store.crashFile)
    previous shouldBe PreviousRun.Abnormal("report text")
  }

  it should "report a crash once: acknowledging keeps the file under another name and clears the marker" in {
    val store = record()
    val run = for
      _      <- store.markRunning(identity, at)
      _      <- store.recordCrash("report text")
      _      <- store.acknowledge
      second <- store.previousRun(identity, at)
    yield second

    run.unsafeRunSync() shouldBe PreviousRun.Clean
    Files.readString(store.acknowledgedCrashFile) shouldBe "report text"
    Files.exists(store.crashFile) shouldBe false
  }

  it should "write a crash file synchronously for a handler that cannot run IO" in {
    val store = record()

    store.writeCrash("from a dying thread") shouldBe Some(store.crashFile)

    Files.readString(store.crashFile) shouldBe "from a dying thread"
  }

  it should "never fail when the directory cannot be written" in {
    val blocker = Files.createTempFile("serenity-not-a-directory", ".txt")
    val store   = CrashRecord(blocker.resolve("logs"))

    store.markRunning(identity, at).unsafeRunSync()
    store.recordCrash("report").unsafeRunSync() shouldBe None
    store.previousRun(identity, at).unsafeRunSync() shouldBe PreviousRun.Clean
    store.acknowledge.unsafeRunSync()
    store.markCleanExit.unsafeRunSync()
  }
