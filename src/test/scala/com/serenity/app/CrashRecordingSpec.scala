package com.serenity.app

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.diagnostics.{CrashRecord, PreviousRun, RuntimeIdentity}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CrashRecordingSpec extends AnyFlatSpec with Matchers:

  private val identity = RuntimeIdentity.of("1.2.3", "abc1234", _ => None)
  private val at       = Instant.parse("2026-10-06T08:30:00Z")

  private def record(): CrashRecord = CrashRecord(TestTemp.directory("serenity-crash-recording").resolve("logs"))

  "CrashReporter.recordingTo" should "log the crash and leave a crash file for the next launch" in {
    val store  = record()
    val logged = AtomicReference[Option[(String, Throwable)]](None)
    val error  = RuntimeException("edt failure")

    val recorder = CrashReporter.recordingTo(store, identity, () => at)((message, throwable) =>
      logged.set(Some((message, throwable)))
    )
    recorder("[RUNTIME] Uncaught exception on thread AWT-EventQueue-0", error)

    logged.get().map(_._1) shouldBe Some("[RUNTIME] Uncaught exception on thread AWT-EventQueue-0")
    store.previousRun(identity, at).unsafeRunSync() match
      case PreviousRun.Abnormal(report) =>
        report should include("edt failure")
        report should include("AWT-EventQueue-0")
      case PreviousRun.Clean => fail("an uncaught exception must leave a crash file")
  }

  it should "still log when the crash file cannot be written" in {
    val blocker = TestTemp.file("serenity-not-a-directory", ".txt")
    val logged  = AtomicReference[Option[String]](None)

    val recorder = CrashReporter.recordingTo(CrashRecord(blocker.resolve("logs")), identity, () => at)((message, _) =>
      logged.set(Some(message))
    )
    recorder("crash", RuntimeException("x"))

    logged.get() shouldBe Some("crash")
  }

  "StartupRecovery.Plan.crashRecorder" should "do nothing by default" in {
    StartupRecovery.Plan.normal.crashRecorder("crash", RuntimeException("x")).unsafeRunSync() shouldBe ()
  }
