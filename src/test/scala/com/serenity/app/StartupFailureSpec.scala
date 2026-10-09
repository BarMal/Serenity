package com.serenity.app

import java.nio.file.Files
import java.time.Instant

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.diagnostics.{CrashRecord, PreviousRun, RuntimeIdentity}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StartupFailureSpec extends AnyFlatSpec with Matchers:

  private val identity = RuntimeIdentity.of("1.2.3", "abc1234", _ => None)
  private val at       = Instant.parse("2026-10-06T08:30:00Z")
  private val failure  = IllegalStateException("no fonts could be loaded")

  private def record(): CrashRecord = CrashRecord(Files.createTempDirectory("serenity-startup-failure").resolve("logs"))

  final private case class Outcome(notices: List[StartupFailure.Notice], console: List[String])

  private def run(store: CrashRecord, display: StartupFailure.Notice => IO[Unit]): Outcome =
    (for
      console <- Ref.of[IO, List[String]](Nil)
      shown   <- Ref.of[IO, List[StartupFailure.Notice]](Nil)
      _ <- StartupFailure.report(
        failure,
        identity,
        store,
        at,
        notice => shown.update(_ :+ notice) >> display(notice),
        line => console.update(_ :+ line)
      )
      notices <- shown.get
      lines   <- console.get
    yield Outcome(notices, lines)).unsafeRunSync()

  "StartupFailure.report" should "write a crash file naming the build and the error" in {
    val store = record()
    run(store, _ => IO.unit)

    val report = Files.readString(store.acknowledgedCrashFile)
    report should include(identity.summary)
    report should include("no fonts could be loaded")
  }

  it should "show a notice that says where the crash file and logs are" in {
    val store   = record()
    val outcome = run(store, _ => IO.unit)

    outcome.notices.map(_.crashFile) shouldBe List(Some(store.acknowledgedCrashFile))
    val notice = outcome.notices.headOption.map(_.message).getOrElse("")
    notice should include("no fonts could be loaded")
    notice should include(store.acknowledgedCrashFile.toString)
    outcome.notices.map(_.title) shouldBe List("Serenity could not start")
  }

  it should "also print the message, so a terminal launch sees it" in {
    val store   = record()
    val outcome = run(store, _ => IO.unit)

    outcome.console.mkString("\n") should include(store.acknowledgedCrashFile.toString)
  }

  it should "fall back to the console alone when no dialog can be shown" in {
    val store   = record()
    val outcome = run(store, _ => IO.raiseError(RuntimeException("headless")))

    outcome.console should not be empty
  }

  it should "leave the next launch with nothing to report, since the user was told now" in {
    val store = record()
    run(store, _ => IO.unit)

    store.previousRun(identity, at).unsafeRunSync() shouldBe PreviousRun.Clean
  }

  it should "still show the notice when the crash file cannot be written" in {
    val blocker = Files.createTempFile("serenity-not-a-directory", ".txt")
    val outcome = run(CrashRecord(blocker.resolve("logs")), _ => IO.unit)

    outcome.notices.map(_.crashFile) shouldBe List(None)
    outcome.notices.headOption.map(_.message).getOrElse("") should include("no fonts could be loaded")
  }
