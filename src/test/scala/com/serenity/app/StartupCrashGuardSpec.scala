package com.serenity.app

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.app.StartupCrashGuard.Decision
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StartupCrashGuardSpec extends AnyFlatSpec with Matchers:

  private def tempMarker(): Path =
    Files.createTempDirectory("serenity-crash-guard").resolve("nested").resolve("startup-in-progress")

  "StartupCrashGuard.unfinishedStarts" should "count no unfinished starts when there is no marker" in {
    StartupCrashGuard.unfinishedStarts(None) shouldBe 0
  }

  it should "read the count the marker holds" in {
    StartupCrashGuard.unfinishedStarts(Some("2")) shouldBe 2
    StartupCrashGuard.unfinishedStarts(Some(" 4\n")) shouldBe 4
  }

  it should "count an unreadable marker as one unfinished start, since its presence alone means one" in {
    StartupCrashGuard.unfinishedStarts(Some("")) shouldBe 1
    StartupCrashGuard.unfinishedStarts(Some("garbage")) shouldBe 1
    StartupCrashGuard.unfinishedStarts(Some("-3")) shouldBe 1
  }

  "StartupCrashGuard.decide" should "proceed while the unfinished starts are below the threshold" in
    (0 until StartupCrashGuard.StartThreshold).foreach { unfinished =>
      StartupCrashGuard.decide(unfinished, safeModeRequested = false) shouldBe Decision.Proceed
    }

  it should "start in safe mode once the threshold of consecutive unfinished starts is reached" in {
    val threshold = StartupCrashGuard.StartThreshold
    StartupCrashGuard.decide(threshold, safeModeRequested = false) shouldBe Decision.StartSafeMode(threshold)
    StartupCrashGuard.decide(threshold + 5, safeModeRequested = false) shouldBe Decision.StartSafeMode(threshold + 5)
  }

  it should "leave a launch that already asked for safe mode alone" in {
    StartupCrashGuard.decide(StartupCrashGuard.StartThreshold + 1, safeModeRequested = true) shouldBe Decision.Proceed
  }

  it should "honour a custom threshold" in {
    StartupCrashGuard.decide(1, safeModeRequested = false, threshold = 1) shouldBe Decision.StartSafeMode(1)
  }

  "StartupCrashGuard.recordStartAttempt" should "return the previous count and leave a marker one higher" in {
    val marker = tempMarker()
    val counts = (for
      first  <- StartupCrashGuard.recordStartAttempt(marker)
      second <- StartupCrashGuard.recordStartAttempt(marker)
      third  <- StartupCrashGuard.recordStartAttempt(marker)
    yield List(first, second, third)).unsafeRunSync()

    counts shouldBe List(0, 1, 2)
    Files.readString(marker, StandardCharsets.UTF_8).trim shouldBe "3"
  }

  "StartupCrashGuard.markStarted" should "clear the marker so the next launch starts counting from zero" in {
    val marker = tempMarker()
    val next = (StartupCrashGuard.recordStartAttempt(marker) >>
      StartupCrashGuard.recordStartAttempt(marker) >>
      StartupCrashGuard.markStarted(marker) >>
      StartupCrashGuard.recordStartAttempt(marker)).unsafeRunSync()

    next shouldBe 0
  }

  it should "succeed when there is no marker to clear" in {
    noException should be thrownBy StartupCrashGuard.markStarted(tempMarker()).unsafeRunSync()
  }

  "A crash loop" should "start in safe mode once the threshold of starts in a row never showed a frame" in {
    val marker = tempMarker()
    val launch: IO[Decision] =
      StartupCrashGuard.recordStartAttempt(marker).map(StartupCrashGuard.decide(_, safeModeRequested = false))
    val crashedLaunches = List.fill(StartupCrashGuard.StartThreshold)(launch).sequence.unsafeRunSync()

    crashedLaunches.distinct shouldBe List(Decision.Proceed)
    launch.unsafeRunSync() shouldBe Decision.StartSafeMode(StartupCrashGuard.StartThreshold)
  }

  "StartupCrashGuard.peekUnfinishedStarts" should "read the count without counting the launch that reads it" in {
    val marker = tempMarker()
    val peeks = (StartupCrashGuard.recordStartAttempt(marker) >>
      StartupCrashGuard.recordStartAttempt(marker) >>
      StartupCrashGuard.peekUnfinishedStarts(marker).product(StartupCrashGuard.peekUnfinishedStarts(marker)))
      .unsafeRunSync()

    peeks shouldBe (2, 2)
  }

  it should "find no unfinished starts when there is no marker" in {
    StartupCrashGuard.peekUnfinishedStarts(tempMarker()).unsafeRunSync() shouldBe 0
  }

  "A safe-mode launch" should "not count itself, and a clean one clears the loop it was started for" in {
    val marker  = tempMarker()
    val crashed = List.fill(StartupCrashGuard.StartThreshold)(StartupCrashGuard.recordStartAttempt(marker)).sequence
    val afterSafeStart = (crashed >> StartupCrashGuard.peekUnfinishedStarts(marker) <*
      StartupCrashGuard.markStarted(marker)).unsafeRunSync()

    afterSafeStart shouldBe StartupCrashGuard.StartThreshold
    StartupCrashGuard.peekUnfinishedStarts(marker).unsafeRunSync() shouldBe 0
  }
