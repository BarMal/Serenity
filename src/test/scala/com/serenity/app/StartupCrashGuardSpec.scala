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
    (0 until StartupCrashGuard.OfferThreshold).foreach { unfinished =>
      StartupCrashGuard.decide(unfinished, safeModeRequested = false) shouldBe Decision.Proceed
    }

  it should "offer safe mode once the threshold of consecutive unfinished starts is reached" in {
    val threshold = StartupCrashGuard.OfferThreshold
    StartupCrashGuard.decide(threshold, safeModeRequested = false) shouldBe Decision.OfferSafeMode(threshold)
    StartupCrashGuard.decide(threshold + 5, safeModeRequested = false) shouldBe Decision.OfferSafeMode(threshold + 5)
  }

  it should "not offer safe mode to a launch that already asked for it" in {
    StartupCrashGuard.decide(StartupCrashGuard.OfferThreshold + 1, safeModeRequested = true) shouldBe Decision.Proceed
  }

  it should "honour a custom threshold" in {
    StartupCrashGuard.decide(1, safeModeRequested = false, threshold = 1) shouldBe Decision.OfferSafeMode(1)
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

  "A crash loop" should "be offered safe mode once the threshold of starts in a row never showed a frame" in {
    val marker = tempMarker()
    val launch: IO[Decision] =
      StartupCrashGuard.recordStartAttempt(marker).map(StartupCrashGuard.decide(_, safeModeRequested = false))
    val crashedLaunches = List.fill(StartupCrashGuard.OfferThreshold)(launch).sequence.unsafeRunSync()

    crashedLaunches.distinct shouldBe List(Decision.Proceed)
    launch.unsafeRunSync() shouldBe Decision.OfferSafeMode(StartupCrashGuard.OfferThreshold)
  }
