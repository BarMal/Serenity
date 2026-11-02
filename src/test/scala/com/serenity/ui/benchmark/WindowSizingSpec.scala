package com.serenity.ui.benchmark

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What the resize scenario does about a window manager that decides the window's size (#2480). The clock and the pause
  * are handed in, so no spec waits.
  */
class WindowSizingSpec extends AnyFlatSpec with Matchers:

  private val asked = WindowPx(1200, 750)

  /** A clock that moves only when the code under test pauses. */
  final private class Clock:
    var now: Long    = 0L
    val pauses       = scala.collection.mutable.ArrayBuffer.empty[Long]
    def read(): Long = now
    def pause(millis: Long): Unit =
      pauses += millis; now += millis

  "WindowSizing.awaitApplied" should "return at once when the window already has the size asked for" in {
    val clock = new Clock

    val outcome =
      WindowSizing.awaitApplied(() => asked, asked, timeoutMillis = 250, pollMillis = 5, clock.read, clock.pause)

    outcome shouldBe WindowSizing.Outcome.Applied(asked)
    clock.pauses shouldBe empty
  }

  it should "wait until the window has the size, and say it was applied" in {
    val clock = new Clock
    val sizes = Iterator(WindowPx(1600, 1000), WindowPx(1600, 1000), asked)

    val outcome =
      WindowSizing.awaitApplied(() => sizes.next(), asked, timeoutMillis = 250, pollMillis = 5, clock.read, clock.pause)

    outcome shouldBe WindowSizing.Outcome.Applied(asked)
    clock.pauses.toList shouldBe List(5L, 5L)
  }

  it should "say the window was kept at its size once the time is up, with the size it had" in {
    val clock = new Clock
    val kept  = WindowPx(1920, 1060)

    val outcome =
      WindowSizing.awaitApplied(() => kept, asked, timeoutMillis = 250, pollMillis = 5, clock.read, clock.pause)

    outcome shouldBe WindowSizing.Outcome.Kept(kept)
    clock.now should be >= 250L
    clock.now should be < 260L
  }

  "WindowSizing.keptReason" should "name both sizes" in {
    WindowSizing.keptReason(asked, WindowPx(1920, 1060)) shouldBe
      "the window manager kept the window at 1920x1060 when it was asked for 1200x750"
  }
