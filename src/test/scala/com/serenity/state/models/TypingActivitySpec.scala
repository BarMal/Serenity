package com.serenity.state.models

import scala.concurrent.duration.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypingActivitySpec extends AnyFlatSpec with Matchers:

  private val typedAt = 1.second.toNanos

  "TypingActivity" should "stay active until its quiet window has passed since the last keystroke" in {
    val typing = TypingActivity.idle.observed(typedAt)

    typing.isActive shouldBe true
    typing.advance(typedAt + TypingActivity.QuietWindow.toNanos - 1).isActive shouldBe true
    typing.advance(typedAt + TypingActivity.QuietWindow.toNanos) shouldBe TypingActivity.idle
  }

  it should "restart the window on each keystroke" in {
    val typing = TypingActivity.idle.observed(typedAt).observed(typedAt + 400.millis.toNanos)

    typing.advance(typedAt + 600.millis.toNanos).isActive shouldBe true
    typing.advance(typedAt + 900.millis.toNanos).isActive shouldBe false
  }

  it should "expire after the same wall time whatever the frame rate advancing it" in {
    def expiry(frameInterval: FiniteDuration): FiniteDuration =
      Iterator
        .iterate((TypingActivity.idle.observed(typedAt), Duration.Zero)) {
          case (typing, elapsed) =>
            val next = elapsed + frameInterval
            (typing.advance(typedAt + next.toNanos), next)
        }
        .dropWhile((typing, _) => typing.isActive)
        .map(_._2)
        .next()

    val window = TypingActivity.QuietWindow
    List(8.millis, 16.millis, 40.millis, 100.millis).foreach { frameInterval =>
      withClue(s"frameInterval=$frameInterval ") {
        expiry(frameInterval) should be >= window
        expiry(frameInterval) should be < window + frameInterval
      }
    }
  }
