package com.serenity.ui.layout

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SettledProbeSpec extends AnyFlatSpec with Matchers:

  private def settle[A](readings: A*): (A, Int) =
    val remaining = readings.iterator
    val taken     = AtomicInteger(0)
    val settled = SettledProbe {
      val _ = taken.incrementAndGet()
      remaining.nextOption().getOrElse(readings.last)
    }
    (settled, taken.get)

  "a settled probe" should "return a reading that repeats, not the first one a concurrent first use perturbed" in {
    settle(false, true, true, true, true)._1 shouldBe true
    settle(-0.253f, -0.772f, -0.772f, -0.772f)._1 shouldBe -0.772f
  }

  it should "not accept two agreeing readings split by a disagreeing one" in {
    settle(true, false, true, false, false, false)._1 shouldBe false
  }

  it should "read a font that never disturbs it only as often as it needs to agree" in {
    settle(7, 7, 7, 7)._2 shouldBe SettledProbe.AgreeingReadings
  }

  it should "give up on a probe that never repeats, returning its latest reading" in {
    val (value, taken) = settle((1 to SettledProbe.MaxReadings + 10)*)
    taken shouldBe SettledProbe.MaxReadings
    value shouldBe SettledProbe.MaxReadings
  }
