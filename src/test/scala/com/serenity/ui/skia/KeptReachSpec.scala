package com.serenity.ui.skia

import com.serenity.ui.layout.PixelRect
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class KeptReachSpec extends AnyFlatSpec with Matchers:

  private val surface = PixelRect(0, 0, 640, 600)
  private val kept    = KeptReach(List(PixelRect(0, 100, 600, 40)))

  private def pieces(reach: PixelRect): Set[PixelRect] =
    kept.uncoveredWithin(reach, surface, () => ()).toSet

  "The pieces a draw may paint in" should "be what the reach has left once the kept rows are cut out" in {
    pieces(PixelRect(10, 90, 100, 70)) shouldBe Set(
      PixelRect(10, 90, 100, 10),
      PixelRect(10, 140, 100, 20)
    )
  }

  it should "leave out a piece that lies wholly off the surface, as it has no pixels to paint" in {
    pieces(PixelRect(-15, 90, 640, 70)) should not contain PixelRect(-15, 100, 15, 40)
    pieces(PixelRect(-15, 90, 640, 70)) shouldBe Set(
      PixelRect(0, 90, 625, 10),
      PixelRect(0, 140, 625, 20),
      PixelRect(600, 100, 25, 40)
    )
  }

  it should "trim a piece that reaches past the surface to the part on it" in {
    pieces(PixelRect(560, 90, 120, 70)) shouldBe Set(
      PixelRect(560, 90, 80, 10),
      PixelRect(560, 140, 80, 20),
      PixelRect(600, 100, 40, 40)
    )
  }

  it should "be worked out once for the frame, however many draws take their part of it" in {
    val sweeps = java.util.concurrent.atomic.AtomicInteger(0)
    val fresh  = KeptReach(List(PixelRect(0, 100, 600, 40)))
    List(PixelRect(0, 90, 50, 70), PixelRect(300, 400, 30, 20), PixelRect(10, 90, 100, 70)).foreach(reach =>
      fresh.uncoveredWithin(
        reach,
        surface,
        () =>
          val _ = sweeps.incrementAndGet()
      )
    )
    sweeps.get shouldBe 1
  }
