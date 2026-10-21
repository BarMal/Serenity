package com.serenity

import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SkiaColdWarmDebugSpec extends AnyFlatSpec with Matchers with PersistedFrameFixture:
  override protected def frames: FrameBackend = SkiaFrames

  private def report(label: String, a: FramePixels, b: FramePixels): Unit =
    val count = a.rgbPixels.zip(b.rgbPixels).count((x, y) => (x & 0xffffff) != (y & 0xffffff))
    println(s"DEBUGEXP $label differing=$count")

  "Cold and warm Skia frames" should "be compared in isolation" in {
    val first  = stateOf(proseLines.mkString("\n"), CursorPosition(4, 5))
    val second = typedAt(first, proseLines, line = 4, text = "x")
    report("cold(first) vs cold(first)", cold(first), cold(first))
    report("cold(second) vs cold(second)", cold(second), cold(second))
    val one = Session()
    val _   = one.settle(first)
    val (settled, _) = one.draw(first, Damage.Everything)
    report("settled session full frame vs cold(first)", settled, cold(first))
    val two = Session()
    val (fresh, _) = two.draw(first, Damage.Everything)
    report("fresh session first frame vs cold(first)", fresh, cold(first))
    val three = Session()
    val (typed, _) = three.draw(second, Damage.Everything)
    report("fresh session full frame of second vs cold(second)", typed, cold(second))
    val four = Session()
    val _    = four.settle(first)
    val (incremental, _) = four.draw(second, DamageProducer.forTransition(first, second))
    report("incremental second vs cold(second)", incremental, cold(second))
    report("incremental second vs fresh session full second", incremental, typed)
    succeed
  }
