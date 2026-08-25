package com.serenity.ui.renderer

import com.serenity.state.models.{BufferId, Damage}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DamageAccumulatorSpec extends AnyFlatSpec with Matchers:

  private val someDamage      = Damage.BufferRows(BufferId(1), Set(0))
  private val otherDamage     = Damage.BufferRows(BufferId(1), Set(5))
  private val bufferA: AnyRef = new Object
  private val bufferB: AnyRef = new Object

  "DamageAccumulator.accumulateBuffers" should "merge new damage into every identity already being tracked" in {
    val tracked = Map(bufferA -> Damage.Nothing, bufferB -> Damage.Nothing)
    val updated = DamageAccumulator.accumulateBuffers(tracked, someDamage)

    updated shouldBe Map(bufferA -> someDamage, bufferB -> someDamage)
  }

  it should "fold repeated accumulation via Damage's Monoid rather than overwriting" in {
    val tracked = Map(bufferA -> someDamage)
    val updated = DamageAccumulator.accumulateBuffers(tracked, otherDamage)

    updated shouldBe Map(bufferA -> Damage.BufferRows(BufferId(1), Set(0, 5)))
  }

  it should "leave an untracked identity absent, since only observeDraw starts tracking one" in {
    val updated = DamageAccumulator.accumulateBuffers(Map.empty, someDamage)

    updated shouldBe Map.empty
  }

  it should "be a no-op when the new damage is Nothing" in {
    val tracked = Map(bufferA -> someDamage)
    DamageAccumulator.accumulateBuffers(tracked, Damage.Nothing) shouldBe tracked
  }

  "DamageAccumulator.observeBufferDraw" should "report Nothing for a buffer identity never seen before" in {
    val (damage, _) = DamageAccumulator.observeBufferDraw(Map.empty, bufferA)
    damage shouldBe Damage.Nothing
  }

  it should "start tracking a never-seen identity at Nothing, so later accumulation reaches it" in {
    val (_, tracked) = DamageAccumulator.observeBufferDraw(Map.empty, bufferA)
    tracked shouldBe Map(bufferA -> Damage.Nothing)
  }

  it should "report the accumulated damage for a tracked identity and reset only that identity" in {
    val tracked           = Map(bufferA -> someDamage, bufferB -> otherDamage)
    val (damage, updated) = DamageAccumulator.observeBufferDraw(tracked, bufferA)

    damage shouldBe someDamage
    updated shouldBe Map(bufferA -> Damage.Nothing, bufferB -> otherDamage)
  }

  it should "round-trip across several frames the way pooled double-buffering actually alternates" in {
    // Buffer A is drawn on frame 0. Frames 1 and 2 damage row 0 and row 5 respectively while A sits unused (frame 1
    // draws into B instead). By the time A is reused on frame 3, it must carry damage from both intervening frames.
    val afterFrame0          = DamageAccumulator.observeBufferDraw(Map.empty, bufferA)._2
    val afterFrame1Damage    = DamageAccumulator.accumulateBuffers(afterFrame0, someDamage)
    val (_, afterFrame1Draw) = DamageAccumulator.observeBufferDraw(afterFrame1Damage, bufferB)
    val afterFrame2Damage    = DamageAccumulator.accumulateBuffers(afterFrame1Draw, otherDamage)
    val (damageForA, _)      = DamageAccumulator.observeBufferDraw(afterFrame2Damage, bufferA)

    damageForA shouldBe Damage.BufferRows(BufferId(1), Set(0, 5))
  }

  "DamageAccumulator.accumulateScreen" should "fold new damage into the current screen damage" in {
    DamageAccumulator.accumulateScreen(someDamage, otherDamage) shouldBe Damage.BufferRows(BufferId(1), Set(0, 5))
  }

  "DamageAccumulator.observeScreenPublish" should "report the accumulated damage and reset to Nothing" in {
    val (damage, reset) = DamageAccumulator.observeScreenPublish(someDamage)

    damage shouldBe someDamage
    reset shouldBe Damage.Nothing
  }

  it should "report Nothing when nothing has damaged the screen since the last publish" in {
    val (damage, reset) = DamageAccumulator.observeScreenPublish(Damage.Nothing)

    damage shouldBe Damage.Nothing
    reset shouldBe Damage.Nothing
  }
