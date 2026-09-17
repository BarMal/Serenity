package com.serenity.animation.sprite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers the companion sprite's typing-reactivity, merged in from the retired `com.serenity.animation.WindowSitter`
  * (issue #934 v2): typing now drives the companion-sprite panel's `Walk` action instead of a title-bar decoration,
  * reusing the same dual-cadence `observeTyping` shape and [[SpriteFrameSelection]] frame math -- see
  * `WindowSitterSpec` (deleted) for the behavior this replaces.
  */
class CompanionSpriteTypingSpec extends AnyFlatSpec with Matchers:

  "CompanionSpriteState.observeTyping" should "wake into the Walk action after printable input" in {
    val awake = CompanionSpriteState.default.observeTyping(1_000_000_000L)

    awake.isTypingActive shouldBe true
    awake.action shouldBe CompanionSpriteAction.Walk
  }

  it should "keep a faster typing cadence active for longer" in {
    val first = CompanionSpriteState.default.observeTyping(1_000_000_000L)
    val fast  = first.observeTyping(1_020_000_000L)
    val slow  = first.observeTyping(1_500_000_000L)

    fast.typingActiveTicks should be > slow.typingActiveTicks
  }

  it should "settle back to Idle once the typing activity window completes" in {
    val active = CompanionSpriteState.default.observeTyping(1_000_000_000L)
    val settled = Iterator
      .iterate(active)(_.advance(new scala.util.Random(0L), actionChance = 0.0))
      .dropWhile(_.isTypingActive)
      .next()

    settled.isTypingActive shouldBe false
    settled.action shouldBe CompanionSpriteAction.Idle
  }

  it should "pulse back through intermediate frames instead of wrapping to rest, per the configured typing cycle" in {
    val settings = CompanionSpriteConfig(typingCycle = SpriteFrameCycle.Pulse, typingActiveTicks = 5)
    val awake    = CompanionSpriteState.default.observeTyping(1_000_000_000L, settings)
    val random   = new scala.util.Random(0L)

    val positions = Iterator.iterate(awake)(_.advance(random, actionChance = 0.0)).take(4).map(_.frameIndex).toVector

    positions shouldBe Vector(1, 2, 1, 0)
  }

  it should "use persisted cadence settings" in {
    val settings = CompanionSpriteConfig(
      typingCycle = SpriteFrameCycle.Blink,
      typingActiveTicks = 3,
      typingFastActiveTicks = 7,
      typingFastThresholdMs = 250
    )

    val sprite = CompanionSpriteState.default.observeTyping(1_000_000_000L, settings)
    sprite.typingActiveTicks shouldBe 3
    sprite.observeTyping(1_100_000_000L, settings).typingActiveTicks shouldBe 7
  }

  it should "not roll into a random idle trick while a typing reaction is in progress" in {
    val random = new scala.util.Random(7L)
    val awake  = CompanionSpriteState.default.observeTyping(1_000_000_000L)

    val ticked = (1 to 5).foldLeft(awake)((s, _) => s.advance(random, actionChance = 1.0))

    ticked.action shouldBe CompanionSpriteAction.Walk
  }

  "CompanionSpriteFrames.currentFrame" should "resolve the typing-driven Walk frame through the configured cycle" in {
    val frames = Map(CompanionSpriteAssets.IdleClipName -> Vector.fill(4)(new java.awt.image.BufferedImage(1, 1, 1)))
    val state = CompanionSpriteState.default
      .observeTyping(1_000_000_000L, CompanionSpriteConfig(typingCycle = SpriteFrameCycle.Blink))

    CompanionSpriteFrames.currentFrame(frames, state) shouldBe Some(frames(CompanionSpriteAssets.IdleClipName)(3))
  }
