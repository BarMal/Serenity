package com.serenity.animation

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WindowSitterSpec extends AnyFlatSpec with Matchers:

  "WindowSitter" should "wake and cycle its decoration after printable input" in {
    val initial = WindowSitter.default
    val awake   = initial.observeTyping(1_000_000_000L)

    awake.isActive shouldBe true
    awake.glyph shouldBe "o"

    val advanced = awake.advance
    advanced.glyph shouldBe "O"
    advanced.isActive shouldBe true
  }

  it should "keep a faster typing cadence active for longer" in {
    val first = WindowSitter.default.observeTyping(1_000_000_000L)
    val fast  = first.observeTyping(1_020_000_000L)
    val slow  = first.observeTyping(1_500_000_000L)

    fast.activeTicks should be > slow.activeTicks
  }

  it should "settle after its activity window completes" in {
    val active  = WindowSitter.default.observeTyping(1_000_000_000L)
    val settled = Iterator.iterate(active)(_.advance).dropWhile(_.isActive).next()

    settled.isActive shouldBe false
    settled.glyph shouldBe "·"
  }

  it should "use persisted frame and cadence settings" in {
    val settings = WindowSitterConfig(
      action = WindowSitterAction.Blink,
      frames = Vector(".", "x"),
      activeTicks = 3,
      fastActiveTicks = 7,
      fastTypingThresholdMs = 250
    )

    val sitter = WindowSitter.default.observeTyping(1_000_000_000L, settings)
    sitter.frames shouldBe Vector(".", "x")
    sitter.activeTicks shouldBe 3
    sitter.advance.glyph shouldBe "."
    sitter.observeTyping(1_100_000_000L, settings).activeTicks shouldBe 7
  }
