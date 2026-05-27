package com.serenity.animation

import com.googlecode.lanterna.TextColor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListBasedAnimationSpec extends AnyFlatSpec with Matchers:

  "AnimatedCharacter list-based advancement" should "consume color steps on advance" in {
    val colors = List(TextColor.ANSI.BLACK, TextColor.ANSI.WHITE)
    val char   = AnimatedCharacter('a', colors)

    char.currentColor shouldEqual TextColor.ANSI.BLACK
    char.isComplete should be(false)

    val advanced = char.advance()
    advanced.currentColor shouldEqual TextColor.ANSI.WHITE
    advanced.isComplete should be(false)

    val completed = advanced.advance()
    completed.isComplete should be(true)
  }

  it should "create deterministic animation duration" in {
    val char = AnimatedCharacter.createFadeAnimation(
      'x',
      TextColor.ANSI.BLACK,
      TextColor.ANSI.WHITE,
      durationMs = 96,
      tickRateMs = 16
    )

    char.colorSteps.length shouldEqual 6 // Exactly 6 steps for 96ms duration
  }

  it should "handle immediate completion for zero-step animations" in {
    val char = AnimatedCharacter.createFadeAnimation(
      'x',
      TextColor.ANSI.WHITE,
      TextColor.ANSI.WHITE,
      durationMs = 0
    )

    char.isComplete shouldEqual true
  }

  it should "handle single-step animations" in {
    val char = AnimatedCharacter.createFadeAnimation(
      'x',
      TextColor.ANSI.BLACK,
      TextColor.ANSI.WHITE,
      durationMs = 16,
      tickRateMs = 16
    )

    char.colorSteps.length shouldEqual 1
    char.currentColor shouldEqual TextColor.ANSI.WHITE
    char.advance().isComplete shouldEqual true
  }

  "AnimationState list-based advancement" should "advance all animations automatically" in {
    val animState = AnimationState.empty
      .addCharacterAnimation('a', 0, 0, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 3)
      .addCharacterAnimation('b', 1, 0, TextColor.ANSI.RED, TextColor.ANSI.BLUE, 2)

    animState.hasActiveAnimations should be(true)
    animState.activeAnimationCount shouldEqual 2

    val frame1 = animState.advanceAllAnimations()
    frame1.activeAnimationCount shouldEqual 2

    val frame2 = frame1.advanceAllAnimations()
    frame2.activeAnimationCount shouldEqual 1 // 'b' completed

    val frame3 = frame2.advanceAllAnimations()
    frame3.activeAnimationCount shouldEqual 0 // All completed
  }

  it should "automatically cleanup completed animations" in {
    val animState = AnimationState.empty
      .addCharacterAnimation('x', 0, 0, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 1)

    animState.animations.size shouldEqual 1

    val advanced = animState.advanceAllAnimations().cleanupCompleted()

    // Character should be gone after cleanup
    advanced.animations.size shouldEqual 0
    advanced.getCharacterColor(0, 0) should be(None)
  }

  it should "handle rapid overlapping animations" in {
    var state = AnimationState.empty

    // Simulate typing "abc" rapidly
    state = state.addCharacterAnimation('a', 0, 0, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 6)
    state = state.addCharacterAnimation('b', 1, 0, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 6)
    state = state.addCharacterAnimation('c', 2, 0, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 6)

    state.activeAnimationCount shouldEqual 3

    // All should advance independently
    var current = state
    (1 to 3).foreach { _ => current = current.advanceAllAnimations() }
    current.activeAnimationCount shouldEqual 3 // All still active

    (1 to 3).foreach { _ => current = current.advanceAnimations() }
    current.activeAnimationCount shouldEqual 0 // All completed

    // Characters should still be accessible before cleanup
    current.getCharacterColor(0, 0) should be(defined)
    current.getCharacterColor(1, 0) should be(defined)
    current.getCharacterColor(2, 0) should be(defined)

    // After cleanup, completed animations are removed
    val cleanedUp = current.cleanupCompleted()
    cleanedUp.getCharacterColor(0, 0) should be(empty)
    cleanedUp.getCharacterColor(1, 0) should be(empty)
    cleanedUp.getCharacterColor(2, 0) should be(empty)
  }

  "List-based animation lifecycle" should "create, advance, and cleanup automatically" in {
    val char = AnimatedCharacter.createFadeAnimation(
      'x',
      TextColor.ANSI.BLACK,
      TextColor.ANSI.WHITE,
      durationMs = 48,
      tickRateMs = 16
    )

    char.colorSteps.length shouldEqual 3 // 48ms / 16ms = 3 steps

    // Step 1: first color
    val step1 = char.advance()
    step1.isComplete should be(false)

    // Step 2: second color
    val step2 = step1.advance()
    step2.isComplete should be(false)

    // Step 3: final color and completion
    val step3 = step2.advance()
    step3.isComplete should be(true)
  }
