package com.serenity.animation

import com.googlecode.lanterna.TextColor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListBasedAnimationSpec extends AnyFlatSpec with Matchers:

  "AnimatedCell list-based advancement" should "consume foreground steps on advance" in {
    val cell = AnimatedCell(Some('a'), List(TextColor.ANSI.BLACK, TextColor.ANSI.WHITE), List.empty)

    cell.currentForeground shouldEqual Some(TextColor.ANSI.BLACK)
    cell.isComplete should be(false)

    val advanced = cell.advance()
    advanced.currentForeground shouldEqual Some(TextColor.ANSI.WHITE)
    advanced.isComplete should be(false)

    val completed = advanced.advance()
    completed.isComplete should be(true)
  }

  it should "create deterministic animation duration" in {
    val cell = AnimatedCell.createFadeAnimation(
      'x',
      TextColor.ANSI.BLACK,
      TextColor.ANSI.WHITE,
      durationMs = 96,
      tickRateMs = 16
    )
    cell.foregroundSteps.length shouldEqual 6
  }

  it should "handle immediate completion for zero-step animations" in {
    val cell = AnimatedCell.createFadeAnimation('x', TextColor.ANSI.WHITE, TextColor.ANSI.WHITE, durationMs = 0)
    cell.isComplete shouldEqual true
  }

  it should "handle single-step animations" in {
    val cell = AnimatedCell.createFadeAnimation(
      'x',
      TextColor.ANSI.BLACK,
      TextColor.ANSI.WHITE,
      durationMs = 16,
      tickRateMs = 16
    )
    cell.foregroundSteps.length shouldEqual 1
    cell.advance().isComplete shouldEqual true
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
    frame2.activeAnimationCount shouldEqual 1

    val frame3 = frame2.advanceAllAnimations()
    frame3.activeAnimationCount shouldEqual 0
  }

  it should "automatically cleanup completed animations" in {
    val animState = AnimationState.empty
      .addCharacterAnimation('x', 0, 0, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 1)

    animState.animations.size shouldEqual 1

    val cleaned = animState.advanceAllAnimations().cleanupCompleted()

    cleaned.animations.size shouldEqual 0
    cleaned.getCell(0, 0) should be(None)
  }

  it should "handle rapid overlapping animations" in {
    var state = AnimationState.empty
      .addCharacterAnimation('a', 0, 0, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 6)
      .addCharacterAnimation('b', 1, 0, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 6)
      .addCharacterAnimation('c', 2, 0, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 6)

    state.activeAnimationCount shouldEqual 3

    var current = state
    (1 to 3).foreach { _ => current = current.advanceAllAnimations() }
    current.activeAnimationCount shouldEqual 3

    (1 to 3).foreach { _ => current = current.advanceAnimations() }
    current.activeAnimationCount shouldEqual 0

    // Completed cells remain in the map until explicitly cleaned up
    current.getCell(0, 0) should be(defined)
    current.getCell(1, 0) should be(defined)
    current.getCell(2, 0) should be(defined)

    val cleanedUp = current.cleanupCompleted()
    cleanedUp.getCell(0, 0) should be(empty)
    cleanedUp.getCell(1, 0) should be(empty)
    cleanedUp.getCell(2, 0) should be(empty)
  }

  "List-based animation lifecycle" should "create, advance, and complete deterministically" in {
    val cell = AnimatedCell.createFadeAnimation(
      'x',
      TextColor.ANSI.BLACK,
      TextColor.ANSI.WHITE,
      durationMs = 48,
      tickRateMs = 16
    )

    cell.foregroundSteps.length shouldEqual 3

    val step1 = cell.advance()
    step1.isComplete should be(false)

    val step2 = step1.advance()
    step2.isComplete should be(false)

    val step3 = step2.advance()
    step3.isComplete should be(true)
  }
