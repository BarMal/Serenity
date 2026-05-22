package com.serenity.animation

import com.googlecode.lanterna.TextColor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AnimationStateSpec extends AnyFlatSpec with Matchers:

  "AnimationState" should "start empty" in {
    val state = AnimationState.empty
    state.getCharacterColor(5, 10) should be(empty)
    state.hasActiveAnimations should be(false)
  }

  it should "add character animation" in {
    val state           = AnimationState.empty
    val backgroundColor = TextColor.ANSI.BLACK
    val foregroundColor = TextColor.ANSI.WHITE
    val steps           = 3

    val newState = state.addCharacterAnimation(
      char = 'a',
      x = 5,
      y = 10,
      backgroundColor = backgroundColor,
      foregroundColor = foregroundColor,
      steps = steps
    )

    newState.getCharacterColor(5, 10) should be(defined)
    newState.getCharacterColor(5, 10).get shouldEqual backgroundColor // First color
    newState.hasActiveAnimations should be(true)
  }

  it should "replace animation at same position" in {
    val state = AnimationState.empty
      .addCharacterAnimation('x', 5, 10, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 3)
      .addCharacterAnimation('y', 5, 10, TextColor.ANSI.RED, TextColor.ANSI.YELLOW, 2)

    val character = state.getCharacter(5, 10)
    character should be(defined)
    character.get.char shouldEqual 'y'
    character.get.currentColor shouldEqual TextColor.ANSI.RED
  }

  it should "maintain multiple animations at different positions" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 1, 1, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 2)
      .addCharacterAnimation('b', 2, 1, TextColor.ANSI.RED, TextColor.ANSI.YELLOW, 2)
      .addCharacterAnimation('c', 1, 2, TextColor.ANSI.BLUE, TextColor.ANSI.CYAN, 2)

    state.getCharacterColor(1, 1) should be(defined)
    state.getCharacterColor(2, 1) should be(defined)
    state.getCharacterColor(1, 2) should be(defined)
    state.getCharacterColor(3, 3) should be(empty)

    state.getCharacter(1, 1).get.char shouldEqual 'a'
    state.getCharacter(2, 1).get.char shouldEqual 'b'
    state.getCharacter(1, 2).get.char shouldEqual 'c'
  }

  it should "advance all animations one step" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 1, 1, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 3)
      .addCharacterAnimation('b', 2, 1, TextColor.ANSI.RED, TextColor.ANSI.YELLOW, 2)

    val advanced = state.advanceAnimations()

    // Both characters should have advanced one step
    val charA = advanced.getCharacter(1, 1).get
    val charB = advanced.getCharacter(2, 1).get

    // Both characters should have advanced but still be active
    charA.isComplete should be(false)
    charB.isComplete should be(false)
  }

  it should "persist completed characters during advance" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 1, 1, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 2) // 2 steps
      .addCharacterAnimation('b', 2, 1, TextColor.ANSI.RED, TextColor.ANSI.YELLOW, 3)  // 3 steps

    // Advance twice - first animation should complete after 2 steps
    val advanced1 = state.advanceAnimations()
    val advanced2 = advanced1.advanceAnimations()

    // Character 'a' should still exist but be completed
    val charA = advanced2.getCharacter(1, 1).get
    val charB = advanced2.getCharacter(2, 1).get

    charA.isComplete should be(true)                    // Completed
    charB.isComplete should be(false)                   // Still animating

    // Check active animation count - only non-completed animations
    advanced2.activeAnimationCount shouldEqual 1
  }

  it should "convert all to completed on theme change" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 1, 1, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 3)
      .addCharacterAnimation('b', 2, 1, TextColor.ANSI.RED, TextColor.ANSI.YELLOW, 2)
      .advanceAnimations() // Advance once

    val themeChanged = state.onThemeChange()

    // All animations should now be completed
    val charA = themeChanged.getCharacter(1, 1).get
    val charB = themeChanged.getCharacter(2, 1).get

    charA.isComplete should be(true)
    charB.isComplete should be(true)
  }

  it should "clear all animations" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 1, 1, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 3)
      .addCharacterAnimation('b', 2, 1, TextColor.ANSI.RED, TextColor.ANSI.YELLOW, 2)

    val cleared = state.clearAll()

    cleared.getCharacterColor(1, 1) should be(empty)
    cleared.getCharacterColor(2, 1) should be(empty)
    cleared.hasActiveAnimations should be(false)
  }

  it should "handle position-based queries correctly" in {
    val state = AnimationState.empty
      .addCharacterAnimation('x', 5, 10, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 3)

    state.getCharacterColor(5, 10) should be(defined)
    state.getCharacterColor(6, 10) should be(empty)
    state.getCharacterColor(5, 11) should be(empty)
  }
