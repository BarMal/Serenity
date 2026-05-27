package com.serenity.animation

import com.googlecode.lanterna.TextColor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AnimationIntegrationSpec extends AnyFlatSpec with Matchers:

  private val black = TextColor.Factory.fromString("#000000")
  private val white = TextColor.Factory.fromString("#ffffff")
  private val red   = TextColor.Factory.fromString("#ff0000")

  "Animation System Integration" should "create realistic character animation workflow" in {
    // Simulate theme colors
    val backgroundColor = black
    val foregroundColor = white

    // Create animation config
    val animConfig = AnimationConfig.quick.get

    // Start with empty animation state
    var animState = AnimationState.empty

    // Simulate typing "Hello" with animations
    val characters = List(
      ('H', 0, 0),
      ('e', 1, 0),
      ('l', 2, 0),
      ('l', 3, 0),
      ('o', 4, 0)
    )

    // Add all characters with animations
    characters.foreach {
      case (char, x, y) =>
        animState = animState.addCharacterAnimation(
          char,
          x,
          y,
          backgroundColor,
          foregroundColor,
          animConfig.steps
        )
    }

    animState.hasActiveAnimations should be(true)
    animState.activeAnimationCount shouldEqual 5

    // All characters should start with background color
    characters.foreach {
      case (char, x, y) =>
        animState.getCharacterColor(x, y) should be(defined)
        animState.getCharacterColor(x, y).get shouldEqual backgroundColor
    }

    // Simulate several animation frames
    val frame1 = animState.advanceAnimations()
    frame1.activeAnimationCount shouldEqual 5 // All still animating

    val frame2 = frame1.advanceAnimations()
    frame2.activeAnimationCount shouldEqual 5

    // After enough frames, animations should complete but characters persist
    var currentFrame = frame2
    (1 to animConfig.steps).foreach { _ => currentFrame = currentFrame.advanceAnimations() }

    currentFrame.activeAnimationCount shouldEqual 0 // All completed (not actively animating)

    // But characters should still exist with their final colors
    characters.foreach {
      case (char, x, y) =>
        val animChar = currentFrame.getCharacter(x, y)
        animChar should be(defined)
        animChar.get.char shouldEqual char
        animChar.get.currentColor shouldEqual foregroundColor
        animChar.get.isComplete should be(true)
    }
  }

  it should "handle theme changes correctly" in {
    var animState = AnimationState.empty

    // Add some animations
    animState = animState
      .addCharacterAnimation('a', 0, 0, black, white, 6)
      .addCharacterAnimation('b', 1, 0, black, white, 6)

    // Advance partway through
    animState = animState.advanceAnimations().advanceAnimations()

    // Characters should be mid-animation
    animState.hasActiveAnimations should be(true)

    // Theme change should complete all animations
    val themeChangedState = animState.onThemeChange()

    // All characters should now be completed with final colors
    themeChangedState.getCharacter(0, 0).get.isComplete should be(true)
    themeChangedState.getCharacter(1, 0).get.isComplete should be(true)
    themeChangedState.getCharacter(0, 0).get.currentColor shouldEqual white
    themeChangedState.getCharacter(1, 0).get.currentColor shouldEqual white
  }

  it should "support character modification during animation" in {
    var animState = AnimationState.empty

    // Add animation for character 'x'
    animState = animState.addCharacterAnimation(
      'x',
      5,
      5,
      black,
      white,
      6
    )

    // Advance partway
    animState = animState.advanceAnimations().advanceAnimations()
    val originalChar = animState.getCharacter(5, 5).get
    originalChar.isComplete should be(false)

    // Character is modified (e.g., user types over it)
    // This should restart the animation with the new character
    animState = animState.addCharacterAnimation(
      'y',
      5,
      5,
      black,
      red,
      3
    )

    val newChar = animState.getCharacter(5, 5).get
    newChar.char shouldEqual 'y'
    // New animation should have color steps remaining
    newChar.isComplete should be(false)
  }
