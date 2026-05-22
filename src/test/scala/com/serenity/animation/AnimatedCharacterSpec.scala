//package com.serenity.animation
//
//import com.googlecode.lanterna.TextColor
//import org.scalatest.flatspec.AnyFlatSpec
//import org.scalatest.matchers.should.Matchers
//
//class AnimatedCharacterSpec extends AnyFlatSpec with Matchers:
//
//  "AnimatedCharacter" should "start with first transition color" in {
//    val transitionColors = List(
//      TextColor.ANSI.BLACK,
//      TextColor.ANSI.BLACK_BRIGHT,
//      TextColor.ANSI.WHITE
//    )
//    val character = AnimatedCharacter(
//      char = 'a',
//      finalColor = TextColor.ANSI.WHITE,
//      transitionColors = transitionColors,
//      currentStep = 0
//    )
//
//    character.currentColor shouldEqual TextColor.ANSI.BLACK
//    character.isAnimationComplete should be(false)
//  }
//
//  it should "advance to next color on step" in {
//    val transitionColors = List(
//      TextColor.ANSI.BLACK,
//      TextColor.ANSI.BLACK_BRIGHT,
//      TextColor.ANSI.WHITE
//    )
//    val character = AnimatedCharacter(
//      char = 'a',
//      finalColor = TextColor.ANSI.WHITE,
//      transitionColors = transitionColors,
//      currentStep = 0
//    )
//
//    val stepped = character.advanceStep()
//    stepped.currentStep shouldEqual 1
//    stepped.transitionColors shouldEqual List(TextColor.ANSI.BLACK_BRIGHT, TextColor.ANSI.WHITE)
//    stepped.currentColor shouldEqual TextColor.ANSI.BLACK_BRIGHT // Head of new list
//    stepped.isAnimationComplete should be(false)
//  }
//
//  it should "complete animation when all steps exhausted" in {
//    val transitionColors = List(
//      TextColor.ANSI.WHITE // Last color in list
//    )
//    val character = AnimatedCharacter(
//      char = 'a',
//      finalColor = TextColor.ANSI.WHITE,
//      transitionColors = transitionColors,
//      currentStep = 2
//    )
//
//    character.currentColor shouldEqual TextColor.ANSI.WHITE // Head
//    character.isAnimationComplete should be(false)
//
//    val stepped = character.advanceStep()
//    stepped.transitionColors should be(empty) // Tail of single-element list
//    stepped.isAnimationComplete should be(true)
//    stepped.currentColor shouldEqual TextColor.ANSI.WHITE // Final color
//  }
//
//  it should "use final color when animation complete" in {
//    val transitionColors = List(
//      TextColor.ANSI.BLACK,
//      TextColor.ANSI.BLACK_BRIGHT
//    )
//    val finalColor = TextColor.ANSI.WHITE
//    val character = AnimatedCharacter(
//      char = 'a',
//      finalColor = finalColor,
//      transitionColors = transitionColors,
//      currentStep = 10 // Way beyond range
//    ).markComplete()
//
//    character.currentColor shouldEqual finalColor
//    character.isAnimationComplete should be(true)
//  }
//
//  it should "handle empty transition list" in {
//    val character = AnimatedCharacter(
//      char = 'x',
//      finalColor = TextColor.ANSI.RED,
//      transitionColors = List.empty,
//      currentStep = 0
//    )
//
//    character.currentColor shouldEqual TextColor.ANSI.RED
//    character.isAnimationComplete should be(true) // Empty list means complete
//
//    val stepped = character.advanceStep()
//    stepped.isAnimationComplete should be(true) // Should remain complete
//  }
//
//  it should "restart animation with new colors" in {
//    val oldTransitions = List(TextColor.ANSI.BLACK, TextColor.ANSI.WHITE)
//    val character = AnimatedCharacter(
//      char = 'b',
//      finalColor = TextColor.ANSI.WHITE,
//      transitionColors = oldTransitions,
//      currentStep = 1
//    )
//
//    val newTransitions = List(
//      TextColor.ANSI.RED,
//      TextColor.ANSI.RED_BRIGHT,
//      TextColor.ANSI.YELLOW
//    )
//    val restarted = character.restartWith(newTransitions, TextColor.ANSI.YELLOW)
//
//    restarted.currentStep shouldEqual 0
//    restarted.transitionColors shouldEqual newTransitions
//    restarted.finalColor shouldEqual TextColor.ANSI.YELLOW
//    restarted.currentColor shouldEqual TextColor.ANSI.RED
//    restarted.isAnimationComplete should be(false)
//  }
//
//  "AnimatedCharacter.completed" should "create a completed character" in {
//    val character = AnimatedCharacter.completed('z', TextColor.ANSI.BLUE)
//
//    character.char shouldEqual 'z'
//    character.finalColor shouldEqual TextColor.ANSI.BLUE
//    character.transitionColors should be(empty)
//    character.currentStep shouldEqual 0
//    character.isAnimationComplete should be(true)
//    character.currentColor shouldEqual TextColor.ANSI.BLUE
//  }
//
//  "AnimatedCharacter.fromInterpolation" should "create character from color interpolation" in {
//    val startColor = TextColor.ANSI.BLACK
//    val endColor   = TextColor.ANSI.WHITE
//    val steps      = 4
//
//    val character = AnimatedCharacter.fromInterpolation('c', startColor, endColor, steps)
//
//    character.char shouldEqual 'c'
//    character.finalColor shouldEqual endColor
//    character.transitionColors should have length steps
//    character.transitionColors.head shouldEqual startColor
//    character.transitionColors.last shouldEqual endColor
//    character.currentStep shouldEqual 0
//    character.currentColor shouldEqual startColor
//    character.isAnimationComplete should be(false)
//  }
