package com.serenity.animation

import com.googlecode.lanterna.TextColor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AnimationStateSpec extends AnyFlatSpec with Matchers:

  "AnimationState" should "start empty" in {
    val state = AnimationState.empty
    state.getCell(5, 10) should be(empty)
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

    newState.getCell(5, 10) should be(defined)
    newState.getCell(5, 10).get.currentForeground shouldEqual Some(backgroundColor.toColor())
    newState.hasActiveAnimations should be(true)
  }

  it should "replace animation at same position" in {
    val state = AnimationState.empty
      .addCharacterAnimation('x', 5, 10, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 3)
      .addCharacterAnimation('y', 5, 10, TextColor.ANSI.RED, TextColor.ANSI.YELLOW, 2)

    val cell = state.getCell(5, 10)
    cell should be(defined)
    cell.get.content shouldEqual Some('y')
    cell.get.currentForeground shouldEqual Some(TextColor.ANSI.RED.toColor())
  }

  it should "maintain multiple animations at different positions" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 1, 1, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 2)
      .addCharacterAnimation('b', 2, 1, TextColor.ANSI.RED, TextColor.ANSI.YELLOW, 2)
      .addCharacterAnimation('c', 1, 2, TextColor.ANSI.BLUE, TextColor.ANSI.CYAN, 2)

    state.getCell(1, 1) should be(defined)
    state.getCell(2, 1) should be(defined)
    state.getCell(1, 2) should be(defined)
    state.getCell(3, 3) should be(empty)

    state.getCell(1, 1).get.content shouldEqual Some('a')
    state.getCell(2, 1).get.content shouldEqual Some('b')
    state.getCell(1, 2).get.content shouldEqual Some('c')
  }

  it should "advance all animations one step" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 1, 1, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 3)
      .addCharacterAnimation('b', 2, 1, TextColor.ANSI.RED, TextColor.ANSI.YELLOW, 2)

    val advanced = state.advanceAnimations()

    val cellA = advanced.getCell(1, 1).get
    val cellB = advanced.getCell(2, 1).get

    cellA.isComplete should be(false)
    cellB.isComplete should be(false)
  }

  it should "persist completed cells during advance" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 1, 1, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 2)
      .addCharacterAnimation('b', 2, 1, TextColor.ANSI.RED, TextColor.ANSI.YELLOW, 3)

    val advanced2 = state.advanceAnimations().advanceAnimations()

    val cellA = advanced2.getCell(1, 1).get
    val cellB = advanced2.getCell(2, 1).get

    cellA.isComplete should be(true)
    cellB.isComplete should be(false)
    advanced2.activeAnimationCount shouldEqual 1
  }

  it should "convert all to completed on theme change" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 1, 1, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 3)
      .addCharacterAnimation('b', 2, 1, TextColor.ANSI.RED, TextColor.ANSI.YELLOW, 2)
      .advanceAnimations()

    val themeChanged = state.onThemeChange()

    themeChanged.getCell(1, 1).get.isComplete should be(true)
    themeChanged.getCell(2, 1).get.isComplete should be(true)
  }

  it should "clear all animations" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 1, 1, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 3)
      .addCharacterAnimation('b', 2, 1, TextColor.ANSI.RED, TextColor.ANSI.YELLOW, 2)

    val cleared = state.clearAll()

    cleared.getCell(1, 1) should be(empty)
    cleared.getCell(2, 1) should be(empty)
    cleared.hasActiveAnimations should be(false)
  }

  it should "handle position-based queries correctly" in {
    val state = AnimationState.empty
      .addCharacterAnimation('x', 5, 10, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 3)

    state.getCell(5, 10) should be(defined)
    state.getCell(6, 10) should be(empty)
    state.getCell(5, 11) should be(empty)
  }

  it should "return the current foreground color via getCharacterColor while active" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 5, 10, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 2)

    state.getCharacterColor(5, 10) shouldEqual Some(TextColor.ANSI.BLACK.toColor())
    state.getCharacterColor(6, 10) should be(empty)
  }

  it should "return None from getCharacterColor once animation is complete" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 5, 10, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 1)
      .advanceAnimations()

    state.getCharacterColor(5, 10) should be(empty)
  }
