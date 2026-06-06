package com.serenity.animation

import java.awt.Color

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AnimationStateSpec extends AnyFlatSpec with Matchers:

  private val black  = Color.BLACK
  private val white  = Color.WHITE
  private val red    = Color.RED
  private val yellow = Color.YELLOW
  private val blue   = Color.BLUE
  private val cyan   = Color.CYAN

  "AnimationState" should "start empty" in {
    val state = AnimationState.empty
    state.getCell(5, 10) should be(empty)
    state.hasActiveAnimations should be(false)
  }

  it should "add character animation" in {
    val state = AnimationState.empty
    val newState = state.addCharacterAnimation(
      char = 'a',
      x = 5,
      y = 10,
      startColor = black,
      endColor = white,
      steps = 3
    )

    newState.getCell(5, 10) should be(defined)
    newState.getCell(5, 10).get.currentForeground shouldEqual Some(black)
    newState.hasActiveAnimations should be(true)
  }

  it should "replace animation at same position" in {
    val state = AnimationState.empty
      .addCharacterAnimation('x', 5, 10, black, white, 3)
      .addCharacterAnimation('y', 5, 10, red, yellow, 2)

    val cell = state.getCell(5, 10)
    cell should be(defined)
    cell.get.content shouldEqual Some('y')
    cell.get.currentForeground shouldEqual Some(red)
  }

  it should "maintain multiple animations at different positions" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 1, 1, black, white, 2)
      .addCharacterAnimation('b', 2, 1, red, yellow, 2)
      .addCharacterAnimation('c', 1, 2, blue, cyan, 2)

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
      .addCharacterAnimation('a', 1, 1, black, white, 3)
      .addCharacterAnimation('b', 2, 1, red, yellow, 2)

    val advanced = state.advanceAnimations()

    val cellA = advanced.getCell(1, 1).get
    val cellB = advanced.getCell(2, 1).get

    cellA.isComplete should be(false)
    cellB.isComplete should be(false)
  }

  it should "persist completed cells during advance" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 1, 1, black, white, 2)
      .addCharacterAnimation('b', 2, 1, red, yellow, 3)

    val advanced2 = state.advanceAnimations().advanceAnimations()

    val cellA = advanced2.getCell(1, 1).get
    val cellB = advanced2.getCell(2, 1).get

    cellA.isComplete should be(true)
    cellB.isComplete should be(false)
    advanced2.activeAnimationCount shouldEqual 1
  }

  it should "convert all to completed on theme change" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 1, 1, black, white, 3)
      .addCharacterAnimation('b', 2, 1, red, yellow, 2)
      .advanceAnimations()

    val themeChanged = state.onThemeChange()

    themeChanged.getCell(1, 1).get.isComplete should be(true)
    themeChanged.getCell(2, 1).get.isComplete should be(true)
  }

  it should "clear all animations" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 1, 1, black, white, 3)
      .addCharacterAnimation('b', 2, 1, red, yellow, 2)

    val cleared = state.clearAll()

    cleared.getCell(1, 1) should be(empty)
    cleared.getCell(2, 1) should be(empty)
    cleared.hasActiveAnimations should be(false)
  }

  it should "handle position-based queries correctly" in {
    val state = AnimationState.empty
      .addCharacterAnimation('x', 5, 10, black, white, 3)

    state.getCell(5, 10) should be(defined)
    state.getCell(6, 10) should be(empty)
    state.getCell(5, 11) should be(empty)
  }

  it should "return the current foreground color via getCharacterColor while active" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 5, 10, black, white, 2)

    state.getCharacterColor(5, 10) shouldEqual Some(black)
    state.getCharacterColor(6, 10) should be(empty)
  }

  it should "return None from getCharacterColor once animation is complete" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 5, 10, black, white, 1)
      .advanceAnimations()

    state.getCharacterColor(5, 10) should be(empty)
  }
