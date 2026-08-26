package com.serenity.animation

import java.awt.Color

import com.serenity.rope.{Balance, Rope}
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

  it should "reuse empty state when advancing animations" in {
    val state = AnimationState.empty

    state.advanceAnimations() should be theSameInstanceAs state
    state.advanceAllAnimations() should be theSameInstanceAs state
  }

  it should "clear only animations owned by the disabled motion family" in {
    val editor = AnimatedCell(Some('e'), List(black, white), List.empty)
    val ui     = AnimatedCell(Some('u'), List(black, white), List.empty, owner = AnimationOwner.UiTransitions)
    val state = AnimationState(
      Map(
        CharacterKey(0, 0) -> editor,
        CharacterKey(1, 0) -> ui
      )
    )

    state.clear(AnimationOwner.EditorText).animations.values.map(_.owner).toSet shouldBe Set(
      AnimationOwner.UiTransitions
    )
    state.clear(AnimationOwner.UiTransitions).animations.values.map(_.owner).toSet shouldBe Set(
      AnimationOwner.EditorText
    )
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

  it should "reuse inactive state when advancing and cleanup is unnecessary" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 1, 1, black, white, 1)
      .advanceAllAnimations()

    state.hasActiveAnimations should be(false)
    state.advanceAllAnimations() should be theSameInstanceAs state
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

  it should "advance every cell when no relevance predicate is given, matching prior behavior" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 1, 1, black, white, 3)
      .addCharacterAnimation('b', 2, 1, red, yellow, 3)

    val advanced = state.advanceAnimations()

    advanced.getCell(1, 1).get should not equal state.getCell(1, 1).get
    advanced.getCell(2, 1).get should not equal state.getCell(2, 1).get
  }

  it should "leave cells excluded by the relevance predicate untouched while advancing the rest" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 1, 1, black, white, 3)
      .addCharacterAnimation('b', 2, 1, red, yellow, 3)

    val onlyFirstColumnRelevant: CharacterKey => Boolean = _.column == 1
    val advanced                                         = state.advanceAnimations(onlyFirstColumnRelevant)

    advanced.getCell(1, 1).get should not equal state.getCell(1, 1).get
    advanced.getCell(2, 1).get shouldEqual state.getCell(2, 1).get
  }

  it should "never complete an excluded cell no matter how many ticks pass while it stays excluded" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 1, 1, black, white, 1)

    val neverRelevant: CharacterKey => Boolean = _ => false
    val advanced = state.advanceAllAnimations(neverRelevant).advanceAllAnimations(neverRelevant)

    advanced.getCell(1, 1) should be(defined)
    advanced.getCell(1, 1).get.isComplete should be(false)
    advanced.hasActiveAnimations should be(true)
  }

  it should "resume and complete an excluded cell once it becomes relevant again" in {
    val state = AnimationState.empty
      .addCharacterAnimation('a', 1, 1, black, white, 1)

    val excluded = state.advanceAllAnimations(_ => false)
    excluded.getCell(1, 1).get.isComplete should be(false)
    excluded.hasActiveAnimations should be(true)

    // A single-step animation completes (and is cleaned up) on its first real advance.
    val resumed = excluded.advanceAllAnimations(_ => true)
    resumed.getCell(1, 1) should be(empty)
    resumed.hasActiveAnimations should be(false)
  }

  "remapThroughEdits" should "move a key past an insertion earlier in the text" in {
    given Balance = Balance.default
    val before    = Rope("line one\nline two")
    val after     = Rope("\nline one\nline two")
    val state     = AnimationState.empty.addCharacterAnimation('X', 3, 1, black, white, 5)

    val remapped = state.remapThroughEdits(before, after, List(TextEdit(0, 0, "\n")))

    remapped.getCell(3, 1) should be(empty)
    remapped.getCell(3, 2) should be(defined)
  }

  it should "drop an animation whose character an edit deletes" in {
    given Balance = Balance.default
    val before    = Rope("Helloa")
    val after     = Rope("Hello")
    val state     = AnimationState.empty.addCharacterAnimation('a', 5, 0, black, white, 5)

    val remapped = state.remapThroughEdits(before, after, List(TextEdit(5, 6, "")))

    remapped.animations shouldBe empty
  }

  it should "leave a key before every edit untouched" in {
    given Balance = Balance.default
    val before    = Rope("ab\ncd")
    val after     = Rope("abX\ncd")
    val state     = AnimationState.empty.addCharacterAnimation('a', 0, 0, black, white, 5)

    val remapped = state.remapThroughEdits(before, after, List(TextEdit(2, 2, "X")))

    remapped.getCell(0, 0) should be(defined)
  }

  it should "be a no-op when there are no edits or no animations" in {
    given Balance = Balance.default
    val before    = Rope("abc")
    val state     = AnimationState.empty.addCharacterAnimation('a', 0, 0, black, white, 5)

    state.remapThroughEdits(before, before, Nil) shouldEqual state
    AnimationState.empty.remapThroughEdits(before, before, List(TextEdit(0, 0, "x"))) shouldEqual AnimationState.empty
  }
