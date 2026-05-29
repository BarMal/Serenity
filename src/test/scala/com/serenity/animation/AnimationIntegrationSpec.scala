package com.serenity.animation

import com.googlecode.lanterna.TextColor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AnimationIntegrationSpec extends AnyFlatSpec with Matchers:

  private val black = TextColor.Factory.fromString("#000000")
  private val white = TextColor.Factory.fromString("#ffffff")
  private val red   = TextColor.Factory.fromString("#ff0000")

  "Animation System Integration" should "create realistic character animation workflow" in {
    val backgroundColor = black
    val foregroundColor = white
    val animConfig      = AnimationConfig.quick.get
    var animState       = AnimationState.empty

    val characters = List(('H', 0, 0), ('e', 1, 0), ('l', 2, 0), ('l', 3, 0), ('o', 4, 0))

    characters.foreach { case (char, x, y) =>
      animState = animState.addCharacterAnimation(char, x, y, backgroundColor, foregroundColor, animConfig.steps)
    }

    animState.hasActiveAnimations should be(true)
    animState.activeAnimationCount shouldEqual 5

    characters.foreach { case (_, x, y) =>
      animState.getCell(x, y) should be(defined)
      animState.getCell(x, y).get.currentForeground shouldEqual Some(backgroundColor)
    }

    val frame1 = animState.advanceAnimations()
    frame1.activeAnimationCount shouldEqual 5

    val frame2 = frame1.advanceAnimations()
    frame2.activeAnimationCount shouldEqual 5

    var currentFrame = frame2
    (1 to animConfig.steps).foreach { _ => currentFrame = currentFrame.advanceAnimations() }

    currentFrame.activeAnimationCount shouldEqual 0

    characters.foreach { case (char, x, y) =>
      val cell = currentFrame.getCell(x, y)
      cell should be(defined)
      cell.get.content shouldEqual Some(char)
      cell.get.isComplete should be(true)
      // Once exhausted, currentForeground is None; renderer falls back to theme colour
      cell.get.currentForeground shouldEqual None
    }
  }

  it should "handle theme changes correctly" in {
    var animState = AnimationState.empty
      .addCharacterAnimation('a', 0, 0, black, white, 6)
      .addCharacterAnimation('b', 1, 0, black, white, 6)

    animState = animState.advanceAnimations().advanceAnimations()
    animState.hasActiveAnimations should be(true)

    val themeChangedState = animState.onThemeChange()

    themeChangedState.getCell(0, 0).get.isComplete should be(true)
    themeChangedState.getCell(1, 0).get.isComplete should be(true)
    // All fg steps cleared; renderer will use new theme colour
    themeChangedState.getCell(0, 0).get.currentForeground shouldEqual None
    themeChangedState.getCell(1, 0).get.currentForeground shouldEqual None
  }

  it should "apply background steps when cell has no foreground steps" in {
    val backgroundColor = black
    val foregroundColor = white
    val cell = AnimatedCell(
      content = Some('a'),
      foregroundSteps = List.empty,
      backgroundSteps = RgbInterpolator.interpolate(black, white, 3)
    )
    val state = AnimationState.empty.mergeAnimations(Map(CharacterKey(0, 0) -> cell))

    state.getCell(0, 0).get.currentForeground shouldEqual None
    state.getCell(0, 0).get.currentBackground shouldEqual Some(black)

    val step1 = state.advanceAnimations()
    step1.getCell(0, 0).get.currentBackground should not equal Some(black)
    step1.getCell(0, 0).get.currentBackground should not equal None
  }

  it should "track background-only cells via getLineAnimations and advance their background color" in {
    val bgCell = AnimatedCell(
      content = None,
      foregroundSteps = List.empty,
      backgroundSteps = RgbInterpolator.interpolate(black, white, 3)
    )
    val state = AnimationState.empty
      .mergeAnimations(Map(CharacterKey(5, 2) -> bgCell))
      .mergeAnimations(Map(CharacterKey(7, 2) -> bgCell))
      .mergeAnimations(Map(CharacterKey(3, 4) -> bgCell))

    val line2Animations = state.getLineAnimations(2)
    line2Animations.keys should contain(5)
    line2Animations.keys should contain(7)
    line2Animations.keys should not contain 3

    line2Animations(5).currentBackground shouldEqual Some(black)
    line2Animations(5).currentForeground shouldEqual None

    val advanced = state.advanceAnimations()
    advanced.getLineAnimations(2)(5).currentBackground should not equal Some(black)
    advanced.getLineAnimations(2)(5).currentBackground should not equal None
  }

  it should "support character modification during animation" in {
    var animState = AnimationState.empty
      .addCharacterAnimation('x', 5, 5, black, white, 6)

    animState = animState.advanceAnimations().advanceAnimations()
    animState.getCell(5, 5).get.isComplete should be(false)

    animState = animState.addCharacterAnimation('y', 5, 5, black, red, 3)

    val cell = animState.getCell(5, 5).get
    cell.content shouldEqual Some('y')
    cell.isComplete should be(false)
  }
