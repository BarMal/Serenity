package com.serenity.animation

import java.awt.Color

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AnimatedCellSpec extends AnyFlatSpec with Matchers:

  private val black = new Color(0, 0, 0)
  private val white = new Color(255, 255, 255)
  private val red   = new Color(255, 0, 0)
  private val blue  = new Color(0, 0, 255)

  // ── Consuming advance ─────────────────────────────────────────────────────

  "AnimatedCell (consuming)" should "drop the head from both lists on advance" in {
    val cell = AnimatedCell(Some('a'), List(black, white), List(red, blue))
    val next = cell.advance()
    next.foregroundSteps shouldEqual List(white)
    next.backgroundSteps shouldEqual List(blue)
  }

  it should "be complete when both lists are empty" in {
    AnimatedCell(None, List.empty, List.empty).isComplete should be(true)
  }

  it should "not be complete while foregroundSteps still has entries" in {
    val cell = AnimatedCell(None, List(black, white), List.empty)
    cell.isComplete should be(false)
    cell.advance().isComplete should be(false)
    cell.advance().advance().isComplete should be(true)
  }

  it should "not be complete while backgroundSteps still has entries" in {
    val cell = AnimatedCell(None, List.empty, List(red, blue))
    cell.isComplete should be(false)
    cell.advance().isComplete should be(false)
    cell.advance().advance().isComplete should be(true)
  }

  it should "advance empty lists without error and remain complete" in {
    val cell = AnimatedCell(None, List.empty, List.empty)
    cell.advance() shouldEqual cell
  }

  it should "become complete after one advance when both lists have one element" in {
    val cell = AnimatedCell(None, List(black), List(red))
    cell.advance().isComplete should be(true)
  }

  // ── Cycling advance ───────────────────────────────────────────────────────

  "AnimatedCell (cycling)" should "rotate both lists on advance" in {
    val cell = AnimatedCell(None, List(black, white, red), List(blue, black), cycling = true)
    val next = cell.advance()
    next.foregroundSteps shouldEqual List(white, red, black)
    next.backgroundSteps shouldEqual List(black, blue)
  }

  it should "never be complete regardless of advance count" in {
    val cell = AnimatedCell(None, List(black), List(red), cycling = true)
    cell.isComplete should be(false)
    cell.advance().isComplete should be(false)
    cell.advance().advance().advance().isComplete should be(false)
  }

  it should "cycle a single-element list back to itself" in {
    val cell = AnimatedCell(None, List(black), List.empty, cycling = true)
    cell.advance().foregroundSteps shouldEqual List(black)
  }

  it should "leave empty lists unchanged when cycling" in {
    val cell = AnimatedCell(None, List(black), List.empty, cycling = true)
    cell.advance().backgroundSteps shouldEqual List.empty
  }

  // ── Current colours ───────────────────────────────────────────────────────

  "AnimatedCell.currentForeground" should "return the head of foregroundSteps" in {
    AnimatedCell(None, List(black, white), List.empty).currentForeground shouldEqual Some(black)
    AnimatedCell(None, List(black, white), List.empty).advance().currentForeground shouldEqual Some(white)
  }

  it should "return None when foregroundSteps is empty" in {
    AnimatedCell(None, List.empty, List(red)).currentForeground shouldEqual None
  }

  "AnimatedCell.currentBackground" should "return the head of backgroundSteps" in {
    AnimatedCell(None, List.empty, List(red, blue)).currentBackground shouldEqual Some(red)
    AnimatedCell(None, List.empty, List(red, blue)).advance().currentBackground shouldEqual Some(blue)
  }

  it should "return None when backgroundSteps is empty" in {
    AnimatedCell(None, List(black), List.empty).currentBackground shouldEqual None
  }

  // ── complete() ────────────────────────────────────────────────────────────

  "AnimatedCell.complete()" should "empty both lists and mark the cell complete" in {
    val cell = AnimatedCell(None, List(black, white), List(red, blue))
    val done = cell.complete()
    done.foregroundSteps shouldEqual List.empty
    done.backgroundSteps shouldEqual List.empty
    done.isComplete should be(true)
  }

  // ── Smart constructors ────────────────────────────────────────────────────

  "AnimatedCell.fromForegroundInterpolation" should "produce a foreground-only fade" in {
    val cell = AnimatedCell.fromForegroundInterpolation('x', black, white, steps = 4)
    cell.content shouldEqual Some('x')
    cell.foregroundSteps shouldEqual RgbInterpolator.interpolateRgba(black, white, 4)
    cell.backgroundSteps shouldEqual List.empty
    cell.cycling should be(false)
  }

  "AnimatedCell.fromThemeTransition" should "interpolate both foreground and background" in {
    val cell = AnimatedCell.fromThemeTransition(
      oldForeground = black,
      newForeground = white,
      oldBackground = red,
      newBackground = blue,
      steps = 4
    )
    cell.content shouldEqual None
    cell.foregroundSteps shouldEqual RgbInterpolator.interpolateRgba(black, white, 4)
    cell.backgroundSteps shouldEqual RgbInterpolator.interpolateRgba(red, blue, 4)
    cell.cycling should be(false)
  }

  "AnimatedCell.completed" should "produce a cell with a single static foreground step" in {
    val cell = AnimatedCell.completed('z', white)
    cell.content shouldEqual Some('z')
    cell.currentForeground shouldEqual Some(white)
    cell.backgroundSteps shouldEqual List.empty
    cell.isComplete should be(false)
    cell.advance().isComplete should be(true)
  }

  "AnimatedCell.createFadeAnimation" should "derive step count from duration and tick rate" in {
    val cell = AnimatedCell.createFadeAnimation('a', black, white, durationMs = 96, tickRateMs = 16)
    cell.content shouldEqual Some('a')
    cell.foregroundSteps should have length 6
    cell.backgroundSteps shouldEqual List.empty
  }

  it should "be immediately complete for zero duration" in {
    val cell = AnimatedCell.createFadeAnimation('a', black, white, durationMs = 0)
    cell.isComplete should be(true)
  }

  "AnimatedCell.parametricForeground" should "advance delayed colour interpolation without storing step lists" in {
    val cell = AnimatedCell.parametricForeground('a', black, white, steps = 3, delayFrames = 2)

    cell.foregroundSteps shouldBe empty
    cell.currentForeground shouldBe Some(black)
    cell.advance().currentForeground shouldBe Some(black)
    cell.advance().advance().currentForeground shouldBe Some(black)
    cell.advance().advance().advance().currentForeground shouldBe Some(new Color(128, 128, 128))
    cell.advance().advance().advance().advance().currentForeground shouldBe Some(white)
    cell.advance().advance().advance().advance().advance().isComplete shouldBe true
  }
