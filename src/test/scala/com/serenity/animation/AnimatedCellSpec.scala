package com.serenity.animation

import java.awt.Color

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `AnimatedCell` (issue #1574) now carries its foreground/background colour as `Option[Tween[Color]]` -- the same
  * primitive every other tweened value in this codebase uses -- rather than the old `ColorTimeline`/step-list pair.
  * `cycling`/step-list advance (`.drop(1)`/`rotate`) had no production caller (grepped: only this spec constructed
  * them) and is retired along with the fields it advanced.
  */
class AnimatedCellSpec extends AnyFlatSpec with Matchers:

  private val black = new Color(0, 0, 0)
  private val white = new Color(255, 255, 255)
  private val red   = new Color(255, 0, 0)
  private val blue  = new Color(0, 0, 255)

  // ── Current colours ───────────────────────────────────────────────────────

  "AnimatedCell.currentForeground" should "return the tween's current value" in {
    val cell = AnimatedCell(None, foregroundAnimation = Some(Tween(black, white, EasingCurve.Linear, steps = 1)))
    cell.currentForeground shouldEqual Some(black)
    cell.advance().currentForeground shouldEqual Some(white)
  }

  it should "return None when there is no foreground animation" in
    AnimatedCell(None, backgroundAnimation = Some(Tween(red, blue, EasingCurve.Linear, steps = 1))).currentForeground
      .shouldEqual(None)

  "AnimatedCell.currentBackground" should "return the tween's current value" in {
    val cell = AnimatedCell(None, backgroundAnimation = Some(Tween(red, blue, EasingCurve.Linear, steps = 1)))
    cell.currentBackground shouldEqual Some(red)
    cell.advance().currentBackground shouldEqual Some(blue)
  }

  it should "return None when there is no background animation" in
    AnimatedCell(None, foregroundAnimation = Some(Tween(black, white, EasingCurve.Linear, steps = 1))).currentBackground
      .shouldEqual(None)

  // ── isComplete / advance ─────────────────────────────────────────────────

  "AnimatedCell" should "be complete when it carries no animation at all" in {
    AnimatedCell(None).isComplete should be(true)
  }

  it should "not be complete while its foreground tween still has steps left" in {
    val cell = AnimatedCell(None, foregroundAnimation = Some(Tween(black, white, EasingCurve.Linear, steps = 2)))
    cell.isComplete should be(false)
    cell.advance().isComplete should be(false)
    cell.advance().advance().isComplete should be(true)
  }

  it should "not be complete while its background tween still has steps left" in {
    val cell = AnimatedCell(None, backgroundAnimation = Some(Tween(red, blue, EasingCurve.Linear, steps = 2)))
    cell.isComplete should be(false)
    cell.advance().isComplete should be(false)
    cell.advance().advance().isComplete should be(true)
  }

  it should "require both foreground and background tweens to be complete" in {
    val cell = AnimatedCell(
      None,
      foregroundAnimation = Some(Tween(black, white, EasingCurve.Linear, steps = 1)),
      backgroundAnimation = Some(Tween(red, blue, EasingCurve.Linear, steps = 2))
    )
    cell.advance().isComplete should be(false) // background still has one step left
    cell.advance().advance().isComplete should be(true)
  }

  it should "advance an animation-free cell without error and remain complete" in {
    val cell = AnimatedCell(None)
    cell.advance() shouldEqual cell
  }

  // ── complete() ────────────────────────────────────────────────────────────

  "AnimatedCell.complete()" should "clear both animations and mark the cell complete" in {
    val cell = AnimatedCell(
      None,
      foregroundAnimation = Some(Tween(black, white, EasingCurve.Linear, steps = 2)),
      backgroundAnimation = Some(Tween(red, blue, EasingCurve.Linear, steps = 2))
    )
    val done = cell.complete()
    done.foregroundAnimation shouldEqual None
    done.backgroundAnimation shouldEqual None
    done.isComplete should be(true)
  }

  // ── Smart constructors ────────────────────────────────────────────────────

  "AnimatedCell.fromThemeTransition" should "interpolate both foreground and background" in {
    val cell = AnimatedCell.fromThemeTransition(
      oldForeground = black,
      newForeground = white,
      oldBackground = red,
      newBackground = blue,
      steps = 4
    )
    cell.content shouldEqual None
    cell.foregroundAnimation shouldEqual Some(Tween(black, white, EasingCurve.Linear, steps = 4))
    cell.backgroundAnimation shouldEqual Some(Tween(red, blue, EasingCurve.Linear, steps = 4))
  }

  "AnimatedCell.completed" should "produce a cell with a single static foreground step" in {
    val cell = AnimatedCell.completed('z', white)
    cell.content shouldEqual Some('z')
    cell.currentForeground shouldEqual Some(white)
    cell.currentBackground shouldEqual None
    cell.isComplete should be(false)
    cell.advance().isComplete should be(true)
    cell.advance().currentForeground shouldEqual Some(white)
  }

  "AnimatedCell.parametricForeground" should "advance delayed colour interpolation via a single Tween" in {
    val cell = AnimatedCell.parametricForeground('a', black, white, steps = 3, delayFrames = 2)

    cell.currentForeground shouldBe Some(black)
    cell.advance().currentForeground shouldBe Some(black)
    cell.advance().advance().currentForeground shouldBe Some(black)
    cell.advance().advance().advance().currentForeground shouldBe Some(new Color(85, 85, 85))
    cell.advance().advance().advance().advance().currentForeground shouldBe Some(new Color(170, 170, 170))
    cell.advance().advance().advance().advance().advance().currentForeground shouldBe Some(white)
    cell.advance().advance().advance().advance().advance().isComplete shouldBe true
  }

  it should "leave backgroundAnimation unset" in {
    AnimatedCell.parametricForeground('a', black, white, steps = 3).backgroundAnimation shouldBe None
  }

  it should "produce no foreground animation for zero or fewer steps" in {
    AnimatedCell.parametricForeground('a', black, white, steps = 0).foregroundAnimation shouldBe None
  }
