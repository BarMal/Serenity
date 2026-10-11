package com.serenity.ui.tui

import java.awt.Color

import com.serenity.ui.theme.TextStyle
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** `TerminalAnsiDiff`'s emitted escape sequences only earn their keep if replaying them against the previous frame
  * always lands on the target frame, whatever the previous frame happened to contain. `AnsiInterpreter` is a small
  * terminal simulator that checks this directly, for arbitrary narrow-glyph buffer pairs -- wide-glyph orphan repair is
  * a property of `TerminalScreenBuffer`'s writes rather than of the differ, and is covered separately in
  * `TerminalScreenBufferSpec`.
  */
class TerminalAnsiDiffPropertySpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  private val width  = 4
  private val height = 3

  private val genColor: Gen[Color] =
    for
      r <- Gen.choose(0, 255)
      g <- Gen.choose(0, 255)
      b <- Gen.choose(0, 255)
    yield new Color(r, g, b)

  /** Background colors additionally cover the alpha-0 "use the terminal's native background" sentinel (SGR 49), on top
    * of the ordinary opaque truecolor backgrounds `genColor` produces.
    */
  private val genBgColor: Gen[Color] =
    Gen.frequency(9 -> genColor, 1 -> Gen.const(AnsiInterpreter.TransparentBackground))

  private val genStyle: Gen[TextStyle] =
    for
      bold      <- Gen.oneOf(true, false)
      italic    <- Gen.oneOf(true, false)
      underline <- Gen.oneOf(true, false)
    yield TextStyle(isBold = bold, isItalic = italic, isUnderlined = underline)

  private val genCell: Gen[TerminalCell] =
    for
      char  <- Gen.oneOf('a' to 'z')
      fg    <- genColor
      bg    <- genBgColor
      style <- genStyle
    yield TerminalCell(char.toInt, fg, bg, style, CellSpan.Narrow)

  private val genFrame: Gen[TerminalFrame] =
    Gen
      .listOfN(height, Gen.listOfN(width, genCell))
      .map(rows => TerminalFrame(width, height, rows.map(_.toVector).toVector))

  property("replaying an emitted diff against the previous frame always reproduces the next frame") {
    forAll(genFrame, genFrame) { (previous, next) =>
      val ansi   = TerminalAnsiDiff.emit(Some(previous), next)
      val result = AnsiInterpreter(previous, ansi)
      result shouldBe next
    }
  }

  property("replaying a full repaint always reproduces the frame regardless of what came before") {
    forAll(genFrame, genFrame) { (arbitraryPrior, next) =>
      val ansi   = TerminalAnsiDiff.emit(None, next)
      val result = AnsiInterpreter(arbitraryPrior, ansi)
      result shouldBe next
    }
  }

  private val scrollWidth  = 6
  private val scrollHeight = 8

  private def genFrameOf(frameWidth: Int, frameHeight: Int): Gen[TerminalFrame] =
    Gen
      .listOfN(frameHeight, Gen.listOfN(frameWidth, genCell))
      .map(rows => TerminalFrame(frameWidth, frameHeight, rows.map(_.toVector).toVector))

  /** A frame and what it becomes when a random band of its rows scrolls up or down, with fresh rows exposed. */
  private val genScrolled: Gen[(TerminalFrame, TerminalFrame)] =
    for
      previous <- genFrameOf(scrollWidth, scrollHeight)
      fresh    <- genFrameOf(scrollWidth, scrollHeight)
      top      <- Gen.choose(0, scrollHeight - 3)
      bottom   <- Gen.choose(top + 2, scrollHeight - 1)
      distance <- Gen.choose(1, bottom - top)
      up       <- Gen.oneOf(true, false)
    yield
      val shift = if up then distance else -distance
      val rows = Vector.tabulate(scrollHeight) { y =>
        val source = y + shift
        if y < top || y > bottom then previous.cells(y)
        else if source >= top && source <= bottom then previous.cells(source)
        else fresh.cells(y)
      }
      (previous, TerminalFrame(scrollWidth, scrollHeight, rows))

  property("replaying a diff whose rows scrolled reproduces the next frame, with or without scroll regions") {
    forAll(genScrolled) { (previous, next) =>
      AnsiInterpreter(previous, TerminalAnsiDiff.emit(Some(previous), next)) shouldBe next
      AnsiInterpreter(previous, TerminalAnsiDiff.emit(Some(previous), next, scrollRegions = false)) shouldBe next
    }
  }
