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
      bg    <- genColor
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
