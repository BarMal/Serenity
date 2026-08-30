package com.serenity.ui.tui

import java.awt.Color

import com.serenity.ui.theme.TextStyle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TerminalAnsiDiffSpec extends AnyFlatSpec with Matchers:

  private val esc = 0x1b.toChar.toString

  private def cell(char: Char, fg: Color = Color.WHITE, bg: Color = Color.BLACK): TerminalCell =
    TerminalCell(char.toInt, fg, bg, TextStyle.normal, CellSpan.Narrow)

  private def sgr(fg: Color, bg: Color): String =
    s"$esc[0;38;2;${fg.getRed};${fg.getGreen};${fg.getBlue};48;2;${bg.getRed};${bg.getGreen};${bg.getBlue}m"

  "TerminalAnsiDiff.emit" should "produce the empty string for two identical frames" in {
    val frame = TerminalFrame.blank(3, 2)
    TerminalAnsiDiff.emit(Some(frame), frame) shouldBe ""
  }

  it should "emit exactly one positioned SGR run for a single changed cell" in {
    val previous = TerminalFrame.blank(3, 2)
    val next     = previous.copy(cells = previous.cells.updated(1, previous.cells(1).updated(2, cell('x'))))

    val expected = s"$esc[2;3H${sgr(Color.WHITE, Color.BLACK)}x$esc[0m"
    TerminalAnsiDiff.emit(Some(previous), next) shouldBe expected
  }

  it should "emit a full clear-and-repaint when there is no previous frame" in {
    val next = TerminalFrame(1, 1, Vector(Vector(cell('x'))))

    val expected = s"$esc[2J$esc[H$esc[1;1H${sgr(Color.WHITE, Color.BLACK)}x$esc[0m"
    TerminalAnsiDiff.emit(None, next) shouldBe expected
  }

  it should "emit a full clear-and-repaint on a resize even with a previous frame present" in {
    val previous = TerminalFrame.blank(1, 1)
    val next     = TerminalFrame(2, 1, Vector(Vector(cell('a'), cell('b'))))

    val expected =
      s"$esc[2J$esc[H$esc[1;1H${sgr(Color.WHITE, Color.BLACK)}ab$esc[0m"
    TerminalAnsiDiff.emit(Some(previous), next) shouldBe expected
  }

  it should "batch a contiguous run of same-styled changed cells behind one cursor move and one SGR" in {
    val previous = TerminalFrame.blank(4, 1)
    val next     = TerminalFrame(4, 1, Vector(Vector(cell('a'), cell('b'), cell('c'), previous.cells.head(3))))

    val expected = s"$esc[1;1H${sgr(Color.WHITE, Color.BLACK)}abc$esc[0m"
    TerminalAnsiDiff.emit(Some(previous), next) shouldBe expected
  }

  it should "re-emit SGR mid-run when the colour changes cell to cell" in {
    val previous = TerminalFrame.blank(2, 1)
    val next     = TerminalFrame(2, 1, Vector(Vector(cell('a', fg = Color.RED), cell('b', fg = Color.GREEN))))

    val expected =
      s"$esc[1;1H${sgr(Color.RED, Color.BLACK)}a${sgr(Color.GREEN, Color.BLACK)}b$esc[0m"
    TerminalAnsiDiff.emit(Some(previous), next) shouldBe expected
  }

  // A background Color with alpha 0 is the "use the terminal's own native background" sentinel: emit SGR 49 (reset to
  // default background) instead of an explicit 24-bit truecolor fill, so a compositor's own transparency (e.g.
  // kitty's background_opacity) can show through cells the app never paints an opaque color into.
  it should "emit SGR 49 instead of an explicit truecolor fill for a fully transparent background" in {
    val transparentBg = new Color(0, 0, 0, 0)
    val previous      = TerminalFrame.blank(1, 1)
    val next          = TerminalFrame(1, 1, Vector(Vector(cell('x', bg = transparentBg))))

    val expected = s"$esc[1;1H$esc[0;38;2;255;255;255;49mx$esc[0m"
    TerminalAnsiDiff.emit(Some(previous), next) shouldBe expected
  }

  it should "not emit SGR 49 for an ordinary opaque background" in {
    val previous = TerminalFrame.blank(1, 1)
    val next     = TerminalFrame(1, 1, Vector(Vector(cell('x'))))

    val output = TerminalAnsiDiff.emit(Some(previous), next)
    output should include("48;2;0;0;0")
    output should not include ";49m"
  }

  it should "distinguish a transparent background from an opaque one with identical RGB when batching runs" in {
    val opaqueBlack      = Color.BLACK
    val transparentBlack = new Color(0, 0, 0, 0)
    val previous         = TerminalFrame.blank(2, 1)
    val next = TerminalFrame(
      2,
      1,
      Vector(Vector(cell('a', bg = opaqueBlack), cell('b', bg = transparentBlack)))
    )

    val expected =
      s"$esc[1;1H${sgr(Color.WHITE, opaqueBlack)}a$esc[0;38;2;255;255;255;49mb$esc[0m"
    TerminalAnsiDiff.emit(Some(previous), next) shouldBe expected
  }
