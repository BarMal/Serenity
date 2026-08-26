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
