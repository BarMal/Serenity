package com.serenity.ui.tui

import java.awt.Color
import java.io.StringWriter

import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.theme.TextStyle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1895: the bytes [[TerminalAnsiDiff]] spends, and that spending fewer never changes what the terminal shows. Every
  * emission is replayed through the [[TerminalEmulator]] and compared with the frame it was meant to produce; byte
  * counts are compared against [[LegacyAnsiDiff]], the encoder as it was before the change.
  */
class TerminalAnsiDiffEfficiencySpec extends AnyFlatSpec with Matchers:

  private val esc = 0x1b.toChar.toString

  private val Width  = 40
  private val Height = 12

  private def cell(char: Char, fg: Color = Color.WHITE, bg: Color = Color.BLACK, style: TextStyle = TextStyle.normal) =
    TerminalCell(char.toInt, fg, bg, style, CellSpan.Narrow)

  private def rowOf(text: String, width: Int): Vector[TerminalCell] = text.padTo(width, ' ').map(cell(_)).toVector

  private def frameOfLines(width: Int, lines: Seq[String]): TerminalFrame =
    TerminalFrame(width, lines.size, lines.map(rowOf(_, width)).toVector)

  /** Lines of distinct text, so a scroll moves whole rows that share no cells with their new neighbours. */
  private def numberedLines(count: Int, firstNumber: Int): Seq[String] =
    (0 until count).map { i =>
      val number = firstNumber + i
      f"line $number%03d " + (0 until 30).map(column => ('a' + (number * 7 + column * 3) % 26).toChar).mkString
    }

  private def withChar(line: String, x: Int, char: Char, width: Int): String =
    val padded = line.padTo(width, ' ')
    padded.take(x) + char.toString + padded.drop(x + 1)

  private def withRow(frame: TerminalFrame, y: Int, row: Vector[TerminalCell]): TerminalFrame =
    frame.copy(cells = frame.cells.updated(y, row))

  /** The SGR a cell gets when the terminal's attributes are unknown: a reset followed by everything the cell needs. */
  private def fullSgr(fg: Color, bg: Color, attributes: String = ""): String =
    s"$esc[0;$attributes" +
      s"38;2;${fg.getRed};${fg.getGreen};${fg.getBlue};48;2;${bg.getRed};${bg.getGreen};${bg.getBlue}m"

  private def replayed(previous: TerminalFrame, ansi: String): TerminalFrame = AnsiInterpreter(previous, ansi)

  // -- SGR deltas -----------------------------------------------------------------------------------------------------

  "a style change inside a run" should "send only the attribute that changed" in {
    val previous = TerminalFrame.blank(2, 1)
    val next     = TerminalFrame(2, 1, Vector(Vector(cell('a'), cell('b', style = TextStyle.bold))))

    val ansi = TerminalAnsiDiff.emit(Some(previous), next)

    ansi shouldBe s"$esc[1;1H${fullSgr(Color.WHITE, Color.BLACK)}a$esc[1mb$esc[0m"
    replayed(previous, ansi) shouldBe next
  }

  it should "send a single-attribute-off code for bold-off instead of resetting everything" in {
    val previous = TerminalFrame.blank(2, 1)
    val next     = TerminalFrame(2, 1, Vector(Vector(cell('a', style = TextStyle.bold), cell('b'))))

    val ansi = TerminalAnsiDiff.emit(Some(previous), next)

    ansi shouldBe s"$esc[1;1H${fullSgr(Color.WHITE, Color.BLACK, "1;")}a$esc[22mb$esc[0m"
    replayed(previous, ansi) shouldBe next
  }

  it should "send italic-off and underline-off as their own codes" in {
    val previous = TerminalFrame.blank(3, 1)
    val next = TerminalFrame(
      3,
      1,
      Vector(Vector(cell('a', style = TextStyle.italic), cell('b', style = TextStyle.underlined), cell('c')))
    )

    val ansi = TerminalAnsiDiff.emit(Some(previous), next)

    ansi should include(s"a$esc[23;4mb$esc[24mc")
    replayed(previous, ansi) shouldBe next
  }

  it should "fall back to a full reset when that is shorter than switching each attribute off" in {
    val loud     = TextStyle(isBold = true, isItalic = true, isUnderlined = true)
    val previous = TerminalFrame.blank(2, 1)
    val next = TerminalFrame(
      2,
      1,
      Vector(Vector(cell('a', fg = Color.RED, style = loud), cell('b', bg = Color.BLUE)))
    )

    val ansi = TerminalAnsiDiff.emit(Some(previous), next)

    ansi shouldBe
      s"$esc[1;1H$esc[0;1;3;4;38;2;255;0;0;48;2;0;0;0ma$esc[0;38;2;255;255;255;48;2;0;0;255mb$esc[0m"
    replayed(previous, ansi) shouldBe next
  }

  it should "send nothing for a cell that shares the attributes the terminal already holds" in {
    val previous = TerminalFrame.blank(3, 1)
    val next     = TerminalFrame(3, 1, Vector(Vector(cell('a'), cell('b'), cell('c'))))

    TerminalAnsiDiff.emit(Some(previous), next) shouldBe s"$esc[1;1H${fullSgr(Color.WHITE, Color.BLACK)}abc$esc[0m"
  }

  // -- cursor movement ------------------------------------------------------------------------------------------------

  "two changed cells separated by a short unchanged gap" should "be joined by rewriting the gap, not by a new move" in {
    val previous = TerminalFrame.blank(12, 1)
    val changed  = Vector.tabulate(12)(x => if x == 2 then cell('x') else if x == 5 then cell('y') else cell(' '))
    val next     = TerminalFrame(12, 1, Vector(changed))

    val ansi = TerminalAnsiDiff.emit(Some(previous), next)

    ansi shouldBe s"$esc[1;3H${fullSgr(Color.WHITE, Color.BLACK)}x  y$esc[0m"
    replayed(previous, ansi) shouldBe next
  }

  it should "still be skipped when rewriting the gap would need more bytes than moving over it" in {
    val redGap   = cell('z', fg = Color.RED)
    val previous = TerminalFrame(6, 1, Vector(Vector.tabulate(6)(x => if x == 3 then redGap else cell(' '))))
    val next =
      TerminalFrame(
        6,
        1,
        Vector(Vector.tabulate(6)(x => if x == 2 then cell('x') else if x == 4 then cell('y') else previous(x, 0)))
      )

    val ansi = TerminalAnsiDiff.emit(Some(previous), next)

    ansi shouldBe s"$esc[1;3H${fullSgr(Color.WHITE, Color.BLACK)}x$esc[Cy$esc[0m"
    replayed(previous, ansi) shouldBe next
  }

  "a distant changed cell on the same row" should "be reached with a relative or column move rather than a second CUP" in {
    val previous = TerminalFrame.blank(30, 1)
    val next = TerminalFrame(
      30,
      1,
      Vector(Vector.tabulate(30)(x => if x == 1 then cell('x') else if x == 20 then cell('y') else cell(' ')))
    )

    val ansi = TerminalAnsiDiff.emit(Some(previous), next)

    ansi shouldBe s"$esc[1;2H${fullSgr(Color.WHITE, Color.BLACK)}x$esc[21Gy$esc[0m"
    replayed(previous, ansi) shouldBe next
  }

  "a changed cell at the start of the next row" should "be reached with a carriage return and line feed" in {
    val previous = TerminalFrame.blank(3, 3)
    val next =
      withRow(withRow(previous, 0, Vector(cell('a'), cell(' '), cell(' '))), 1, Vector(cell('b'), cell(' '), cell(' ')))

    val ansi = TerminalAnsiDiff.emit(Some(previous), next)

    ansi shouldBe s"$esc[1;1H${fullSgr(Color.WHITE, Color.BLACK)}a\r\nb$esc[0m"
    replayed(previous, ansi) shouldBe next
  }

  "a full repaint" should "move between rows without absolute positioning" in {
    val next = frameOfLines(10, numberedLines(4, 0).map(_.take(10)))

    val ansi = TerminalAnsiDiff.emit(None, next)

    ansi should not include s"$esc[2;1H"
    replayed(TerminalFrame.blank(10, 4, Color.RED, Color.BLUE), ansi) shouldBe next
  }

  // -- scroll regions -------------------------------------------------------------------------------------------------

  "a full-screen scroll up by one line" should "ask the terminal to scroll and repaint only the exposed row" in {
    val previous = frameOfLines(Width, numberedLines(Height, 0))
    val next     = frameOfLines(Width, numberedLines(Height, 1))

    val ansi = TerminalAnsiDiff.emit(Some(previous), next)

    ansi should include(s"$esc[1S")
    ansi should include("line 012")
    ansi should not include "line 005"
    replayed(previous, ansi) shouldBe next
  }

  it should "give the same result when the caller's dirty-row hint names every row" in {
    val previous = frameOfLines(Width, numberedLines(Height, 0))
    val next     = frameOfLines(Width, numberedLines(Height, 1))

    val ansi = TerminalAnsiDiff.emit(Some(previous), next, dirtyRows = Some((0 until Height).toSet))

    ansi should include(s"$esc[1S")
    replayed(previous, ansi) shouldBe next
  }

  "a scroll of the rows between a fixed header and status line" should "set a region around them and reset it" in {
    def screen(first: Int) =
      frameOfLines(Width, "HEADER" +: numberedLines(Height - 2, first) :+ "STATUS")
    val previous = screen(0)
    val next     = screen(1)

    val ansi = TerminalAnsiDiff.emit(Some(previous), next)

    ansi should include(s"$esc[2;11r$esc[1S$esc[r")
    ansi should not include "HEADER"
    ansi should not include "STATUS"
    replayed(previous, ansi) shouldBe next
  }

  "a scroll down by two lines" should "use SD and repaint the two rows it exposes" in {
    val previous = frameOfLines(Width, numberedLines(Height, 5))
    val next     = frameOfLines(Width, numberedLines(Height, 3))

    val ansi = TerminalAnsiDiff.emit(Some(previous), next)

    ansi should include(s"$esc[2T")
    ansi should include("line 003")
    ansi should include("line 004")
    ansi should not include "line 007"
    replayed(previous, ansi) shouldBe next
  }

  "a terminal without scroll region support" should "get the same screen from rewritten rows" in {
    val previous = frameOfLines(Width, numberedLines(Height, 0))
    val next     = frameOfLines(Width, numberedLines(Height, 1))

    val ansi = TerminalAnsiDiff.emit(Some(previous), next, scrollRegions = false)

    ansi should not include s"$esc[1S"
    ansi.length should be > TerminalAnsiDiff.emit(Some(previous), next).length
    replayed(previous, ansi) shouldBe next
  }

  "a change that is not a scroll" should "not use a scroll region" in {
    val previous = frameOfLines(Width, numberedLines(Height, 0))
    val next     = frameOfLines(Width, numberedLines(Height, 100))

    val ansi = TerminalAnsiDiff.emit(Some(previous), next)

    ansi should not include regex(s"$esc\\[[0-9;]*[rST]")
    replayed(previous, ansi) shouldBe next
  }

  // -- bytes per frame: A (LegacyAnsiDiff) against B (TerminalAnsiDiff) -----------------------------------------------

  "a typing frame" should "cost only the cursor move and the glyph when the terminal already holds the cell's attributes" in {
    val previous = frameOfLines(80, numberedLines(24, 0))
    val next     = withRow(previous, 11, previous.cells(11).updated(40, cell('x')))
    val held     = Some(SgrState.of(cell('x')))

    val encoded = TerminalAnsiDiff.encode(Some(previous), next, Some(Set(11)), scrollRegions = true, startPen = held)

    encoded.ansi shouldBe s"$esc[12;41Hx"
    val legacy = LegacyAnsiDiff.emit(Some(previous), next, Some(Set(11)))
    (encoded.ansi.length * 4) should be < legacy.length
  }

  it should "cost no more bytes than the legacy encoder even when nothing is known about the terminal's attributes" in {
    val previous = frameOfLines(80, numberedLines(24, 0))
    val next     = withRow(previous, 11, previous.cells(11).updated(40, cell('x')))

    TerminalAnsiDiff.emit(Some(previous), next).length should be <= LegacyAnsiDiff.emit(Some(previous), next).length
  }

  "a scroll frame" should "cost a fraction of the legacy encoder's bytes" in {
    val previous = frameOfLines(80, numberedLines(24, 0))
    val next     = frameOfLines(80, numberedLines(24, 1))

    val legacy = LegacyAnsiDiff.emit(Some(previous), next)
    val scroll = TerminalAnsiDiff.emit(Some(previous), next)

    (scroll.length * 10) should be < legacy.length
    replayed(previous, scroll) shouldBe replayed(previous, legacy)
  }

  it should "still cost fewer bytes than the legacy encoder when the terminal cannot scroll a region" in {
    val previous = frameOfLines(80, numberedLines(24, 0))
    val next     = frameOfLines(80, numberedLines(24, 1))

    val rewritten = TerminalAnsiDiff.emit(Some(previous), next, scrollRegions = false)

    rewritten.length should be < LegacyAnsiDiff.emit(Some(previous), next).length
  }

  // -- the render surface, end to end ---------------------------------------------------------------------------------

  final private case class SurfaceRun(secondFlushBytes: Int, screen: TerminalEmulator)

  private def paintLines(surface: TerminalRenderSurface, lines: Seq[String], width: Int): Unit =
    lines.zipWithIndex.foreach((line, y) => surface.putString(0, y, line.padTo(width, ' ')))

  private def runSurface(scrollRegions: Boolean, first: Seq[String], second: Seq[String]): SurfaceRun =
    val writer = new StringWriter()
    val surface =
      new TerminalRenderSurface(80, 24, writer, CellMetrics(charWidth = 1, lineHeight = 1, ascent = 0), scrollRegions)
    paintLines(surface, first, 80)
    surface.flush()
    val afterFirst = writer.getBuffer.length
    paintLines(surface, second, 80)
    surface.flush()
    SurfaceRun(writer.getBuffer.length - afterFirst, TerminalEmulator.blank(80, 24).consume(writer.toString))

  "a typing flush through the render surface" should "stay under 40 bytes, sync brackets and caret included" in {
    val lines = numberedLines(24, 0)
    val typed = lines.updated(11, withChar(lines(11), 40, 'x', 80))

    val run = runSurface(scrollRegions = true, lines, typed)

    run.secondFlushBytes should be <= 40
    run.screen.rowText(11) shouldBe typed(11).padTo(80, ' ')
  }

  "a scroll flush through the render surface" should "stay under 150 bytes on a terminal with scroll regions" in {
    val run = runSurface(scrollRegions = true, numberedLines(24, 0), numberedLines(24, 1))

    run.secondFlushBytes should be <= 150
  }

  it should "show the same screen whether the terminal scrolled a region or the rows were rewritten" in {
    val first    = numberedLines(24, 0)
    val second   = numberedLines(24, 1)
    val scrolled = runSurface(scrollRegions = true, first, second)
    val repaint  = runSurface(scrollRegions = false, first, second)

    scrolled.screen.rows shouldBe repaint.screen.rows
    scrolled.screen.rows shouldBe second.map(_.padTo(80, ' ')).toVector
    scrolled.secondFlushBytes should be < repaint.secondFlushBytes
  }

  it should "leave scroll regions unused unless the terminal is known to support them" in {
    val writer  = new StringWriter()
    val surface = new TerminalRenderSurface(80, 24, writer, CellMetrics(charWidth = 1, lineHeight = 1, ascent = 0))
    paintLines(surface, numberedLines(24, 0), 80)
    surface.flush()
    paintLines(surface, numberedLines(24, 1), 80)
    surface.flush()

    writer.toString should not include s"$esc[1S"
  }
