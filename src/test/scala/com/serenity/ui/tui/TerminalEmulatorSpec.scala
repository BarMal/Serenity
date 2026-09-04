package com.serenity.ui.tui

import java.awt.Color

import com.serenity.ui.theme.TextStyle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers the test-only mock terminal every TUI behaviour spec asserts through: the escape vocabulary
  * [[TerminalAnsiDiff]], [[TerminalRenderSurface]] and [[TerminalShell]] actually emit, interpreted back into a cell
  * grid plus the terminal-side state (caret, cursor visibility and shape, alternate screen, DEC private modes, OSC 52
  * clipboard writes) that a raw byte-substring assertion cannot express.
  */
class TerminalEmulatorSpec extends AnyFlatSpec with Matchers:

  private val esc = 0x1b.toChar.toString
  private val bel = 0x07.toChar.toString

  private def emulator(width: Int = 8, height: Int = 3): TerminalEmulator =
    TerminalEmulator.blank(width, height)

  private def cup(row: Int, col: Int): String = s"$esc[$row;${col}H"

  "printable text" should "land at the cursor position the last CUP set" in {
    val result = emulator().consume(cup(2, 3) + "hi")

    result.rowText(1) shouldBe "  hi    "
    result.rowText(0) shouldBe "        "
  }

  it should "advance the cursor one column per narrow glyph" in {
    val result = emulator().consume(cup(1, 1) + "abc")

    result.cursor.col shouldBe 3
    result.cursor.row shouldBe 0
  }

  it should "wrap to the next row when it runs past the last column" in {
    val result = emulator(width = 4, height = 2).consume(cup(1, 3) + "abcd")

    result.rowText(0) shouldBe "  ab"
    result.rowText(1) shouldBe "cd  "
  }

  "CUP" should "home the cursor when its parameters are omitted" in {
    val result = emulator().consume(cup(3, 5) + s"$esc[H" + "x")

    result.rowText(0) should startWith("x")
  }

  "a printed glyph" should "carry the colours and style the active SGR run set" in {
    val result = emulator().consume(
      s"$esc[1;4;38;2;10;20;30;48;2;40;50;60m" + cup(1, 1) + "x"
    )

    val cell = result.cellAt(0, 0)
    cell.text shouldBe "x"
    cell.fg shouldBe new Color(10, 20, 30)
    cell.bg shouldBe new Color(40, 50, 60)
    cell.style shouldBe TextStyle(isBold = true, isUnderlined = true)
  }

  it should "read SGR 49 back as the alpha-0 native-background sentinel TerminalAnsiDiff emits it for" in {
    val result = emulator().consume(s"$esc[0;38;2;1;2;3;49m" + cup(1, 1) + "x")

    result.cellAt(0, 0).bg.getAlpha shouldBe 0
  }

  it should "reset every attribute on SGR 0" in {
    val result = emulator().consume(s"$esc[1;3;4m" + s"$esc[0m" + cup(1, 1) + "x")

    result.cellAt(0, 0).style shouldBe TextStyle.normal
  }

  "ED 2" should "clear the whole grid to the active background" in {
    val painted = emulator().consume(cup(1, 1) + "abc")
    val cleared = painted.consume(s"$esc[48;2;9;9;9m" + s"$esc[2J")

    cleared.rows.foreach(_ shouldBe "        ")
    cleared.cellAt(0, 0).bg shouldBe new Color(9, 9, 9)
  }

  "ED 0 and ED 1" should "clear forwards from and backwards to the cursor" in {
    val painted = emulator(width = 4, height = 2).consume(cup(1, 1) + "abcd" + cup(2, 1) + "efgh")

    // Both are inclusive of the cursor's own cell, per ECMA-48.
    painted.consume(cup(1, 3) + s"$esc[0J").rows shouldBe Vector("ab  ", "    ")
    painted.consume(cup(2, 3) + s"$esc[1J").rows shouldBe Vector("    ", "   h")
  }

  "EL" should "clear from the cursor to the end of its own row only" in {
    val painted = emulator(width = 4, height = 2).consume(cup(1, 1) + "abcd" + cup(2, 1) + "efgh")

    painted.consume(cup(1, 3) + s"$esc[K").rows shouldBe Vector("ab  ", "efgh")
  }

  "DECTCEM" should "track the cursor's visibility across hide and show" in {
    emulator().consume(s"$esc[?25l").cursor.visible shouldBe false
    emulator().consume(s"$esc[?25l$esc[?25h").cursor.visible shouldBe true
  }

  "DECSCUSR" should "record the caret shape the surface asked the terminal for" in {
    emulator().consume(s"$esc[5 q").cursor.shape shouldBe Some(5)
    emulator().consume(s"$esc[2 q").cursor.shape shouldBe Some(2)
  }

  "the alternate screen" should "be entered and left by DEC private mode 1049" in {
    val entered = emulator().consume(s"$esc[?1049h")
    entered.inAlternateScreen shouldBe true
    entered.consume(s"$esc[?1049l").inAlternateScreen shouldBe false
  }

  "DEC private modes" should "record the input modes the TUI input handler and shell enable" in {
    val result = emulator().consume(s"$esc[?1002h$esc[?1003h$esc[?1006h$esc[?2004h$esc[?1004h")

    result.mouseTrackingEnabled shouldBe true
    result.bracketedPasteEnabled shouldBe true
    result.focusReportingEnabled shouldBe true

    val disabled = result.consume(s"$esc[?2004l$esc[?1006l$esc[?1003l$esc[?1002l$esc[?1004l")
    disabled.mouseTrackingEnabled shouldBe false
    disabled.bracketedPasteEnabled shouldBe false
    disabled.focusReportingEnabled shouldBe false
  }

  "DEC 2026 synchronized-update brackets" should "be absorbed without disturbing the content between them" in {
    val result = emulator().consume(s"$esc[?2026h" + cup(1, 1) + "ok" + s"$esc[?2026l")

    result.rowText(0) shouldBe "ok      "
  }

  "the keyboard-protocol negotiation sequences" should "be absorbed rather than printed or misread as SGR" in {
    val result = emulator().consume(s"$esc[?u$esc[>3u$esc[<u$esc[>4;2m$esc[>4;1f$esc[?4m$esc[?4g" + cup(1, 1) + "x")

    result.rowText(0) shouldBe "x       "
    result.cellAt(0, 0).style shouldBe TextStyle.normal
  }

  "OSC 52" should "capture the clipboard payload the terminal was asked to set" in {
    val sequence = com.serenity.input.Osc52.encode("copied text").getOrElse(fail("expected an encodable payload"))

    emulator().consume(sequence).osc52Payloads shouldBe Vector("copied text")
  }

  it should "accept a String-terminated payload as well as a BEL-terminated one" in {
    val payload =
      java.util.Base64.getEncoder.encodeToString("st form".getBytes(java.nio.charset.StandardCharsets.UTF_8))

    emulator().consume(s"$esc]52;c;$payload$esc\\").osc52Payloads shouldBe Vector("st form")
  }

  "a wide glyph" should "occupy its leading cell and reserve the column to its right" in {
    val result = emulator().consume(cup(1, 1) + "漢x")

    result.cellAt(0, 0).span shouldBe CellSpan.Wide
    result.cellAt(0, 0).text shouldBe "漢"
    result.cellAt(1, 0).span shouldBe CellSpan.Continuation
    result.cellAt(2, 0).text shouldBe "x"
  }

  "carriage return, line feed and backspace" should "move the cursor the way a real terminal moves it" in {
    val result = emulator().consume(cup(1, 1) + "abc\r\nd" + 0x08.toChar.toString + "e")

    result.rowText(0) shouldBe "abc     "
    result.rowText(1) shouldBe "e       "
  }

  "a frame emitted by TerminalAnsiDiff" should "replay onto the exact cells it was diffed from" in {
    val buffer = new TerminalScreenBuffer(8, 3)
    buffer.setForegroundColor(Color.WHITE)
    buffer.setBackgroundColor(Color.BLACK)
    buffer.putString(0, 0, "hello")
    buffer.putString(2, 2, "xy")
    val frame = buffer.snapshot

    val replayed = emulator().consume(TerminalAnsiDiff.emit(None, frame))

    replayed.frame shouldBe frame
  }

  "rows and find" should "expose the grid as text for content assertions" in {
    val result = emulator(width = 12).consume(cup(2, 4) + "needle")

    result.rows should have size 3
    result.find("needle") shouldBe Some((3, 1))
    result.rowsContaining("needle") shouldBe Vector(1)
    result.find("absent") shouldBe None
  }

  "render" should "dump the grid with row numbers and the caret marked, for failure diagnostics" in {
    val dump = emulator(width = 4, height = 2).consume(cup(1, 1) + "ab" + s"$esc[?25h").render

    dump should include("ab")
    dump should include("0")
    dump should include("caret")
  }
end TerminalEmulatorSpec
