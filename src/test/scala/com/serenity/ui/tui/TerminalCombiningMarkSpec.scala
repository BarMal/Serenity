package com.serenity.ui.tui

import java.awt.Color

import com.serenity.ui.theme.TextStyle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1866 (PR A2): a combining mark is merged into the cell of its base, which a terminal then draws as one glyph, and
  * the ANSI that repaints the cell writes the mark straight after its base.
  */
class TerminalCombiningMarkSpec extends AnyFlatSpec with Matchers:

  private val acute      = "́"
  private val circumflex = "̂"
  private val esc        = 0x1b.toChar.toString

  private def buffer(width: Int = 10, height: Int = 3): TerminalScreenBuffer = new TerminalScreenBuffer(width, height)

  "putString" should "merge a combining mark into the cell of the character before it" in {
    val buf = buffer()
    buf.putString(0, 0, s"e${acute}x")

    val frame = buf.snapshot
    frame(0, 0).codePoint shouldBe 'e'.toInt
    frame(0, 0).text shouldBe s"e$acute"
    frame(0, 0).span shouldBe CellSpan.Narrow
    frame(1, 0).text shouldBe "x"
  }

  it should "stack several marks on the one base, in order" in {
    val buf = buffer()
    buf.putString(0, 0, s"e$acute${circumflex}x")

    buf.snapshot(0, 0).text shouldBe s"e$acute$circumflex"
    buf.snapshot(1, 0).text shouldBe "x"
  }

  it should "merge a mark after a wide character into the wide character's leading cell" in {
    val buf = buffer()
    buf.putString(0, 0, s"中${acute}x")

    val frame = buf.snapshot
    frame(0, 0).text shouldBe s"中$acute"
    frame(0, 0).span shouldBe CellSpan.Wide
    frame(1, 0).span shouldBe CellSpan.Continuation
    frame(2, 0).text shouldBe "x"
  }

  it should "leave a mark with no base before it out, and not borrow the cell to its left" in {
    val buf = buffer()
    buf.putString(0, 0, "z")
    buf.putString(1, 0, s"${acute}a")

    val frame = buf.snapshot
    frame(0, 0).text shouldBe "z"
    frame(1, 0).text shouldBe "a"
  }

  it should "not attach a mark to a cell an earlier call wrote" in {
    val buf = buffer()
    buf.putString(0, 0, "e")
    buf.putString(1, 0, acute)

    val frame = buf.snapshot
    frame(0, 0).text shouldBe "e"
    frame(1, 0).text shouldBe " "
  }

  it should "drop the marks of a cell when something else is written over it" in {
    val buf = buffer()
    buf.putString(0, 0, s"e$acute")
    buf.putString(0, 0, "x")

    buf.snapshot(0, 0).text shouldBe "x"
  }

  it should "not write a mark outside the active clip" in {
    val buf = buffer()
    buf.withClip(1, 0, 3, 1) {
      buf.putString(0, 0, s"e${acute}x")
    }

    buf.snapshot(0, 0).text shouldBe " "
  }

  "flushDiff" should "write the mark straight after its base, then address the next cell absolutely" in {
    val buf = buffer(width = 4, height = 1)
    buf.putString(0, 0, s"e${acute}xy")

    val encoded = buf.flushDiff(scrollRegions = false, startPen = None)
    encoded.ansi should include(s"e$acute")
    // #2365: after a cluster of more than one codepoint the cursor is addressed absolutely, not assumed.
    encoded.ansi should include(s"$esc[1;2Hxy")
  }

  "TerminalAnsiDiff.emit" should "repaint a cell whose only change is a mark" in {
    def cell(marks: String): TerminalCell =
      TerminalCell('e'.toInt, Color.WHITE, Color.BLACK, TextStyle.normal, CellSpan.Narrow, marks)
    val before = TerminalFrame(1, 1, Vector(Vector(cell(""))))
    val after  = TerminalFrame(1, 1, Vector(Vector(cell(acute))))

    TerminalAnsiDiff.emit(Some(before), after) should include(s"e$acute")
    TerminalAnsiDiff.emit(Some(after), after) shouldBe ""
  }

  "A frame round trip" should "show the same marks once the ANSI has been interpreted" in {
    val buf = buffer(width = 6, height = 1)
    buf.putString(0, 0, s"e${acute}中${circumflex}x")

    val ansi  = buf.flushDiff(scrollRegions = false, startPen = None).ansi
    val shown = TerminalEmulator.blank(6, 1).consume(ansi).frame
    shown(0, 0).text shouldBe s"e$acute"
    shown(1, 0).text shouldBe s"中$circumflex"
    shown(1, 0).span shouldBe CellSpan.Wide
    shown(3, 0).text shouldBe "x"
  }
