package com.serenity.ui.tui

import java.awt.Color

import com.serenity.ui.theme.TextStyle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1890: the compact grid behind [[TerminalScreenBuffer]] -- that it holds exactly what the [[TerminalFrame]] value
  * model says, that its row comparisons see every difference a cell comparison would, and that a flush from the buffer
  * brings a terminal to the same screen the buffer holds.
  */
class CellGridSpec extends AnyFlatSpec with Matchers:

  private val styled = TextStyle(isBold = true, fontFamily = Some("Fira Code"), fontSize = Some(13f))

  private def cell(char: Char, fg: Color = Color.WHITE, style: TextStyle = TextStyle.normal): TerminalCell =
    TerminalCell(char.toInt, fg, Color.BLACK, style, CellSpan.Narrow)

  private def frameOf(rows: Vector[Vector[TerminalCell]]): TerminalFrame =
    TerminalFrame(rows.headOption.fold(0)(_.size), rows.size, rows)

  "a grid built from a frame" should "give back that frame, wide cells and every style attribute included" in {
    val wide = TerminalCell(0x6f22, Color.RED, Color.BLUE, styled, CellSpan.Wide)
    val tail = TerminalCell.continuation(Color.RED, Color.BLUE, styled)
    val frame = frameOf(
      Vector(
        Vector(cell('a'), wide, tail, cell('b', Color.GREEN, TextStyle.italic)),
        Vector(cell('z'), cell('c'), cell('d'), cell('e', style = styled))
      )
    )

    CellGrid.fromFrame(frame, new StyleTable).toFrame shouldBe frame
  }

  it should "keep supplementary-plane codepoints intact" in {
    val emoji = TerminalCell(0x1f600, Color.WHITE, Color.BLACK, TextStyle.normal, CellSpan.Wide)
    val frame = frameOf(Vector(Vector(emoji, TerminalCell.continuation(Color.WHITE, Color.BLACK, TextStyle.normal))))

    CellGrid.fromFrame(frame, new StyleTable).toFrame shouldBe frame
  }

  "a style table" should "give equal styles one id and different styles different ids" in {
    val table = new StyleTable
    val first = table.intern(Color.RED, Color.BLACK, TextStyle.normal)

    table.intern(new Color(Color.RED.getRGB), Color.BLACK, TextStyle.normal) shouldBe first
    table.intern(Color.RED, Color.BLACK, TextStyle.bold) should not be first
    table.intern(Color.BLUE, Color.BLACK, TextStyle.normal) should not be first
    table(first).fg shouldBe Color.RED
  }

  "row comparison" should "see a difference in glyph, in colour or in style, and nothing else" in {
    val table    = new StyleTable
    val base     = frameOf(Vector.fill(2)(Vector(cell('a'), cell('b'), cell('c'), cell('d'))))
    val previous = CellGrid.fromFrame(base, table)
    val changed = CellGrid.fromFrame(
      frameOf(
        Vector(
          Vector(cell('a'), cell('X'), cell('c'), cell('d')),
          Vector(cell('a'), cell('b', Color.RED), cell('c'), cell('d', style = TextStyle.bold))
        )
      ),
      table
    )

    changed.rowEquals(0, previous, 0) shouldBe false
    changed.differingColumns(0, previous, 0) shouldBe Seq(1)
    changed.differingColumns(1, previous, 1) shouldBe Seq(1, 3)
    previous.rowEquals(0, previous, 1) shouldBe true
    previous.differingColumns(0, previous, 1) shouldBe empty
    previous.rowHash(0) shouldBe previous.rowHash(1)
    changed.rowHash(0) should not be previous.rowHash(0)
  }

  "fillSpan" should "report a change only when a cell actually differed" in {
    val table = new StyleTable
    val style = table.intern(Color.WHITE, Color.BLACK, TextStyle.normal)
    val grid  = new CellGrid(6, 2, table, style)

    grid.fillSpan(1, 1, 5, CellGrid.Blank, style) shouldBe false
    grid.fillSpan(1, 1, 5, CellGrid.pack('x'.toInt, CellSpan.Narrow), style) shouldBe true
    grid.fillSpan(1, 1, 5, CellGrid.pack('x'.toInt, CellSpan.Narrow), style) shouldBe false
    grid.cellAt(0, 1).codePoint shouldBe ' '.toInt
    grid.cellAt(1, 1).codePoint shouldBe 'x'.toInt
    grid.cellAt(4, 1).codePoint shouldBe 'x'.toInt
    grid.cellAt(5, 1).codePoint shouldBe ' '.toInt
  }

  "a bulk fillRect" should "blank the half of a wide glyph it cuts, on either side, as cell-by-cell writes do" in {
    val buffer = new TerminalScreenBuffer(8, 1)
    buffer.putString(1, 0, "漢字")

    buffer.fillRect(2, 0, 1, 1, '.')
    buffer.snapshot(1, 0).text shouldBe " "
    buffer.snapshot(2, 0).text shouldBe "."
    buffer.snapshot(3, 0).span shouldBe CellSpan.Wide

    buffer.fillRect(3, 0, 1, 1, ',')
    buffer.snapshot(3, 0).text shouldBe ","
    buffer.snapshot(4, 0).span shouldBe CellSpan.Narrow
    buffer.snapshot(4, 0).text shouldBe " "
  }

  it should "stay inside the active clip and the screen" in {
    val buffer = new TerminalScreenBuffer(6, 3)
    buffer.withClip(1, 1, 3, 1)(buffer.fillRect(-2, -2, 20, 20, '#'))

    val rows = buffer.snapshot.cells.map(_.map(_.text).mkString)
    rows shouldBe Vector("      ", " ###  ", "      ")
  }

  "flushDiff" should "keep the terminal equal to the buffer across edits, overwrites of wide glyphs and fills" in {
    val buffer = new TerminalScreenBuffer(10, 4)
    val edits: List[TerminalScreenBuffer => Unit] = List(
      _.putString(0, 0, "hello 漢字"),
      b =>
        b.setForegroundColor(Color.RED)
        b.putString(1, 1, "red")
      ,
      _.putString(7, 0, "x"),
      _.fillRect(0, 2, 10, 2, '-'),
      b =>
        b.setForegroundColor(Color.WHITE)
        b.enableStyle(TextStyle.bold)
        b.putString(2, 3, "bold😀")
      ,
      _.putString(6, 3, "é"),
      _.fillRect(5, 0, 3, 1, ' ')
    )

    val (emulator, _) = edits.foldLeft((TerminalEmulator.blank(10, 4), Option.empty[SgrState])) {
      case ((terminal, pen), edit) =>
        edit(buffer)
        val encoded = buffer.flushDiff(scrollRegions = true, startPen = pen)
        (terminal.consume(encoded.ansi), encoded.endPen)
    }

    emulator.frame shouldBe buffer.snapshot
  }

  it should "emit nothing for a buffer nobody wrote to since the last flush" in {
    val buffer = new TerminalScreenBuffer(10, 4)
    buffer.putString(0, 0, "abc")
    val _ = buffer.flushDiff(scrollRegions = true, startPen = None)

    buffer.flushDiff(scrollRegions = true, startPen = None).ansi shouldBe ""
  }
