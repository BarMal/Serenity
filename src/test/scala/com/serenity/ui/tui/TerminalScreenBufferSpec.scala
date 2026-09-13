package com.serenity.ui.tui

import java.awt.Color

import com.serenity.ui.theme.TextStyle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TerminalScreenBufferSpec extends AnyFlatSpec with Matchers:

  private def buffer(width: Int = 10, height: Int = 5): TerminalScreenBuffer =
    new TerminalScreenBuffer(width, height)

  "putString" should "write each character as a narrow cell with the active colours and style" in {
    val buf = buffer()
    buf.setForegroundColor(Color.RED)
    buf.setBackgroundColor(Color.BLUE)
    buf.putString(1, 2, "hi")

    val frame = buf.snapshot
    frame(1, 2) shouldBe TerminalCell('h'.toInt, Color.RED, Color.BLUE, TextStyle.normal, CellSpan.Narrow)
    frame(2, 2) shouldBe TerminalCell('i'.toInt, Color.RED, Color.BLUE, TextStyle.normal, CellSpan.Narrow)
  }

  it should "reserve the following column as a continuation cell for a wide glyph" in {
    val buf = buffer()
    buf.putString(0, 0, "中")

    val frame = buf.snapshot
    frame(0, 0).span shouldBe CellSpan.Wide
    frame(0, 0).codePoint shouldBe "中".codePointAt(0)
    frame(1, 0).span shouldBe CellSpan.Continuation
  }

  it should "advance two columns past a wide glyph before writing the next character" in {
    val buf = buffer()
    buf.putString(0, 0, "中x")

    buf.snapshot(2, 0).text shouldBe "x"
  }

  "enableStyle/disableStyle" should "layer and unwind independently per attribute" in {
    val buf = buffer()
    buf.enableStyle(TextStyle.bold)
    buf.enableStyle(TextStyle.italic)
    buf.putString(0, 0, "a")
    buf.disableStyle(TextStyle.bold)
    buf.putString(1, 0, "b")

    val frame = buf.snapshot
    frame(0, 0).style shouldBe TextStyle(isBold = true, isItalic = true)
    frame(1, 0).style shouldBe TextStyle(isBold = false, isItalic = true)
  }

  "fillRect" should "fill every cell in the rectangle with the given character" in {
    val buf = buffer()
    buf.fillRect(1, 1, 2, 2, 'x')

    val frame = buf.snapshot
    for
      y <- 1 to 2
      x <- 1 to 2
    do frame(x, y).text shouldBe "x"
    frame(0, 0).text shouldBe " "
  }

  "withClip" should "drop writes outside the clip rectangle" in {
    val buf = buffer()
    buf.withClip(0, 0, 2, 2) {
      buf.fillRect(0, 0, 5, 5, 'x')
    }

    val frame = buf.snapshot
    frame(1, 1).text shouldBe "x"
    frame(2, 2).text shouldBe " "
    frame(4, 4).text shouldBe " "
  }

  it should "intersect with an already-active clip rather than replacing it" in {
    val buf = buffer()
    buf.withClip(0, 0, 3, 3) {
      buf.withClip(1, 1, 10, 10) {
        buf.fillRect(0, 0, 5, 5, 'x')
      }
    }

    val frame = buf.snapshot
    frame(1, 1).text shouldBe "x"
    frame(0, 0).text shouldBe " "
    frame(3, 3).text shouldBe " "
  }

  "writing over half of a wide glyph" should "blank the orphaned continuation cell" in {
    val buf = buffer()
    buf.putString(0, 0, "中")
    buf.putString(0, 0, "a")

    val frame = buf.snapshot
    frame(0, 0).text shouldBe "a"
    frame(1, 0) shouldBe TerminalCell.blank(Color.WHITE, Color.BLACK)
  }

  it should "blank the orphaned leader cell when the continuation half is overwritten" in {
    val buf = buffer()
    buf.putString(0, 0, "中")
    buf.putString(1, 0, "a")

    val frame = buf.snapshot
    frame(0, 0) shouldBe TerminalCell.blank(Color.WHITE, Color.BLACK)
    frame(1, 0).text shouldBe "a"
  }

  // The orphan repair (`blankIfContinuation`/`blankIfWideLeader`) funnels through `writeCell` itself rather than
  // writing `grid` directly, so it is subject to the same active clip as every other write -- these two cases are
  // the only way to observe that structurally, since both helpers are private.
  it should "honor the active clip when repairing an orphaned wide-glyph continuation cell" in {
    val buf = buffer()
    buf.putString(0, 0, "中") // leader at (0,0), continuation at (1,0)

    buf.withClip(0, 0, 1, 1) {
      buf.putString(0, 0, "a") // overwrites the leader; the orphaned continuation at (1,0) is outside this clip
    }

    val frame = buf.snapshot
    frame(0, 0).text shouldBe "a"
    frame(1, 0).span shouldBe CellSpan.Continuation // left alone: repair must respect the clip too
  }

  it should "honor the active clip when repairing an orphaned wide-glyph leader cell" in {
    val buf = buffer()
    buf.putString(0, 0, "中") // leader at (0,0), continuation at (1,0)

    buf.withClip(1, 0, 1, 1) {
      buf.putString(1, 0, "a") // overwrites the continuation; the orphaned leader at (0,0) is outside this clip
    }

    val frame = buf.snapshot
    frame(1, 0).text shouldBe "a"
    frame(0, 0).span shouldBe CellSpan.Wide // left alone: repair must respect the clip too
  }

  // #1464: TerminalAnsiDiff.emitDiff rescanned every cell of every row to find what changed, duplicating a diff the
  // renderer's own dirty-row bookkeeping (FramePlan/DirtyLineDiff) already does -- but that bookkeeping only covers
  // editor pane content, not chrome/panels/modal, so it can't safely stand in as the dirty-row source for the whole
  // screen. Every paint path (putString/fillRect, and the wide-glyph orphan repair) funnels through `writeCell`, so
  // tracking rows actually written with a different value there gives a complete, always-correct dirty-row set.
  "consumeDirtyRows" should "report only the rows a write actually changed, then reset" in {
    val buf = buffer()
    buf.putString(0, 2, "hi")

    buf.consumeDirtyRows() shouldBe Set(2)
    buf.consumeDirtyRows() shouldBe Set.empty
  }

  it should "not mark a row dirty when a write leaves its cells unchanged" in {
    val buf = buffer()
    buf.putString(0, 1, "x")
    val _ = buf.consumeDirtyRows() // drain the write above

    buf.putString(0, 1, "x") // identical content, same colours/style

    buf.consumeDirtyRows() shouldBe Set.empty
  }

  it should "accumulate every row touched since the last call, across multiple writes" in {
    val buf = buffer()
    buf.putString(0, 0, "a")
    buf.fillRect(2, 3, 1, 1, 'x')

    buf.consumeDirtyRows() shouldBe Set(0, 3)
  }

  it should "mark a row dirty when overwriting one half of a wide glyph blanks its orphaned other half" in {
    val buf = buffer()
    buf.putString(0, 4, "中")
    val _ = buf.consumeDirtyRows()

    buf.putString(0, 4, "a") // orphans the continuation cell at column 1, blanked via a write outside putString's loop

    buf.consumeDirtyRows() shouldBe Set(4)
  }
