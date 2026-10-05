package com.serenity

import java.awt.Rectangle

import com.serenity.ui.terminal.SwingWindow
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The cursor overlay is redrawn from scratch every frame, so bounding its repaint safely requires covering both where
  * a cursor used to be and where it is now -- not just the base frame's own dirty region. These cases pin down that
  * unioning logic, since #963 found no test in the repo exercising SwingWindow's repaint-region plumbing.
  */
class SwingWindowCursorRepaintSpec extends AnyFlatSpec with Matchers:

  "combinedCursorRepaintRegion" should "fall back to a full repaint when the base frame itself is unbounded" in {
    SwingWindow.combinedCursorRepaintRegion(
      baseDirtyRegion = None,
      previousCursorRects = List(new Rectangle(10, 10, 5, 16)),
      currentCursorRects = List(new Rectangle(20, 10, 5, 16))
    ) shouldBe None
  }

  it should "report an empty region when nothing changed" in {
    SwingWindow.combinedCursorRepaintRegion(
      baseDirtyRegion = Some(new Rectangle(0, 0, 0, 0)),
      previousCursorRects = Nil,
      currentCursorRects = Nil
    ) shouldBe Some(new Rectangle(0, 0, 0, 0))
  }

  it should "cover both the old and new caret position when only the cursor moved" in {
    val previous = new Rectangle(10, 10, 5, 16)
    val current  = new Rectangle(40, 10, 5, 16)

    val region = SwingWindow.combinedCursorRepaintRegion(
      baseDirtyRegion = Some(new Rectangle(0, 0, 0, 0)),
      previousCursorRects = List(previous),
      currentCursorRects = List(current)
    )

    region shouldBe Some(previous.union(current))
  }

  it should "union the base frame's dirty region with the caret's old and new positions" in {
    val baseRegion = new Rectangle(0, 32, 400, 16)
    val previous   = new Rectangle(10, 200, 5, 16)
    val current    = new Rectangle(10, 216, 5, 16)

    val region = SwingWindow.combinedCursorRepaintRegion(
      baseDirtyRegion = Some(baseRegion),
      previousCursorRects = List(previous),
      currentCursorRects = List(current)
    )

    region shouldBe Some(baseRegion.union(previous).union(current))
  }

  it should "still repaint the caret's old position when the cursor becomes invisible" in {
    val previous = new Rectangle(10, 10, 5, 16)

    val region = SwingWindow.combinedCursorRepaintRegion(
      baseDirtyRegion = Some(new Rectangle(0, 0, 0, 0)),
      previousCursorRects = List(previous),
      currentCursorRects = Nil
    )

    region shouldBe Some(previous)
  }

  it should "union every cursor in a multi-cursor edit" in {
    val a = new Rectangle(10, 10, 5, 16)
    val b = new Rectangle(10, 42, 5, 16)
    val c = new Rectangle(10, 74, 5, 16)

    val region = SwingWindow.combinedCursorRepaintRegion(
      baseDirtyRegion = Some(new Rectangle(0, 0, 0, 0)),
      previousCursorRects = List(a),
      currentCursorRects = List(a, b, c)
    )

    region shouldBe Some(a.union(b).union(c))
  }

  it should "not let a zero-size sentinel rectangle drag the union back to the origin" in {
    val current = new Rectangle(200, 300, 5, 16)

    val region = SwingWindow.combinedCursorRepaintRegion(
      baseDirtyRegion = Some(new Rectangle(0, 0, 0, 0)),
      previousCursorRects = Nil,
      currentCursorRects = List(current)
    )

    region shouldBe Some(current)
  }

  "cursorRepaint" should "keep a caret far from the base frame's rects as a rect of its own" in {
    val caretRow  = new Rectangle(0, 48, 400, 16)
    val statusRow = new Rectangle(0, 624, 960, 16)
    val caret     = new Rectangle(10, 48, 2, 16)

    SwingWindow.cursorRepaint(Some(List(caretRow, statusRow)), List(caret), List(caret)) shouldBe
      SwingWindow.CanvasRepaint.Rects(List(caretRow, statusRow))
  }

  it should "repaint a caret's old and new rows apart when it jumps far" in {
    val previous = new Rectangle(10, 16, 2, 16)
    val current  = new Rectangle(10, 400, 2, 16)

    SwingWindow.cursorRepaint(Some(Nil), List(previous), List(current)) shouldBe
      SwingWindow.CanvasRepaint.Rects(List(previous, current))
  }

  it should "repaint the whole canvas when the base frame is unbounded" in {
    SwingWindow.cursorRepaint(None, Nil, List(new Rectangle(10, 10, 2, 16))) shouldBe SwingWindow.CanvasRepaint.Whole
  }

  "mergedRepaint" should "cover both frames' rects when a second frame lands before the canvas paints" in {
    val first  = new Rectangle(0, 48, 400, 16)
    val second = new Rectangle(0, 400, 400, 16)

    SwingWindow.mergedRepaint(
      SwingWindow.CanvasRepaint.Rects(List(first)),
      SwingWindow.CanvasRepaint.Rects(List(second))
    ) shouldBe SwingWindow.CanvasRepaint.Rects(List(first, second))
  }

  it should "repaint the whole canvas when either frame needs it" in {
    val rects = SwingWindow.CanvasRepaint.Rects(List(new Rectangle(0, 48, 400, 16)))

    SwingWindow.mergedRepaint(rects, SwingWindow.CanvasRepaint.Whole) shouldBe SwingWindow.CanvasRepaint.Whole
    SwingWindow.mergedRepaint(SwingWindow.CanvasRepaint.Whole, rects) shouldBe SwingWindow.CanvasRepaint.Whole
  }
