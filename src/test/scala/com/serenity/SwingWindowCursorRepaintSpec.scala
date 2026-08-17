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
