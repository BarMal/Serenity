package com.serenity.state.models

import com.serenity.animation.{EasingCurve, Tween}
import com.serenity.ui.layout.LayoutRect
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Selection grow/settle (issue #1085 phase 3): [[SelectionGeometryState]] wraps one [[Tween]]`[LayoutRect]` per
  * visual line a selection's highlight touches, keyed by [[SelectionLineKey]] (a visual line's own `bufferLine`/
  * `startColumn`, the same identity `TextVisualLine` itself carries) rather than raw list position -- selection
  * anchor-stability (`Selection`'s `DirectedRange`) means a line's identity never needs to shift as neighbouring lines
  * are added or removed, so keying by identity is simpler than "index from the anchor" bookkeeping while producing the
  * same result for every transition anchor-stability actually permits (resize, append, truncate, create, clear).
  *
  * [[SelectionGeometryState.diff]] is the one place old vs. new per-line rects (each computed against the buffer's own
  * text layout by `state.manager.SelectionGeometry`, not here -- this model has no font/wrap dependency) are turned
  * into the tween list: a line only in the new set grows in from a zero-width sliver at its own start column; a line
  * only in the old set shrinks to a zero-width sliver and is marked for removal; a line in both, with an unchanged
  * rect, needs no tween at all (mirrors `PinnedPanelAnimations.collapsedRect`'s "zero-size synthetic endpoint" shape,
  * generalised from one rect to a managed list).
  */
class SelectionGeometryStateSpec extends AnyFlatSpec with Matchers:

  private val curve = EasingCurve.Linear
  private val steps = 4

  private def key(bufferLine: Int, startColumn: Int): SelectionLineKey = SelectionLineKey(bufferLine, startColumn)

  private def rect(startColumn: Int, width: Int): LayoutRect = LayoutRect(x = startColumn, y = 0, width = width, height = 1)

  "SelectionGeometryState.diff" should "tween only the line whose extent actually changed on a same-line-count resize" in {
    val before = Map(key(0, 0) -> rect(2, 3), key(1, 0) -> rect(0, 2))
    val after  = Map(key(0, 0) -> rect(2, 3), key(1, 0) -> rect(0, 5))

    val geometry = SelectionGeometryState.diff(None, before, after, curve, steps)

    geometry shouldBe defined
    geometry.get.lines.map(_.key) shouldBe List(key(1, 0))
    geometry.get.lines.head.tween.start shouldBe rect(0, 2)
    geometry.get.lines.head.tween.end shouldBe rect(0, 5)
    geometry.get.lines.head.removing shouldBe false
  }

  it should "grow a newly appended line in from a zero-width sliver at its own start column" in {
    val before = Map(key(0, 0) -> rect(2, 3))
    val after  = Map(key(0, 0) -> rect(2, 3), key(1, 0) -> rect(0, 4))

    val geometry = SelectionGeometryState.diff(None, before, after, curve, steps)

    geometry shouldBe defined
    val appended = geometry.get.lines.find(_.key == key(1, 0)).getOrElse(fail("expected the appended line"))
    appended.tween.start shouldBe rect(0, 0)
    appended.tween.end shouldBe rect(0, 4)
    appended.removing shouldBe false
  }

  it should "shrink a truncated line to a zero-width sliver at its own start column and mark it removing" in {
    val before = Map(key(0, 0) -> rect(2, 3), key(1, 0) -> rect(0, 4))
    val after  = Map(key(0, 0) -> rect(2, 3))

    val geometry = SelectionGeometryState.diff(None, before, after, curve, steps)

    geometry shouldBe defined
    val truncated = geometry.get.lines.find(_.key == key(1, 0)).getOrElse(fail("expected the truncated line"))
    truncated.tween.start shouldBe rect(0, 4)
    truncated.tween.end shouldBe rect(0, 0)
    truncated.removing shouldBe true
  }

  it should "grow every line in from nothing when a selection is created fresh" in {
    val after = Map(key(0, 5) -> rect(5, 3), key(1, 0) -> rect(0, 2))

    val geometry = SelectionGeometryState.diff(None, Map.empty, after, curve, steps)

    geometry shouldBe defined
    geometry.get.lines should have length 2
    geometry.get.lines.foreach { line =>
      line.tween.start.width shouldBe 0
      line.removing shouldBe false
    }
  }

  it should "shrink every line to nothing, marked removing, when a selection is cleared entirely" in {
    val before = Map(key(0, 5) -> rect(5, 3), key(1, 0) -> rect(0, 2))

    val geometry = SelectionGeometryState.diff(None, before, Map.empty, curve, steps)

    geometry shouldBe defined
    geometry.get.lines should have length 2
    geometry.get.lines.foreach { line =>
      line.tween.end.width shouldBe 0
      line.removing shouldBe true
    }
  }

  it should "return None when there is nothing to animate, before or after" in {
    SelectionGeometryState.diff(None, Map.empty, Map.empty, curve, steps) shouldBe None
  }

  it should "return None when the selection is unchanged" in {
    val rects = Map(key(0, 0) -> rect(0, 3))
    SelectionGeometryState.diff(None, rects, rects, curve, steps) shouldBe None
  }

  it should "retarget an in-flight tween rather than restarting it from the old rect" in {
    val inFlightTween = Tween(start = rect(0, 0), end = rect(0, 2), curve = curve, steps = 4, currentFrame = 2)
    val existing       = SelectionGeometryState(List(SelectionLineGeometry(key(0, 0), inFlightTween)))
    val before          = Map(key(0, 0) -> rect(0, 2))
    val after           = Map(key(0, 0) -> rect(0, 6))

    val geometry = SelectionGeometryState.diff(Some(existing), before, after, curve, steps)

    geometry shouldBe defined
    val retargeted = geometry.get.lines.head
    retargeted.tween.start shouldBe inFlightTween.currentValue
    retargeted.tween.end shouldBe rect(0, 6)
  }

  "SelectionGeometryState.advance" should "advance every line's tween by one step" in {
    val tween    = Tween(start = rect(0, 0), end = rect(0, 4), curve = curve, steps = steps)
    val state    = SelectionGeometryState(List(SelectionLineGeometry(key(0, 0), tween)))
    val advanced = state.advance

    advanced.lines.head.tween.currentValue shouldBe rect(0, 1)
    advanced.isComplete shouldBe false
  }

  it should "drop a removing line once its shrink completes" in {
    val tween = Tween(start = rect(0, 3), end = rect(0, 0), curve = curve, steps = 1)
    val state = SelectionGeometryState(List(SelectionLineGeometry(key(0, 0), tween, removing = true)))

    val advanced = state.advance

    advanced.lines shouldBe empty
    advanced.isComplete shouldBe true
  }

  it should "keep a non-removing line once its tween completes, reaching its final rect" in {
    val tween = Tween(start = rect(0, 0), end = rect(0, 4), curve = curve, steps = 1)
    val state = SelectionGeometryState(List(SelectionLineGeometry(key(0, 0), tween)))

    val advanced = state.advance

    advanced.isComplete shouldBe true
    advanced.rectFor(0, 0) shouldBe Some(rect(0, 4))
  }

  "SelectionGeometryState.rectFor" should "find a line's current rect by its buffer line and start column" in {
    val tween = Tween(start = rect(0, 0), end = rect(0, 4), curve = curve, steps = 4)
    val state = SelectionGeometryState(List(SelectionLineGeometry(key(2, 7), tween)))

    state.rectFor(2, 7) shouldBe Some(rect(0, 0))
    state.rectFor(2, 8) shouldBe None
    state.rectFor(3, 7) shouldBe None
  }
