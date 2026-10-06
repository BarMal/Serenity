package com.serenity.state.models

import com.serenity.ui.layout.PanelPosition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PointerShapeSpec extends AnyFlatSpec with Matchers:

  "PointerShape.forTarget" should "show the default pointer over inert chrome" in {
    PointerShape.forTarget(PointerHitTarget.Inert) shouldBe PointerShape.Default
  }

  it should "show the text pointer over editor text" in {
    PointerShape.forTarget(PointerHitTarget.EditorText) shouldBe PointerShape.Text
  }

  it should "show the hand pointer over a link, button or row" in {
    PointerShape.forTarget(PointerHitTarget.Control) shouldBe PointerShape.Hand
  }

  it should "resize horizontally across the left and right dock edges" in {
    PointerShape.forTarget(PointerHitTarget.DockEdge(PanelPosition.Left)) shouldBe PointerShape.ResizeHorizontal
    PointerShape.forTarget(PointerHitTarget.DockEdge(PanelPosition.Right)) shouldBe PointerShape.ResizeHorizontal
  }

  it should "resize vertically across the top and bottom dock edges" in {
    PointerShape.forTarget(PointerHitTarget.DockEdge(PanelPosition.Top)) shouldBe PointerShape.ResizeVertical
    PointerShape.forTarget(PointerHitTarget.DockEdge(PanelPosition.Bottom)) shouldBe PointerShape.ResizeVertical
  }

  it should "resize along the same axis a text-area margin drag moves" in
    PanelPosition.values.foreach { side =>
      PointerShape.forTarget(PointerHitTarget.TextAreaMargin(side)) shouldBe
        PointerShape.forTarget(PointerHitTarget.DockEdge(side))
    }
