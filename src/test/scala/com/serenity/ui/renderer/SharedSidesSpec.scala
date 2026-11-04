package com.serenity.ui.renderer

import com.serenity.state.models.PaneId
import com.serenity.ui.layout.{EditorPaneLayout, LayoutRect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SharedSidesSpec extends AnyFlatSpec with Matchers:

  private def layout(x: Int, y: Int, width: Int, height: Int): EditorPaneLayout =
    val rect = LayoutRect(x, y, width, height)
    EditorPaneLayout(rect, rect, rect, rect)

  private val left       = PaneId(0)
  private val right      = PaneId(1)
  private val belowLeft  = PaneId(2)
  private val belowRight = PaneId(3)

  "A lone pane" should "share no side" in {
    SharedSides.of(left, Map(left -> layout(6, 2, 104, 63))) shouldBe SharedSides.Unshared
  }

  "Two panes side by side" should "share the right side of the left pane and the left side of the right pane" in {
    val layouts = Map(left -> layout(6, 2, 104, 63), right -> layout(110, 2, 104, 63))

    SharedSides.of(left, layouts) shouldBe SharedSides(left = false, right = true)
    SharedSides.of(right, layouts) shouldBe SharedSides(left = true, right = false)
  }

  "Two panes one above the other" should "share no side" in {
    val layouts = Map(left -> layout(6, 2, 208, 30), belowLeft -> layout(6, 32, 208, 30))

    SharedSides.of(left, layouts) shouldBe SharedSides.Unshared
    SharedSides.of(belowLeft, layouts) shouldBe SharedSides.Unshared
  }

  "Two panes with a gap between them" should "share no side" in {
    val layouts = Map(left -> layout(6, 2, 100, 63), right -> layout(112, 2, 100, 63))

    SharedSides.of(left, layouts) shouldBe SharedSides.Unshared
    SharedSides.of(right, layouts) shouldBe SharedSides.Unshared
  }

  "Four panes in two columns of two" should "share the one side that faces the other column" in {
    val layouts = Map(
      left       -> layout(6, 2, 104, 30),
      right      -> layout(110, 2, 104, 30),
      belowLeft  -> layout(6, 32, 104, 30),
      belowRight -> layout(110, 32, 104, 30)
    )

    SharedSides.of(left, layouts) shouldBe SharedSides(left = false, right = true)
    SharedSides.of(belowLeft, layouts) shouldBe SharedSides(left = false, right = true)
    SharedSides.of(right, layouts) shouldBe SharedSides(left = true, right = false)
    SharedSides.of(belowRight, layouts) shouldBe SharedSides(left = true, right = false)
  }

  "A pane whose neighbour sits beside only another row" should "share no side with it" in {
    val layouts = Map(left -> layout(6, 2, 104, 30), belowRight -> layout(110, 32, 104, 30))

    SharedSides.of(left, layouts) shouldBe SharedSides.Unshared
  }
