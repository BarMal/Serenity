package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PaneWidthConstraintRegressionSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  behavior of "Pane width constraints"

  it should "keep panes narrower than the minimum off screen while preserving their layout entries" in {
    val panes = (0 until 6).map(index => PaneId(index) -> EditorPane.empty(PaneId(index))).toMap
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        layout = Layout(
          editorPanes = panes,
          activeEditorPaneId = Some(PaneId(0)),
          paneOrder = panes.keys.toList.sortBy(_.value)
        ),
        focus = Focus.EditorPane(PaneId(0))
      )
    )
    val viewportSize = ViewportSize(80, 24)
    val layout       = LayoutEngine.calculateLayout(state, viewportSize)

    val defaultLayouts = LayoutEngine.calculatePaneLayouts(state, layout)
    val visibleDefault = visiblePaneCount(defaultLayouts, layout.editorPanelRect)

    val oneCellMinimumLayouts     = LayoutEngine.calculatePaneLayoutsWithMinWidth(state, layout, minWidth = 1)
    val visibleWithOneCellMinimum = visiblePaneCount(oneCellMinimumLayouts, layout.editorPanelRect)

    defaultLayouts.should(have).size(panes.size)
    visibleDefault shouldBe 1
    oneCellMinimumLayouts.should(have).size(panes.size)
    visibleWithOneCellMinimum shouldBe panes.size
  }

  private def visiblePaneCount(layouts: Map[PaneId, LayoutRect], editorRect: LayoutRect): Int =
    layouts.values.count(rect => rect.x >= editorRect.x && rect.x < editorRect.right)
