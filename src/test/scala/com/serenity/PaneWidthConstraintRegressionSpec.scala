package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PaneWidthConstraintRegressionSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  behavior of "Pane width constraints"

  // The pre-#821 flat pane-strip engine hid panes narrower than the minimum width entirely off screen (a windowing
  // fallback keyed on focus). The workspace tree, now the sole pane layout path (#814-#821), has no such windowing: a
  // `WorkspaceNode.Split`'s `splitExtent` always keeps every leaf within its parent's rect, clamped rather than
  // hidden, however far short of its minimum width that leaves it. So the current, real guarantee this regression
  // test protects is narrower but still meaningful: every pane keeps a layout entry, and that entry is always a
  // genuine sub-rect of the editor area, never displaced off screen the way the old windowing fallback could leave a
  // stale rect.
  it should "keep every pane's layout entry on screen, however far short of the minimum width it falls" in {
    val paneIds = (0 until 6).map(PaneId.apply).toList
    val panes   = paneIds.map(id => id -> EditorPane.empty(id)).toMap
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        layout = Layout(
          editorPanes = panes,
          activeEditorPaneId = Some(PaneId(0)),
          workspaceTree = Some(linearWorkspaceTree(paneIds))
        ),
        focus = Focus.EditorPane(PaneId(0))
      )
    )
    val viewportSize = ViewportSize(80, 24)
    val layout       = LayoutEngine.calculateLayout(state, viewportSize)

    val defaultLayouts        = LayoutEngine.calculatePaneLayouts(state, layout)
    val oneCellMinimumLayouts = LayoutEngine.calculatePaneLayoutsWithMinWidth(state, layout, minWidth = 1)

    defaultLayouts.should(have).size(panes.size)
    oneCellMinimumLayouts.should(have).size(panes.size)
    allOnScreen(defaultLayouts, layout.editorPanelRect) shouldBe true
    allOnScreen(oneCellMinimumLayouts, layout.editorPanelRect) shouldBe true
  }

  private def allOnScreen(layouts: Map[PaneId, LayoutRect], editorRect: LayoutRect): Boolean =
    layouts.values.forall(rect => rect.x >= editorRect.x && rect.right <= editorRect.right)

  private def linearWorkspaceTree(paneIds: List[PaneId]): WorkspaceTree =
    def leaf(paneId: PaneId): WorkspaceNode = WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)
    def build(paneId: PaneId, remaining: List[PaneId]): WorkspaceNode =
      remaining match
        case Nil => leaf(paneId)
        case next :: tail =>
          WorkspaceNode.Split(
            WorkspaceNodeId(s"split-${paneId.value}-${remaining.map(_.value).mkString("-")}"),
            SplitAxis.Horizontal,
            1.0 / (remaining.size + 1),
            leaf(paneId),
            build(next, tail)
          )
    WorkspaceTree(build(paneIds.head, paneIds.tail))
