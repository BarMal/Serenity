package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent, ViewIntent}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** End-to-end scenario coverage (#821) for the workspace-tree composition contract: the default distraction-free
  * page, nested mixed-axis pane splits, multiple docked panels sharing one edge, and maximise/restore -- the pieces
  * of #812's acceptance criteria that only had unit-level coverage before this (see `WorkspaceTreeSpec`,
  * `PaneWidthConstraintRegressionSpec`, `UIHotkeysAndPanelsSpec`), not a full state-pipeline-plus-renderer pass.
  */
class WorkspaceLayoutUiScenarioSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def overlaps(first: LayoutRect, second: LayoutRect): Boolean =
    first.x < second.right && second.x < first.right && first.y < second.bottom && second.y < first.bottom

  "UiScenarioDriver" should "render the distraction-free default page with one editor leaf and no persistent chrome" in {
    val driver = UiScenarioDriver.create("distraction-free-default").unsafeRunSync()
    val state  = driver.state.unsafeRunSync()

    state.persisted.layout.editorPanes.keySet shouldBe Set(PaneId(0))
    state.persisted.layout.workspaceTree.map(_.dockedSurfaceIds) shouldBe Some(Nil)
    state.pinnedSurfaces shouldBe empty
    state.runtime.modalStack shouldBe empty
    state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))

    val frame = driver.renderFrame("default").unsafeRunSync()
    frame.evidence.focus shouldBe Focus.EditorPane(PaneId(0))
    frame.evidence.surfaceRects shouldBe empty
    frame.evidence.layoutViolations shouldBe empty
  }

  it should "lay out nested mixed-axis workspace panes without overlap, each rendering its own buffer" in {
    val driver         = UiScenarioDriver.create("nested-mixed-axis-panes").unsafeRunSync()
    val (left, top, bottom) = (PaneId(0), PaneId(1), PaneId(2))
    val (leftBuf, topBuf, bottomBuf) = (BufferId(0), BufferId(1), BufferId(2))
    val tree = WorkspaceTree(
      WorkspaceNode.Split(
        WorkspaceNodeId("outer"),
        SplitAxis.Horizontal,
        0.4,
        WorkspaceNode.Leaf(WorkspaceNodeId("left"), left),
        WorkspaceNode.Split(
          WorkspaceNodeId("inner"),
          SplitAxis.Vertical,
          0.5,
          WorkspaceNode.Leaf(WorkspaceNodeId("top"), top),
          WorkspaceNode.Leaf(WorkspaceNodeId("bottom"), bottom)
        )
      )
    )

    driver
      .updateState { state =>
        state.copy(
          persisted = state.persisted.copy(
            buffers = Map(
              leftBuf   -> Buffer.fromString(leftBuf, "leftpane"),
              topBuf    -> Buffer.fromString(topBuf, "toppane"),
              bottomBuf -> Buffer.fromString(bottomBuf, "bottompane")
            ),
            bufferOrder = List(leftBuf, topBuf, bottomBuf),
            layout = state.persisted.layout.copy(
              editorPanes = Map(
                left   -> EditorPane.withBuffer(left, leftBuf),
                top    -> EditorPane.withBuffer(top, topBuf),
                bottom -> EditorPane.withBuffer(bottom, bottomBuf)
              ),
              activeEditorPaneId = Some(left),
              workspaceTree = Some(tree)
            ),
            focus = Focus.EditorPane(left)
          ),
          runtime = state.runtime.copy(nextBufferId = BufferId(3), nextPaneId = PaneId(3))
        )
      }
      .unsafeRunSync()

    val state            = driver.state.unsafeRunSync()
    val calculatedLayout = LayoutEngine.calculateLayout(state, driver.environment.viewport)
    val paneRects        = LayoutEngine.calculatePaneLayouts(state, calculatedLayout)

    paneRects.keySet shouldBe Set(left, top, bottom)
    val rects = List(paneRects(left), paneRects(top), paneRects(bottom))
    rects.foreach(rect => calculatedLayout.editorPanelRect.containsRect(rect) shouldBe true)
    for
      first  <- rects
      second <- rects
      if first != second
    do overlaps(first, second) shouldBe false
    // Horizontal outer split: left's column sits fully left of top/bottom's shared column.
    paneRects(left).right shouldBe paneRects(top).x
    paneRects(left).right shouldBe paneRects(bottom).x
    // Vertical inner split: top sits directly above bottom, same column.
    paneRects(top).bottom shouldBe paneRects(bottom).y
    paneRects(top).x shouldBe paneRects(bottom).x

    val frame = driver.renderFrame("nested-panes").unsafeRunSync()
    frame.evidence.layoutViolations shouldBe empty
    val drawn = frame.evidence.drawnText.map(_.text).mkString(" ")
    drawn should include("leftpane")
    drawn should include("toppane")
    drawn should include("bottompane")
  }

  it should "dock multiple panels on the same edge, both visible and non-overlapping" in {
    val driver      = UiScenarioDriver.create("multiple-docked-panels-one-edge").unsafeRunSync()
    val firstPanel  = SurfaceId("outline-panel")
    val secondPanel = SurfaceId("comments-panel")

    driver
      .updateState { state =>
        DockedPanelFixtures.dockAll(
          state,
          List(
            (firstPanel, PanelContent.Outline(Nil), PanelPosition.Right, 20),
            (secondPanel, PanelContent.Comments(Nil), PanelPosition.Right, 15)
          )
        )
      }
      .unsafeRunSync()

    val state = driver.state.unsafeRunSync()
    state.pinnedSurfaces.map(_.id).toSet shouldBe Set(firstPanel, secondPanel)
    state.persisted.layout.workspaceTree.flatMap(_.positionForSurface(firstPanel)) shouldBe Some(PanelPosition.Right)
    state.persisted.layout.workspaceTree.flatMap(_.positionForSurface(secondPanel)) shouldBe Some(PanelPosition.Right)

    val frame = driver.renderFrame("multiple-docked").unsafeRunSync()
    frame.evidence.layoutViolations shouldBe empty
    frame.evidence.surfaceRects.keySet shouldBe Set(firstPanel, secondPanel)
    val firstRect  = frame.evidence.surfaceRects(firstPanel)
    val secondRect = frame.evidence.surfaceRects(secondPanel)
    overlaps(firstRect, secondRect) shouldBe false
    // Both panels reserve real space on the right edge of the viewport, distinct from the editor's own extent.
    firstRect.width should be > 0
    secondRect.width should be > 0
  }

  it should "maximise a docked panel to fill the workspace, then restore it to its docked size" in {
    val driver = UiScenarioDriver.create("panel-maximize-restore").unsafeRunSync()

    driver.stateManager.panelManager.pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Bottom, 8).unsafeRunSync()
    val pinnedSurfaceId = driver.state.unsafeRunSync().pinnedSurfaces.headOption
      .getOrElse(fail("Expected one pinned panel"))
      .id

    val pinnedFrame = driver.renderFrame("docked").unsafeRunSync()
    val dockedRect   = pinnedFrame.evidence.surfaceRects.getOrElse(pinnedSurfaceId, fail("Expected docked panel rect"))

    driver.stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "expand-bottom-panel",
          "Expand bottom panel",
          CommandIntent.View(ViewIntent.ExpandPanel(PanelPosition.Bottom)),
          CommandCategory.View
        )
      )
      .unsafeRunSync()

    driver.state.unsafeRunSync().persisted.layout.maximizedWorkspaceNodeId shouldBe defined
    val expandedFrame = driver.renderFrame("expanded").unsafeRunSync()
    val expandedRect =
      expandedFrame.evidence.surfaceRects.getOrElse(pinnedSurfaceId, fail("Expected expanded panel rect"))
    // A Bottom-docked panel already spans close to the full viewport width even at its small docked height, so
    // maximising it grows height dramatically while width can even shrink slightly (line-number gutter reservation
    // differs once the panel takes over the whole workspace) -- total area is the robust "now fills the workspace"
    // signal, not either dimension alone.
    expandedRect.height should be > dockedRect.height
    (expandedRect.width * expandedRect.height) should be > (dockedRect.width * dockedRect.height)

    driver.stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "collapse-expanded-panel",
          "Collapse expanded panel",
          CommandIntent.View(ViewIntent.CollapseExpandedPanel),
          CommandCategory.View
        )
      )
      .unsafeRunSync()

    driver.state.unsafeRunSync().persisted.layout.maximizedWorkspaceNodeId shouldBe None
    val restoredFrame = driver.renderFrame("restored").unsafeRunSync()
    val restoredRect =
      restoredFrame.evidence.surfaceRects.getOrElse(pinnedSurfaceId, fail("Expected restored panel rect"))
    restoredRect shouldBe dockedRect
  }
