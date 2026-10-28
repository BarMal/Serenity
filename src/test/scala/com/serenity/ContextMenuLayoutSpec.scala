package com.serenity

import com.serenity.command.*
import com.serenity.config.AppConfig
import com.serenity.config.AppConfigOps.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.DeviceOrigin
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389: an open context menu is laid out at its content's size, not the editor pane's. */
class ContextMenuLayoutSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private val CellWidthPx  = 9
  private val CellHeightPx = 18

  private def command(label: String): ContextMenuItem =
    val c = Command.typed(label, label, CommandIntent.Edit(EditIntent.Copy), label = label)
    ContextMenuItem(c.name, label, c)

  private def stateWithMenu(menu: ContextMenu): AppState =
    val buffer = Buffer
      .fromString(bufferId, List.fill(20)("abcdefghijklmnopqrstuvwxyz").mkString("\n"))
      .copy(editing = EditingState(List(CursorPosition(6, 18))))
    val initial = AppState.initial(AppConfig.conventional)
    initial.copy(
      persisted = initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.Surface(SurfaceId("context-menu"))
      ),
      runtime = initial.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("context-menu"),
            SurfaceContent.ContextMenu(menu),
            SurfacePresentation.Floating(Some(CursorPosition(6, 18)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )

  private val menu = ContextMenu(
    title = "editor",
    targetFocus = Focus.EditorPane(paneId),
    items = List(command("Save"), command("Cut"), command("Copy"))
  )

  private val expectedWidthCells = "editor".length + 2 + 2

  private def expectedHeightCells(state: AppState): Int =
    ContextMenuSurfaceComposition.frameHeight(
      menu,
      itemGapRows = state.persisted.config.effectiveCommandRunnerItemGapRows,
      itemTargetRows =
        SurfaceFrameLayout.itemTargetRowsFor(SurfaceContent.ContextMenu(menu), state.persisted.config.interfaceDensity)
    )

  private def menuRect(state: AppState): LayoutRect =
    LayoutEngine
      .calculateLayout(state, ViewportSize(100, 30))
      .belowCursorOverlayRect
      .getOrElse(fail("expected the context menu to be laid out below the cursor"))

  "an open context menu" should "be laid out at its content's width and height, not the pane's" in {
    val state = stateWithMenu(menu)
    val rect  = menuRect(state)
    val paneRect =
      LayoutEngine.calculatePaneLayouts(state, LayoutEngine.calculateLayout(state, ViewportSize(100, 30)))(paneId)
    val contentRect = CursorLayout.contentRectForPane(paneRect)

    rect.width shouldBe expectedWidthCells
    rect.height shouldBe expectedHeightCells(state)
    rect.width should be < contentRect.width
  }

  it should "stay inside the pane's content rect" in {
    val state = stateWithMenu(menu)
    val rect  = menuRect(state)
    val paneRect =
      LayoutEngine.calculatePaneLayouts(state, LayoutEngine.calculateLayout(state, ViewportSize(100, 30)))(paneId)
    val contentRect = CursorLayout.contentRectForPane(paneRect)

    rect.x should be >= contentRect.x
    rect.right should be <= contentRect.right
  }

  for scale <- List(1.0, 2.0) do
    it should s"cover exactly its content's device pixels at scale $scale" in {
      val state = stateWithMenu(menu)
      val rect  = menuRect(state)
      val bounds = PixelRect(
        rect.x * CellWidthPx,
        rect.y * CellHeightPx,
        rect.width * CellWidthPx,
        rect.height * CellHeightPx
      )

      DeviceOrigin.widthOf(bounds, scale) shouldBe (expectedWidthCells * CellWidthPx * scale).toInt
      DeviceOrigin.heightOf(bounds, scale) shouldBe (expectedHeightCells(state) * CellHeightPx * scale).toInt
    }
end ContextMenuLayoutSpec
