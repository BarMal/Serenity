package com.serenity.ui.renderer

import java.awt.Font

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme
import com.serenity.{MockRenderSurface, TestWorkspaceTrees}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The room below a pane's rows (#2396) is read off the frame's pane records when the pane has one, instead of building
  * the pane's row bands again for each of the frame's uses of it. Whatever it is read from, it has to be the room the
  * pane's snapshot gives.
  */
class CurrentRoomBelowRowsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val monoFont     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val cellMetrics  = CellMetrics.fromFont(monoFont)
  private val viewportSize = ViewportSize(120, 20)
  private val paneId       = PaneId(0)
  private val bufferId     = BufferId(1)

  private def stateOf(lines: Int): AppState =
    val buffer = Buffer.fromString(bufferId, (0 until lines).map(i => f"L$i%03d").mkString("\n"))
    val base   = AppState.initial
    base.copy(persisted =
      base.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        theme = Theme.light,
        config = AppConfig.default.withoutStatusLine.withLineNumbers(false).withWordWrap(true)
      )
    )

  private def contextAndPlan(state: AppState): (RenderContext, EditorPaneRenderPlan) =
    val caches  = RenderCaches.create()
    // A surface that addresses pixels, as the window's does: a room below the rows exists only on one.
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height):
      override def addressesPixels: Boolean = true
    val context = RenderContext(
      surface = surface,
      layout = CalculatedLayout(
        editorPanelRect = LayoutRect(0, 0, surface.width, surface.height),
        leftSpacerRect = LayoutRect(0, 0, 0, 0),
        rightSpacerRect = LayoutRect(0, 0, 0, 0)
      ),
      codeFont = monoFont,
      textFont = monoFont,
      uiFont = monoFont,
      cellMetrics = cellMetrics,
      uiMetrics = cellMetrics,
      caches = caches
    )
    val scene = caches.authoritativeScene.forState(state, viewportSize, monoFont, monoFont)
    (context, RendererPaneSetup.prepareEditorPaneRenderPlan(state, context, scene))

  /** The room the pane's snapshot gives, and the room read off the frame's records, for `snapshot` in place of the
    * pane's own.
    */
  private def bothRooms(state: AppState, snapshotOf: TextLayoutSnapshot => TextLayoutSnapshot) =
    val (context, plan) = contextAndPlan(state)
    val snapshot        = snapshotOf(plan.snapshots.getOrElse(paneId, fail("expected a snapshot")))
    val shown           = plan.copy(snapshots = plan.snapshots.updated(paneId, snapshot))
    val panes           = RendererPaneSetup.paneRecordsFor(state, context, shown)
    (
      RendererChromePlan.roomBelowRows(context, shown, paneId, snapshot),
      RendererChromePlan.currentRoomBelowRows(context, shown, panes, paneId),
      panes.contains(paneId)
    )

  private def spaced(before: Int, rows: Int)(snapshot: TextLayoutSnapshot): TextLayoutSnapshot =
    snapshot.copy(visualLines =
      snapshot.visualLines.take(rows).map(line => line.copy(spaceBeforePx = before, spaceAfterPx = 3))
    )

  "currentRoomBelowRows" should "equal the room the pane's snapshot gives, for a pane with a frame record" in {
    val (fromSnapshot, fromRecords, hasRecord) = bothRooms(stateOf(400), identity)

    hasRecord shouldBe true
    fromRecords shouldBe fromSnapshot
  }

  it should "equal it for rows that do not tile the pane, where a room below the last row exists" in {
    val results =
      for
        before <- List(3, 6, 9)
        rows   <- 1 to 19
      yield bothRooms(stateOf(400), spaced(before, rows))

    results.foreach { (fromSnapshot, fromRecords, hasRecord) =>
      hasRecord shouldBe true
      fromRecords shouldBe fromSnapshot
    }
    withClue("some row count must leave a room, or this checks nothing: ")(
      results.exists((fromSnapshot, _, _) => fromSnapshot.isDefined) shouldBe true
    )
  }

  it should "build the room from the snapshot for a pane with no frame record" in {
    val (fromSnapshot, fromRecords, hasRecord) = bothRooms(stateOf(0), spaced(6, 3))

    hasRecord shouldBe false
    fromRecords shouldBe fromSnapshot
  }
