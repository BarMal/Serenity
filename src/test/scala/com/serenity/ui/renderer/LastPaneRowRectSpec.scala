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

/** A pane's room below its last row (#2396) needs only the last row's band, so it must not build every row's. The last
  * band has to be exactly the last of the bands built for all of them.
  */
class LastPaneRowRectSpec extends AnyFlatSpec with Matchers:

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
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)
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

  private def lastBandIsTheLastOfAll(snapshotOf: TextLayoutSnapshot => TextLayoutSnapshot, lines: Int): Unit =
    val (context, plan) = contextAndPlan(stateOf(lines))
    val contentRect     = plan.paneLayouts.getOrElse(paneId, fail("expected a pane layout")).contentRect
    val snapshot        = snapshotOf(plan.snapshots.getOrElse(paneId, fail("expected a snapshot")))

    snapshot.visualLines should not be empty
    RendererPaneSetup.paneRowRects(contentRect, context, snapshot) should not be empty
    RendererPaneSetup.lastPaneRowRect(contentRect, context, snapshot) shouldBe
      RendererPaneSetup.paneRowRects(contentRect, context, snapshot).lastOption

  "lastPaneRowRect" should "be the last of the rows' bands when the document is shorter than the pane" in
    lastBandIsTheLastOfAll(identity, lines = 6)

  it should "be the last of the rows' bands when the document fills and overflows the pane" in
    lastBandIsTheLastOfAll(identity, lines = 400)

  it should "be the last of the rows' bands when every row has room above and below it" in
    lastBandIsTheLastOfAll(
      snapshot =>
        snapshot.copy(visualLines = snapshot.visualLines.map(line => line.copy(spaceBeforePx = 6, spaceAfterPx = 3))),
      lines = 400
    )

  it should "be None when the snapshot has no rows" in {
    val (context, plan) = contextAndPlan(stateOf(6))
    val contentRect     = plan.paneLayouts.getOrElse(paneId, fail("expected a pane layout")).contentRect
    val empty           = plan.snapshots.getOrElse(paneId, fail("expected a snapshot")).copy(visualLines = Vector.empty)

    RendererPaneSetup.lastPaneRowRect(contentRect, context, empty) shouldBe None
  }
