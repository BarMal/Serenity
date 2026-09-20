package com.serenity

import java.awt.Font

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.manager.AuthoritativeUiScene
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.RendererEntryPoints
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Multi-column e-reader layout (issue #1338, Phase 2 / slice 1): a full `RendererEntryPoints.render` pass must paint
  * every fitted column of the page side by side, each at its own x-origin, with content flowing column -> column.
  */
class RendererMultiColumnRenderSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val monoFont     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val cellMetrics  = CellMetrics.fromFont(monoFont)
  private val viewportSize = ViewportSize(160, 20)

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def stateWith(config: AppConfig): AppState =
    // Short numbered lines so each is one visual row and chunk boundaries land on predictable line numbers; abundant
    // enough to fill several columns of the page.
    val content = (0 until 400).map(i => f"L$i%03d").mkString("\n")
    val buffer  = Buffer.fromString(bufferId, content)
    val base    = AppState.initial
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
        config = config
      )
    )

  private def columnConfig: AppConfig =
    AppConfig.default.withoutStatusLine
      .withLineNumbers(false)
      .withWordWrap(true)
      .withColumnMode(true)
      .withColumnTargetWidth(20)
      .withColumnGap(2)

  "RendererEntryPoints.render, in column mode" should "paint every fitted column at its own x-origin" in {
    val state   = stateWith(columnConfig)
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)

    // The authoritative scene owns the same per-column placements the render pass paints from, so the test asserts
    // against the placements the renderer itself will use rather than re-deriving the column geometry.
    val scene =
      AuthoritativeUiScene.forState(state, viewportSize, monoFont, monoFont)
    val placements = scene.columnSnapshotsFor(paneId)
    assert(placements.length > 1, s"test setup expected a multi-column page, got ${placements.length} columns")

    RendererEntryPoints.render(
      state,
      cursorVisible = true,
      surface,
      viewportSize,
      monoFont,
      monoFont,
      cellMetrics,
      None
    )

    val contentRect = scene.paneLayouts(paneId).contentRect
    // Each column's own first buffer line must be painted, and at that column's x-origin in pixels (not column 0's).
    placements.foreach { placement =>
      val firstLineText = placement.snapshot.visualLines.headOption.map(_.text).getOrElse(fail("empty column"))
      val expectedXPx   = cellMetrics.toPixelX(contentRect.x + placement.xOffsetCells).toFloat
      val matching      = surface.drawRunPxCalls.filter(_.s.startsWith(firstLineText))
      withClue(s"column ${placement.columnIndex} (line '$firstLineText') at xPx=$expectedXPx: ") {
        matching.exists(call => math.abs(call.xPx - expectedXPx) < cellMetrics.charWidth.toFloat) shouldBe true
      }
    }
  }

  it should "flow content column -> column, later columns starting past the previous column's last line" in {
    val state = stateWith(columnConfig)
    val scene =
      AuthoritativeUiScene.forState(state, viewportSize, monoFont, monoFont)
    val placements = scene.columnSnapshotsFor(paneId)

    placements.sliding(2).foreach {
      case Vector(left, right) if left.snapshot.visualLines.nonEmpty && right.snapshot.visualLines.nonEmpty =>
        assert(right.snapshot.visualLines.head.bufferLine > left.snapshot.visualLines.last.bufferLine)
      case _ => ()
    }
  }

  "RendererEntryPoints.render, with column mode off" should "carry no column placements and paint a single column at the pane origin" in {
    val state =
      stateWith(AppConfig.default.withoutStatusLine.withLineNumbers(false).withWordWrap(true).withColumnMode(false))
    val scene =
      AuthoritativeUiScene.forState(state, viewportSize, monoFont, monoFont)

    scene.columnSnapshotsFor(paneId) shouldBe empty

    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)
    noException should be thrownBy RendererEntryPoints.render(
      state,
      cursorVisible = true,
      surface,
      viewportSize,
      monoFont,
      monoFont,
      cellMetrics,
      None
    )
    // Column-0 content still paints at the pane's own left edge.
    val contentRect = scene.paneLayouts(paneId).contentRect
    val leftEdgePx  = cellMetrics.toPixelX(contentRect.x).toFloat
    surface.drawRunPxCalls.exists(call =>
      call.s.startsWith("L00") && math.abs(call.xPx - leftEdgePx) < cellMetrics.charWidth.toFloat
    ) shouldBe true
  }
