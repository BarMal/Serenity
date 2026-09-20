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

/** Multi-column e-reader layout (issue #1338, Phase 2 / slice 2): each column shows its own line-number rail on its
  * left edge, the column's text wraps in the band the rail leaves, and both scale correctly with line numbers on/off
  * and column mode on/off.
  */
class RendererMultiColumnGutterRenderSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val monoFont     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val cellMetrics  = CellMetrics.fromFont(monoFont)
  private val viewportSize = ViewportSize(160, 20)

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def stateWith(config: AppConfig): AppState =
    // Short numbered lines so each is one visual row and chunk boundaries land on predictable buffer line numbers.
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

  private def columnConfig(lineNumbers: Boolean): AppConfig =
    AppConfig.default.withoutStatusLine
      .withLineNumbers(lineNumbers)
      .withWordWrap(true)
      .withColumnMode(true)
      .withColumnTargetWidth(20)
      .withColumnGap(2)

  private def scene(state: AppState): UiSceneSnapshot =
    AuthoritativeUiScene.forState(state, viewportSize, monoFont, monoFont)

  private def render(state: AppState): MockRenderSurface =
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)
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
    surface

  "AuthoritativeUiScene, in column mode with line numbers on" should "give every column a non-zero gutter rail" in {
    val placements = scene(stateWith(columnConfig(lineNumbers = true))).columnSnapshotsFor(paneId)

    assert(placements.length > 1, s"expected a multi-column page, got ${placements.length}")
    placements.foreach(p => withClue(s"column ${p.columnIndex}: ")(p.gutterWidthCells should be > 0))
  }

  it should "wrap column text in the band the rail leaves, not the full column width" in {
    val placements  = scene(stateWith(columnConfig(lineNumbers = true))).columnSnapshotsFor(paneId)
    val textWidthPx = placements.head.snapshot.panelWidthPx
    val bandWidthPx = placements.head.columnWidthCells * cellMetrics.charWidth

    textWidthPx should be < bandWidthPx
    textWidthPx shouldBe (placements.head.columnWidthCells - placements.head.gutterWidthCells) * cellMetrics.charWidth
  }

  "AuthoritativeUiScene, in column mode with line numbers off" should "carry no rail and wrap at the full column width" in {
    val placements = scene(stateWith(columnConfig(lineNumbers = false))).columnSnapshotsFor(paneId)

    assert(placements.length > 1, s"expected a multi-column page, got ${placements.length}")
    placements.foreach { p =>
      withClue(s"column ${p.columnIndex}: ") {
        p.gutterWidthCells shouldBe 0
        p.snapshot.panelWidthPx shouldBe p.columnWidthCells * cellMetrics.charWidth
      }
    }
  }

  "RendererEntryPoints.render, in column mode with line numbers on" should
    "paint each column's own first buffer line number at that column's rail x-origin" in {
      val state       = stateWith(columnConfig(lineNumbers = true))
      val theScene    = scene(state)
      val placements  = theScene.columnSnapshotsFor(paneId)
      val contentRect = theScene.paneLayouts(paneId).contentRect
      val surface     = render(state)

      placements.foreach { placement =>
        val firstBufferLine = placement.snapshot.visualLines.headOption
          .map(_.bufferLine)
          .getOrElse(fail(s"column ${placement.columnIndex} empty"))
        val expectedNumber = (firstBufferLine + 1).toString
        val railXPx        = cellMetrics.toPixelX(contentRect.x + placement.xOffsetCells).toFloat
        val matching = surface.drawRunPxCalls.filter(call =>
          call.s.trim == expectedNumber && math.abs(call.xPx - railXPx) < cellMetrics.charWidth.toFloat
        )
        withClue(s"column ${placement.columnIndex} rail number '$expectedNumber' at xPx=$railXPx: ") {
          matching should not be empty
        }
      }
    }

  it should "offset each column's text to the right of its rail" in {
    val state       = stateWith(columnConfig(lineNumbers = true))
    val theScene    = scene(state)
    val placements  = theScene.columnSnapshotsFor(paneId)
    val contentRect = theScene.paneLayouts(paneId).contentRect
    val surface     = render(state)

    placements.foreach { placement =>
      val firstText = placement.snapshot.visualLines.headOption.map(_.text).getOrElse(fail("empty column"))
      val textXPx =
        cellMetrics.toPixelX(contentRect.x + placement.xOffsetCells + placement.gutterWidthCells).toFloat
      val matching = surface.drawRunPxCalls.filter(call =>
        call.s.startsWith(firstText) && math.abs(call.xPx - textXPx) < cellMetrics.charWidth.toFloat
      )
      withClue(s"column ${placement.columnIndex} text '$firstText' at xPx=$textXPx: ") {
        matching should not be empty
      }
    }
  }

  it should "never paint a column's text over its own rail column" in {
    val state       = stateWith(columnConfig(lineNumbers = true))
    val theScene    = scene(state)
    val placements  = theScene.columnSnapshotsFor(paneId)
    val contentRect = theScene.paneLayouts(paneId).contentRect
    val surface     = render(state)

    // Every column body draw must start at or past that column's text origin (rail x + gutter width), never inside the
    // rail's own cells.
    placements.foreach { placement =>
      val textLeftPx = cellMetrics.toPixelX(contentRect.x + placement.xOffsetCells + placement.gutterWidthCells).toFloat
      val bodyTexts  = placement.snapshot.visualLines.map(_.text).filter(_.nonEmpty).toSet
      val bodyDraws  = surface.drawRunPxCalls.filter(call => bodyTexts.exists(t => call.s.startsWith(t)))
      bodyDraws.foreach { call =>
        withClue(s"column ${placement.columnIndex} body draw '${call.s}' at xPx=${call.xPx}: ") {
          call.xPx should be >= (textLeftPx - cellMetrics.charWidth.toFloat)
        }
      }
    }
  }

  "RendererEntryPoints.render, with column mode off" should "carry no column rails" in {
    val state = stateWith(
      AppConfig.default.withoutStatusLine.withLineNumbers(true).withWordWrap(true).withColumnMode(false)
    )
    scene(state).columnSnapshotsFor(paneId) shouldBe empty
    noException should be thrownBy render(state)
  }
