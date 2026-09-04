package com.serenity.state.manager

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** TUI mode must wrap text at the exact terminal-cell column count (`TuiRuntime`'s `CellMetricsOne`/
  * `CellMetrics.cellUnit`), not at a pixel measurement of a proportional font -- otherwise the wrap geometry this
  * producer hands to mouse hit-testing and vertical/Home/End cursor movement disagrees with what the terminal itself
  * actually drew, the mismatch #1215-class bugs come from.
  */
class EditorGeometryProducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def stateWith(buffer: Buffer, isTuiMode: Boolean): AppState =
    val base = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          paneOrder = List(paneId)
        ),
        focus = Focus.EditorPane(paneId)
      )
    )
    base.copy(runtime = base.runtime.copy(isTuiMode = isTuiMode, viewportSize = Some(ViewportSize(80, 24))))

  // No language set -> TypographyRole.Prose (see Buffer.typographyRole), which resolves to a proportional font in
  // GUI mode -- the same fixture shape MouseTargetCacheSpec's "use the renderer's proportional wrapped snapshot"
  // test already proves is genuinely proportional (`usesMeasuredLayout`/`isProportional` both true) in this
  // environment. "i" is narrower than the nominal cell width ('M', what `CellMetrics.fromFont` measures) in any
  // real proportional font, so more of them fit in a pixel-measured row than fit in a fixed-width terminal cell row.
  private val narrowRun = "i" * 200

  "EditorGeometryProducer.forPane" should "wrap at exactly the panel's column count in TUI mode" in {
    val buffer   = Buffer.fromString(bufferId, narrowRun)
    val tuiState = stateWith(buffer, isTuiMode = true)
    val panelWidthColumns = com.serenity.ui.layout.LayoutEngine
      .calculateLayout(tuiState, ViewportSize(80, 24))
      .editorPanelRect
      .width

    val geometry = EditorGeometryProducer.forPane(tuiState, paneId).getOrElse(fail("expected geometry for pane"))
    val firstRow = geometry.navigation.visualLines.headOption.getOrElse(fail("expected at least one visual line"))

    (firstRow.endColumn - firstRow.startColumn) shouldBe panelWidthColumns
  }

  it should "wrap later than the panel's column count in GUI mode, for the same proportional-font content" in {
    val buffer   = Buffer.fromString(bufferId, narrowRun)
    val guiState = stateWith(buffer, isTuiMode = false)
    val panelWidthColumns = com.serenity.ui.layout.LayoutEngine
      .calculateLayout(guiState, ViewportSize(80, 24))
      .editorPanelRect
      .width

    val geometry = EditorGeometryProducer.forPane(guiState, paneId).getOrElse(fail("expected geometry for pane"))
    val firstRow = geometry.navigation.visualLines.headOption.getOrElse(fail("expected at least one visual line"))

    (firstRow.endColumn - firstRow.startColumn) should be > panelWidthColumns
  }
