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

  private def panelWidthColumnsFor(state: AppState): Int =
    com.serenity.ui.layout.LayoutEngine.calculateLayout(state, ViewportSize(80, 24)).editorPanelRect.width

  "EditorGeometryProducer.forPane, for vertical navigation" should
    "cover the cursor's own line even when a heavily-wrapped line at the viewport top would fill the window" in {
      // The bug: the producer pinned its geometry window to the viewport's top line and a `visibleLines` row budget, so
      // a single long wrapped line at the top filled the whole window and the cursor's own line (below it) fell out of
      // the geometry -- `moveVertical` then returned None and vertical nav silently dropped to naive char-grid
      // movement, which mis-wraps prose ("stuck"/jumpy cursor past the first screenful).
      val probe             = stateWith(Buffer.fromString(bufferId, "probe"), isTuiMode = true)
      val panelWidthColumns = panelWidthColumnsFor(probe)
      val longLine          = "w" * (panelWidthColumns * 4) // wraps into ~4 visual rows, more than the 3-row window
      val buffer0           = Buffer.fromString(bufferId, Vector.fill(10)(longLine).mkString("\n"))
      val buffer = buffer0.copy(
        viewport =
          buffer0.viewport.copy(topLine = 5, topVisualLine = 0, visibleLines = 3, visibleColumns = panelWidthColumns),
        editing = buffer0.editing.copy(cursors = List(CursorPosition(6, 0)))
      )
      val tuiState = stateWith(buffer, isTuiMode = true)

      val geometry = EditorGeometryProducer.forPane(tuiState, paneId).getOrElse(fail("expected geometry for pane"))

      geometry.navigation.visualLineFor(CursorPosition(6, 0)) should not be None
      geometry.navigation.moveVertical(CursorPosition(6, 0), direction = 1, preferredXPx = 0.0f) should not be None
    }

  it should "cover the cursor's own visual row deep inside one long wrapped paragraph" in {
    // Prose wraps one logical line into far more visual rows than the window's row budget. Anchoring the window on
    // the cursor's *logical* line alone left the cursor's own visual row past the end of the budget once a paragraph
    // ran longer than a few screenfuls, so `moveVertical` returned None and Up/Down fell back to naive char-grid
    // movement -- the "wrapped navigation jumps by logical line" report.
    val probe             = stateWith(Buffer.fromString(bufferId, "probe"), isTuiMode = true)
    val panelWidthColumns = panelWidthColumnsFor(probe)
    val paragraph         = "w" * (panelWidthColumns * 100)
    val buffer0           = Buffer.fromString(bufferId, paragraph)
    val cursor            = CursorPosition(0, panelWidthColumns * 50)
    val buffer = buffer0.copy(
      viewport =
        buffer0.viewport.copy(topLine = 0, topVisualLine = 0, visibleLines = 3, visibleColumns = panelWidthColumns),
      editing = buffer0.editing.copy(cursors = List(cursor))
    )
    val tuiState = stateWith(buffer, isTuiMode = true)

    val geometry = EditorGeometryProducer.forPane(tuiState, paneId).getOrElse(fail("expected geometry for pane"))
    val cursorRow = geometry.navigation
      .visualLineFor(cursor)
      .getOrElse(fail("expected the cursor's own visual row to be covered"))
    cursorRow.startColumn shouldBe cursor.column

    val down = geometry.navigation
      .moveVertical(cursor, direction = 1, preferredXPx = 0.0f)
      .getOrElse(fail("expected a visual row below the cursor"))
    val up = geometry.navigation
      .moveVertical(cursor, direction = -1, preferredXPx = 0.0f)
      .getOrElse(fail("expected a visual row above the cursor"))

    down shouldBe CursorPosition(0, cursor.column + panelWidthColumns)
    up shouldBe CursorPosition(0, cursor.column - panelWidthColumns)
  }
