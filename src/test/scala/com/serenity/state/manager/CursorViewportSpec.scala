package com.serenity.state.manager

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Typewriter-style centring for the cursor's *visual* row, measured on the terminal's own cell grid. The producing
  * bug: viewport centring counted wrapped rows with a pixel measurement of the proportional prose font (not the cell
  * grid the terminal actually wraps on) and discarded the partial wrapped-row offset when the top landed mid-line, so
  * the cursor drifted off-centre in wrapped prose in TUI mode.
  */
class CursorViewportSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def tuiStateWith(buffer: Buffer): AppState =
    val base = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId)
      )
    )
    base.copy(runtime = base.runtime.copy(isTuiMode = true))

  "CursorViewport.adjustForCursor, in TUI mode with word wrap" should
    "centre the cursor's visual row, carrying the partial wrapped-row offset into the top line" in {
      // Each line is 41 cells wide -> wraps into 3 visual rows on a 20-column TUI grid ([0,20),[20,40),[40,41)).
      val line    = "w" * 41
      val content = Vector.fill(30)(line).mkString("\n")
      val buffer = Buffer
        .fromString(bufferId, content)
        .copy(
          viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 20, visibleLines = 8),
          editing = Buffer.fromString(bufferId, content).editing.copy(cursors = List(CursorPosition(10, 0)))
        )
      val state = tuiStateWith(buffer)

      state.persisted.config.surfaceConfig.wordWrapEnabled shouldBe true
      val adjusted = CursorViewport.adjustForCursor(buffer, state, CursorPosition(10, 0))

      // halfVisibleLines = 4. Walking 4 visual rows up from line 10 row 0 crosses line 9 (3 rows) and lands 1 more row
      // up -- 2 rows into line 8's own 3 wrapped rows -- so the cursor sits exactly 4 rows below the top: centred. The
      // pre-fix code forced topVisualLine to 0 whenever the top wasn't the cursor's own line, leaving the cursor at
      // screen row 6 instead of 4.
      adjusted.topLine shouldBe 8
      adjusted.topVisualLine shouldBe 2
    }

  it should "not scroll above the first line when the cursor is near the document start" in {
    val line    = "w" * 41
    val content = Vector.fill(30)(line).mkString("\n")
    val buffer = Buffer
      .fromString(bufferId, content)
      .copy(
        viewport = Viewport(topLine = 5, leftColumn = 0, visibleColumns = 20, visibleLines = 8),
        editing = Buffer.fromString(bufferId, content).editing.copy(cursors = List(CursorPosition(0, 0)))
      )
    val state = tuiStateWith(buffer)

    val adjusted = CursorViewport.adjustForCursor(buffer, state, CursorPosition(0, 0))

    adjusted.topLine shouldBe 0
    adjusted.topVisualLine shouldBe 0
  }

  /** Regression cover for #1293: typewriter scrolling ("keep the caret's line vertically centred", #1204) never held
    * while typing at the actual end of a document -- the single most common typewriter-mode scenario -- because the
    * bottom clamp below always overrode centring whenever there wasn't a full half-viewport of real content beneath the
    * cursor's line, which is true for every line-appending keystroke. `typewriterScrollingEnabled` (default off,
    * preserving the clamp so no existing scroll spec regresses) lifts that clamp instead of adding a new formula.
    */
  private def lastLineState(typewriterScrollingEnabled: Boolean): (Buffer, AppState, CursorPosition) =
    val content = (0 until 20).map(i => s"line $i").mkString("\n")
    val cursor  = CursorPosition(19, 6)
    val buffer = Buffer
      .fromString(bufferId, content)
      .copy(
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 40, visibleLines = 8),
        editing = Buffer.fromString(bufferId, content).editing.copy(cursors = List(cursor))
      )
    val state = tuiStateWith(buffer)
    val configuredState = state.copy(persisted =
      state.persisted.copy(config = state.persisted.config.withTypewriterScrolling(typewriterScrollingEnabled))
    )
    (buffer, configuredState, cursor)

  "CursorViewport.adjustForCursor, with typewriter scrolling enabled" should
    "keep the cursor's line centred at its configured row even at the document's last line, padding past the end" in {
      val (buffer, state, cursor) = lastLineState(typewriterScrollingEnabled = true)

      val adjusted = CursorViewport.adjustForCursor(buffer, state, cursor)

      // halfVisibleLines = 4: line 19 (the document's last, and the cursor's) sits 4 rows below the top even though
      // there are no real lines below it to fill the rest of the viewport.
      adjusted.topLine shouldBe 15
      adjusted.topVisualLine shouldBe 0
    }

  "CursorViewport.adjustForCursor, with typewriter scrolling disabled (the default)" should
    "fall back to showing as much real content as fits at the document's last line" in {
      val (buffer, state, cursor) = lastLineState(typewriterScrollingEnabled = false)

      val adjusted = CursorViewport.adjustForCursor(buffer, state, cursor)

      // Bottom-aligned: lines 12..19 are the latest window that fills all 8 rows with real content, leaving the
      // cursor's line at the very bottom row rather than centred.
      adjusted.topLine shouldBe 12
      adjusted.topVisualLine shouldBe 0
    }

  /** Regression cover for #1041: the vertical scroll math (`halfVisibleLines`, the bottom-alignment clamp) is driven by
    * `viewport.visibleLines`, which is a code-grid row count (`LayoutEngine.updateViewportDimensions` sets it from the
    * panel's grid rows, the same convention `TextLayoutSnapshot.gridWrapWidthPx` uses for columns) -- it does not vary
    * with which font a pane actually draws with. A document-font (Prose) pane whose font has a taller line height than
    * the code font fits fewer of its own rows into that same pixel height than a code-font pane would (exactly what
    * `RendererPaneSetup.snapshotForBuffer`'s own `visibleLines = panelHeightPx / bufferMetrics.lineHeight` computes for
    * the pane actually painted). Using the unadjusted code-grid row count here let the scroll math assume more rows fit
    * than the pane's own font renders, scrolling the cursor below the pane's real bottom edge on a long wrapped line.
    */
  "CursorViewport.adjustForCursor, in GUI mode for a document-font pane" should
    "keep the cursor within the pane's actually-rendered rows, not the code-grid row count" in {
      val fontConfig   = FontLoader.FontConfig(textFontSize = 40f, fontSize = 10f)
      val wrappingLine = ("word " * 30).trim
      val content      = Vector.fill(6)(wrappingLine).mkString("\n")
      val cursorPos    = CursorPosition(3, 0)
      val buffer = Buffer
        .fromString(bufferId, content)
        .copy(
          viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 20, visibleLines = 10),
          editing = Buffer.fromString(bufferId, content).editing.copy(cursors = List(cursorPos))
        )
      val state = AppState.initial.copy(persisted =
        AppState.initial.persisted.copy(
          buffers = Map(buffer.id -> buffer),
          bufferOrder = List(buffer.id),
          layout = AppState.initial.persisted.layout.copy(
            editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
            activeEditorPaneId = Some(paneId),
            workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
          ),
          focus = Focus.EditorPane(paneId),
          config = AppState.initial.persisted.config.withFontConfig(fontConfig)
        )
      )
      buffer.typographyRole shouldBe TypographyRole.Prose
      state.runtime.isTuiMode shouldBe false

      val adjusted = CursorViewport.adjustForCursor(buffer, state, cursorPos)

      // Mirrors RendererPaneSetup.snapshotForBuffer's own row-fit calculation for the pane actually painted.
      val codeFont            = FontLoader.previewCodeFont(fontConfig)
      val textFont            = FontLoader.previewTextFont(fontConfig)
      val panelHeightPx       = buffer.viewport.visibleLines * CellMetrics.fromFont(codeFont).lineHeight
      val actuallyVisibleRows = math.max(1, panelHeightPx / CellMetrics.fromFont(textFont).lineHeight)

      val gridWidthPx = TextLayoutSnapshot.gridWrapWidthPx(buffer.viewport.visibleColumns, fontConfig)
      def visualRowsFor(lineIndex: Int): Int =
        TextLayoutSnapshot
          .boundedVisualLinesForText(
            buffer.document.content.getLine(lineIndex).getOrElse(""),
            lineIndex,
            gridWidthPx,
            textFont
          )
          .length
          .max(1)
      val topVisualRow =
        (adjusted.topLine until cursorPos.line).map(visualRowsFor).sum +
          TextLayoutSnapshot.visualLineIndexForCursor(
            buffer.document.content.getLine(cursorPos.line).getOrElse(""),
            cursorPos.column,
            gridWidthPx,
            textFont,
            wordWrapEnabled = true
          )
      val cursorRowFromTop = topVisualRow - adjusted.topVisualLine

      cursorRowFromTop should be < actuallyVisibleRows
    }

  // Column mode (issue #1338, Phase 1): fixed/discrete snapping only -- `topVisualLine` always lands on an exact
  // `activeColumnIndex * visibleLines` boundary, never a fractional/partial scroll position.
  "CursorViewport.adjustForCursorColumnMode" should
    "snap topVisualLine to the column boundary containing the cursor's visual row" in {
      // 30 single-row lines, 8 rows per column -> columns are [0,8) [8,16) [16,24) [24,30).
      val content = (0 until 30).map(i => s"line $i").mkString("\n")
      val buffer = Buffer
        .fromString(bufferId, content)
        .copy(
          viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 40, visibleLines = 8),
          editing = Buffer.fromString(bufferId, content).editing.copy(cursors = List(CursorPosition(20, 0)))
        )
      val state = tuiStateWith(buffer)

      val adjusted = CursorViewport.adjustForCursorColumnMode(buffer, state, CursorPosition(20, 0))

      adjusted.topLine shouldBe 16
      adjusted.topVisualLine shouldBe 0
    }

  it should "snap to the first column when the cursor is within it" in {
    val content = (0 until 30).map(i => s"line $i").mkString("\n")
    val buffer = Buffer
      .fromString(bufferId, content)
      .copy(
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 40, visibleLines = 8),
        editing = Buffer.fromString(bufferId, content).editing.copy(cursors = List(CursorPosition(3, 0)))
      )
    val state = tuiStateWith(buffer)

    val adjusted = CursorViewport.adjustForCursorColumnMode(buffer, state, CursorPosition(3, 0))

    adjusted.topLine shouldBe 0
    adjusted.topVisualLine shouldBe 0
  }

  "CursorViewport.ensureVisibleCursors" should
    "dispatch to the column-mode placement when column mode and word wrap are both on" in {
      val content = (0 until 30).map(i => s"line $i").mkString("\n")
      val buffer = Buffer
        .fromString(bufferId, content)
        .copy(
          viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 40, visibleLines = 8),
          editing = Buffer.fromString(bufferId, content).editing.copy(cursors = List(CursorPosition(0, 0)))
        )
      val before = tuiStateWith(buffer).copy(persisted =
        tuiStateWith(buffer).persisted.copy(config =
          tuiStateWith(buffer).persisted.config.withColumnMode(true)
        )
      )
      val movedBuffer = buffer.copy(editing = buffer.editing.copy(cursors = List(CursorPosition(20, 0))))
      val after       = before.copy(persisted = before.persisted.copy(buffers = Map(bufferId -> movedBuffer)))

      val result = CursorViewport.ensureVisibleCursors(before, after)

      val resultViewport = result.persisted.buffers(bufferId).viewport
      resultViewport.topLine shouldBe 16
      resultViewport.topVisualLine shouldBe 0
    }
