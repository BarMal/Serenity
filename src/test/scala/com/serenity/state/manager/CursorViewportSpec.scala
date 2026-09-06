package com.serenity.state.manager

import com.serenity.rope.Balance
import com.serenity.state.models.*
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
          paneOrder = List(paneId)
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
