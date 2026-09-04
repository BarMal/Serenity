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
