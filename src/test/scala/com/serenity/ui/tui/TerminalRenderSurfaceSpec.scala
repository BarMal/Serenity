package com.serenity.ui.tui

import java.awt.Color
import java.io.StringWriter

import com.serenity.config.CursorMode
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, PixelRect, ViewportSize}
import com.serenity.ui.renderer.{HardwareCursorShape, HardwareCursorStyle, Renderer}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers #1107's `TerminalRenderSurface`: forwarding onto a `TerminalScreenBuffer`, the `RenderSurface` capability
  * shape (`persistentContentKey`, the required-but-inert `text`/`pixels` groups, `withRoundRectClip` as a plain
  * rectangular clip), and driving an `AppState` through #1104's surface-generic `Renderer` entry points to produce real
  * damage-diffed ANSI output -- the harness AC.
  */
class TerminalRenderSurfaceSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val esc = 0x1b.toChar.toString

  private def surface(width: Int = 10, height: Int = 5): (TerminalRenderSurface, StringWriter) =
    val writer      = new StringWriter()
    val cellMetrics = CellMetrics(charWidth = 1, lineHeight = 1, ascent = 0)
    (new TerminalRenderSurface(width, height, writer, cellMetrics), writer)

  "putString/fillRect" should "forward to the underlying screen buffer, visible through flush's ANSI diff" in {
    val (rs, writer) = surface()
    rs.setForegroundColor(Color.RED)
    rs.setBackgroundColor(Color.BLUE)
    rs.putString(1, 2, "hi")
    rs.flush()

    writer.toString should include("hi")
    // first flush has no previous frame (full repaint), wrapped in #1172's DEC 2026 synchronized-update brackets
    writer.toString should startWith(s"$esc[?2026h$esc[2J$esc[H")
  }

  "flush" should "emit only the cells that changed since the previous flush" in {
    val (rs, writer) = surface()
    rs.putString(0, 0, "a")
    rs.flush()
    writer.getBuffer.setLength(0)

    rs.putString(0, 0, "a") // unchanged
    rs.flush()

    writer.toString shouldBe ""
  }

  "persistentContentKey" should "be present, and stable across frames from the same surface" in {
    val (rs, _) = surface()
    rs.persistentContentKey shouldBe defined
    rs.persistentContentKey shouldBe rs.persistentContentKey
  }

  "clearViewportExcept" should "blank every cell outside the preserved rectangles and leave the rest untouched" in {
    val (rs, writer) = surface(width = 4, height = 2)
    rs.setForegroundColor(Color.WHITE)
    rs.setBackgroundColor(Color.BLACK)
    rs.putString(0, 0, "ab")
    rs.putString(2, 0, "cd")
    rs.flush()
    writer.getBuffer.setLength(0)

    rs.clearViewportExcept(Color.BLACK, List(PixelRect(0, 0, 2, 1)))
    rs.flush()

    // "ab" was preserved and never rewritten, so it produces no diff; "cd" was cleared to blanks.
    writer.toString should not include "cd"
    writer.toString should not include "ab"
  }

  "viewportWidth/viewportHeight" should "report the surface's cell dimensions" in {
    val (rs, _) = surface(width = 7, height = 3)
    rs.viewportWidth shouldBe 7
    rs.viewportHeight shouldBe 3
  }

  "fontRenderContext" should "always be None" in {
    val (rs, _) = surface()
    rs.text.fontRenderContext shouldBe None
  }

  "the required text/pixels capability groups" should "be present and inert rather than throwing" in {
    val (rs, _) = surface()
    noException should be thrownBy rs.text.drawRunPx(0f, 0, 1f, 1, 1, "x")
    noException should be thrownBy rs.text.withLogicalPixelRow(0, 0)(())
    noException should be thrownBy rs.pixels.fillPixelRect(0, 0, 1, 1, Color.RED)
    noException should be thrownBy rs.pixels.withPixelTranslation(0.0, 0.0)(())
  }

  "withRoundRectClip" should "restrict putString to a rectangular cell region, ignoring the arc radius" in {
    val (rs, writer) = surface(width = 4, height = 1)
    rs.setForegroundColor(Color.WHITE)
    rs.setBackgroundColor(Color.BLACK)
    rs.roundedRects shouldBe defined
    rs.roundedRects.get.withRoundRectClip(0, 0, 2, 1, arcPx = 99) {
      rs.putString(0, 0, "abcd") // would overflow the 2-wide clip if it weren't enforced
    }
    rs.flush()

    writer.toString should include("ab")
    writer.toString should not include "cd"
  }

  "Renderer.render (surface-generic)" should "paint an AppState into a TerminalRenderSurface and flush real ANSI escapes" in {
    val (rs, writer) = surface(width = 80, height = 24)
    val paneId       = PaneId(0)
    val bufferId     = BufferId(1)
    val viewport     = ViewportSize(80, 24)
    val buffer       = Buffer.fromString(bufferId, "hello terminal")
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          paneOrder = List(paneId)
        ),
        focus = Focus.EditorPane(paneId),
        theme = Theme.light
      )
    )

    Renderer.render(state, cursorVisible = false, rs, viewport)

    val output = writer.toString
    // first frame: full clear-and-repaint, wrapped in #1172's DEC 2026 synchronized-update brackets
    output should startWith(s"$esc[?2026h$esc[2J$esc[H")
    // The gutter's line number reaches the screen through `putString` (a real cell-addressed draw this surface
    // performs), confirming the frame actually painted through to the terminal rather than producing an empty diff.
    output should include("1")
    // #1105: a surface reporting no FontRenderContext degrades every measured-text call site to the cell-based
    // putString path, so the buffer's own (proportional-font) text now reaches the screen too, instead of being
    // silently dropped behind drawRunPx's no-op.
    output should include("hello terminal")
  }

  // -- #1170: the caret delegated to the terminal's own cursor (DECTCEM/CUP/DECSCUSR) ---------------------------------

  "hardwareCursor" should "be present, delegating caret control to the terminal's own cursor" in {
    val (rs, _) = surface()
    rs.hardwareCursor shouldBe defined
  }

  it should "hide the terminal cursor on the first flush before any caret has been presented" in {
    val (rs, writer) = surface()
    rs.flush()
    writer.toString should include(s"$esc[?25l")
  }

  it should "position, style, and show the terminal cursor via CUP, DECSCUSR, then DECTCEM show, on present" in {
    val (rs, writer) = surface()
    rs.hardwareCursor.get.present(3, 2, HardwareCursorStyle(HardwareCursorShape.Block, blinking = true))
    rs.flush()

    val out = writer.toString
    out should include(s"$esc[3;4H") // CUP is row+1;col+1 -- row=2,col=3 here
    out should include(s"$esc[1 q")  // DECSCUSR: blinking block
    out should include(s"$esc[?25h") // DECTCEM show
    out.indexOf(s"$esc[3;4H") should be < out.indexOf(s"$esc[1 q")
    out.indexOf(s"$esc[1 q") should be < out.indexOf(s"$esc[?25h")
  }

  it should "map every DECSCUSR shape/blink combination onto real ANSI output" in {
    val cases = List(
      HardwareCursorStyle(HardwareCursorShape.Block, blinking = true)      -> 1,
      HardwareCursorStyle(HardwareCursorShape.Block, blinking = false)     -> 2,
      HardwareCursorStyle(HardwareCursorShape.Underline, blinking = true)  -> 3,
      HardwareCursorStyle(HardwareCursorShape.Underline, blinking = false) -> 4,
      HardwareCursorStyle(HardwareCursorShape.Bar, blinking = true)        -> 5,
      HardwareCursorStyle(HardwareCursorShape.Bar, blinking = false)       -> 6
    )
    cases.foreach { (style, param) =>
      val (rs, writer) = surface()
      rs.hardwareCursor.get.present(0, 0, style)
      rs.flush()
      writer.toString should include(s"$esc[$param q")
    }
  }

  it should "emit no caret escape on a later flush when the caret state has not changed" in {
    val (rs, writer) = surface()
    rs.hardwareCursor.get.present(1, 1, HardwareCursorStyle(HardwareCursorShape.Block, blinking = true))
    rs.flush()
    writer.getBuffer.setLength(0)

    rs.flush() // nothing changed -- no content damage, no caret movement

    writer.toString shouldBe ""
  }

  it should "reposition on a later flush when only the caret cell moved, with no content damage" in {
    val (rs, writer) = surface()
    rs.hardwareCursor.get.present(1, 1, HardwareCursorStyle(HardwareCursorShape.Block, blinking = true))
    rs.flush()
    writer.getBuffer.setLength(0)

    rs.hardwareCursor.get.present(5, 1, HardwareCursorStyle(HardwareCursorShape.Block, blinking = true))
    rs.flush()

    writer.toString should include(s"$esc[2;6H")
  }

  it should "hide the terminal cursor again when hide() is called after a present" in {
    val (rs, writer) = surface()
    rs.hardwareCursor.get.present(1, 1, HardwareCursorStyle(HardwareCursorShape.Block, blinking = true))
    rs.flush()
    writer.getBuffer.setLength(0)

    rs.hardwareCursor.get.hide()
    rs.flush()

    writer.toString should include(s"$esc[?25l")
  }

  // Issue #1215: a real TUI session's cursor drifted to a fixed, wrong screen position and stayed there instead of
  // tracking the caret. `TerminalAnsiDiff`'s own CUP writes leave the terminal's real cursor wherever a content
  // diff's last cell was drawn -- entirely independent of the caret -- so a flush whose content changed (an
  // animation tick, a status-bar refresh, anything that doesn't itself move the caret) must still re-assert the
  // caret's own CUP even though the caret's logical target didn't change, or the terminal's visible cursor is left
  // wherever that unrelated content write dragged it.
  it should "reassert the caret's CUP on a later flush where unrelated content changed but the caret itself did not" in {
    val (rs, writer) = surface()
    rs.hardwareCursor.get.present(3, 2, HardwareCursorStyle(HardwareCursorShape.Block, blinking = true))
    rs.flush()
    writer.getBuffer.setLength(0)

    rs.putString(7, 4, "x") // content elsewhere on screen changes; the caret's own target is untouched
    rs.flush()

    val out = writer.toString
    out should include(s"$esc[3;4H")                       // CUP is row+1;col+1 -- row=2,col=3, still the caret's cell
    out.indexOf("x") should be < out.indexOf(s"$esc[3;4H") // the caret CUP lands after the content write, last word
  }

  "Renderer.renderWithCursorOverlay (surface-generic)" should
    "delegate the caret to the terminal's own cursor instead of painting it as cell content, in blink mode" in {
      val (rs, writer) = surface(width = 80, height = 24)
      val state        = editorState(cursorMode = CursorMode.Blink)
      val font         = new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
      val cellMetrics  = CellMetrics(charWidth = 1, lineHeight = 1, ascent = 0)

      Renderer.renderWithCursorOverlay(
        state,
        rs,
        ViewportSize(80, 24),
        font,
        font,
        font,
        cellMetrics,
        cellMetrics,
        None
      )

      val output = writer.toString
      output should include(s"$esc[?25h") // DECTCEM show: caret delegated to the terminal
      output should include(s"$esc[1 q")  // DECSCUSR: default blinking block
    }

  it should "hide the terminal cursor in breathe mode instead -- breathe stays the app-painted exception" in {
    val (rs, writer) = surface(width = 80, height = 24)
    val state        = editorState(cursorMode = CursorMode.Breathe)
    val font         = new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
    val cellMetrics  = CellMetrics(charWidth = 1, lineHeight = 1, ascent = 0)

    Renderer.renderWithCursorOverlay(state, rs, ViewportSize(80, 24), font, font, font, cellMetrics, cellMetrics, None)

    writer.toString should include(s"$esc[?25l")
  }

  // -- #1172: DEC 2026 synchronized updates bracketing a flush's whole emission ---------------------------------------

  "flush" should "bracket a content-changing flush's whole emission in DEC 2026 synchronized-update begin/end" in {
    val (rs, writer) = surface()
    rs.putString(0, 0, "hi")
    rs.flush()

    val out = writer.toString
    out should startWith(s"$esc[?2026h")
    out should endWith(s"$esc[?2026l")
  }

  it should "bracket a caret-only flush (no content damage) in the same synchronized-update begin/end" in {
    val (rs, writer) = surface()
    rs.hardwareCursor.get.present(1, 1, HardwareCursorStyle(HardwareCursorShape.Block, blinking = true))
    rs.flush()
    writer.getBuffer.setLength(0)

    rs.hardwareCursor.get.present(5, 1, HardwareCursorStyle(HardwareCursorShape.Block, blinking = true))
    rs.flush()

    val out = writer.toString
    out should startWith(s"$esc[?2026h")
    out should endWith(s"$esc[?2026l")
  }

  it should "emit no synchronized-update brackets on a flush with no content damage and no caret movement" in {
    val (rs, writer) = surface()
    rs.hardwareCursor.get.present(1, 1, HardwareCursorStyle(HardwareCursorShape.Block, blinking = true))
    rs.flush()
    writer.getBuffer.setLength(0)

    rs.flush() // nothing changed

    writer.toString should not include s"$esc[?2026h"
    writer.toString shouldBe ""
  }

  // -- #1215 (remainder): TUI cell metrics must be honoured end-to-end, not re-derived from the real AWT font ---------

  "Renderer.renderWithCursorOverlay (surface-generic)" should
    "place the CUP escape at the cursor's real cell column on a line longer than a few characters" in {
      val (rs, writer) = surface(width = 80, height = 24)
      val font         = new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
      val cellMetrics  = CellMetrics(charWidth = 1, lineHeight = 1, ascent = 0)
      val paneId       = PaneId(0)
      val bufferId     = BufferId(1)
      val cursorColumn = 15
      val baseBuffer   = Buffer.fromString(bufferId, "Hello, cursor tracking world!")
      val buffer = baseBuffer.copy(editing = baseBuffer.editing.copy(cursors = List(CursorPosition(0, cursorColumn))))
      val state = AppState.initial.copy(
        persisted = AppState.initial.persisted.copy(
          config = AppState.initial.persisted.config
            .withCursorMode(CursorMode.Blink)
            .withLineNumbers(false)
            .withGutter(false)
            .withPaneHeaders(false),
          buffers = Map(buffer.id -> buffer),
          bufferOrder = List(buffer.id),
          layout = AppState.initial.persisted.layout.copy(
            editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
            activeEditorPaneId = Some(paneId),
            paneOrder = List(paneId)
          ),
          focus = Focus.EditorPane(paneId),
          theme = Theme.light
        )
      )

      Renderer.renderWithCursorOverlay(
        state,
        rs,
        ViewportSize(80, 24),
        font,
        font,
        font,
        cellMetrics,
        cellMetrics,
        None
      )

      val out = writer.toString
      // CUP is row+1;col+1: with CellMetricsOne (1 pixel == 1 cell) honoured end-to-end, column 15 must land at
      // screen column 16, not at the ~7x-inflated (and then edge-clamped) column a re-derived `CellMetrics.fromFont`
      // (charWidth=7 for this font in this environment) would produce.
      out should include(s"$esc[1;16H")
    }

  it should "place the CUP escape at the cursor's real cell row across multiple lines" in {
    val (rs, writer) = surface(width = 80, height = 24)
    val font         = new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
    val cellMetrics  = CellMetrics(charWidth = 1, lineHeight = 1, ascent = 0)
    val paneId       = PaneId(0)
    val bufferId     = BufferId(1)
    val cursorLine   = 3
    val cursorColumn = 5
    val text         = (0 until 6).map(n => s"line number $n").mkString("\n")
    val baseBuffer   = Buffer.fromString(bufferId, text)
    val buffer =
      baseBuffer.copy(editing = baseBuffer.editing.copy(cursors = List(CursorPosition(cursorLine, cursorColumn))))
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppState.initial.persisted.config
          .withCursorMode(CursorMode.Blink)
          .withLineNumbers(false)
          .withGutter(false)
          .withPaneHeaders(false),
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          paneOrder = List(paneId)
        ),
        focus = Focus.EditorPane(paneId),
        theme = Theme.light
      )
    )

    Renderer.renderWithCursorOverlay(
      state,
      rs,
      ViewportSize(80, 24),
      font,
      font,
      font,
      cellMetrics,
      cellMetrics,
      None
    )

    val out = writer.toString
    // Row 3 (0-indexed) must land at screen row 4 -- not row 2, which is what a hard-coded 2px "optical lift"
    // designed for a real sub-pixel font's line height produces once it is applied inside a 1px == 1 terminal-row
    // grid (CellMetricsOne's lineHeight=1).
    out should include(s"$esc[4;6H")
  }

  private def editorState(cursorMode: CursorMode): AppState =
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, "hello terminal")
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppState.initial.persisted.config.withCursorMode(cursorMode),
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          paneOrder = List(paneId)
        ),
        focus = Focus.EditorPane(paneId),
        theme = Theme.light
      )
    )
