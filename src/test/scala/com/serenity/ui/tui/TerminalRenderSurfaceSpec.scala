package com.serenity.ui.tui

import java.awt.Color
import java.io.StringWriter

import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, PixelRect, ViewportSize}
import com.serenity.ui.renderer.Renderer
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
    writer.toString should startWith(s"$esc[2J$esc[H") // first flush has no previous frame: full repaint
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
    output should startWith(s"$esc[2J$esc[H") // first frame: full clear-and-repaint
    // The gutter's line number reaches the screen through `putString` (a real cell-addressed draw this surface
    // performs), confirming the frame actually painted through to the terminal rather than producing an empty diff.
    output should include("1")
    // Proportional buffer prose is painted through `drawRunPx`, which this surface -- correctly, per #1107's scope --
    // leaves a deliberate no-op (`fontRenderContext = None`) until #1105 wires the cell-fallback path elsewhere. So
    // the buffer's own text does not reach the screen yet; that gap is #1105's, not this spec's, to close.
    output should not include "hello terminal"
  }
