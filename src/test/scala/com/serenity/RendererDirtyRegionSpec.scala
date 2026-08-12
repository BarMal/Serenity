package com.serenity

import com.serenity.state.models.*
import com.serenity.ui.layout.{PixelRect, ViewportSize}
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** End-to-end cover for dirty-line rendering of editor pane text: a surface that keeps its pixels between frames must
  * only be asked to draw the rows that actually changed, and everything else must still be drawn exactly as before.
  */
class RendererDirtyRegionSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)
  private val viewport = ViewportSize(80, 24)

  private val lines = Vector("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta")

  private def stateWith(content: Vector[String], cursor: CursorPosition = CursorPosition(0, 0)): AppState =
    val buffer = Buffer.fromString(bufferId, content.mkString("\n")).copy(cursors = List(cursor))
    AppState.initial.copy(
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = AppState.initial.layout.copy(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      theme = Theme.light
    )

  /** Every string the frame asked the surface to paint, whichever text path it took. */
  private def drawnText(surface: MockRenderSurface): List[String] =
    surface.putStringCalls.map(_.s) ++ surface.drawRunPxCalls.map(_.s)

  private def drew(surface: MockRenderSurface, text: String): Boolean =
    drawnText(surface).exists(_.contains(text))

  "Dirty-line rendering" should "draw every visible row on the first frame" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    Renderer.render(stateWith(lines), cursorVisible = false, surface, viewport)

    lines.foreach(line => drew(surface, line) shouldBe true)
  }

  it should "draw no pane rows again when nothing changed" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val state   = stateWith(lines)

    Renderer.render(state, cursorVisible = false, surface, viewport)
    surface.clear()
    Renderer.render(state, cursorVisible = false, surface, viewport)

    lines.foreach(line => drew(surface, line) shouldBe false)
  }

  it should "draw pane rows again when the surface does not preserve its pixels" in {
    val surface = new MockRenderSurface(80, 24)
    val state   = stateWith(lines)

    Renderer.render(state, cursorVisible = false, surface, viewport)
    surface.clear()
    Renderer.render(state, cursorVisible = false, surface, viewport)

    lines.foreach(line => drew(surface, line) shouldBe true)
  }

  it should "draw only the edited row again when one line changes" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)

    Renderer.render(stateWith(lines), cursorVisible = false, surface, viewport)
    surface.clear()
    Renderer.render(stateWith(lines.updated(5, "zetaX")), cursorVisible = false, surface, viewport)

    drew(surface, "zetaX") shouldBe true
    drew(surface, "alpha") shouldBe false
    drew(surface, "gamma") shouldBe false
  }

  it should "draw the rows the caret left and entered again when it moves" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)

    Renderer.render(stateWith(lines, CursorPosition(0, 0)), cursorVisible = true, surface, viewport)
    surface.clear()
    Renderer.render(stateWith(lines, CursorPosition(5, 0)), cursorVisible = true, surface, viewport)

    drew(surface, "alpha") shouldBe true
    drew(surface, "zeta") shouldBe true
    drew(surface, "gamma") shouldBe false
  }

  it should "draw the rows a new selection covers again" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val state   = stateWith(lines)
    val selected = state.copy(
      buffers = state.buffers.view
        .mapValues(_.copy(selection = Some(Selection(CursorPosition(5, 0), CursorPosition(5, 4)))))
        .toMap
    )

    Renderer.render(state, cursorVisible = false, surface, viewport)
    surface.clear()
    Renderer.render(selected, cursorVisible = false, surface, viewport)

    drew(surface, "zeta") shouldBe true
  }

  it should "draw every row again when the theme changes" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val state   = stateWith(lines)

    Renderer.render(state, cursorVisible = false, surface, viewport)
    surface.clear()
    Renderer.render(state.copy(theme = Theme.default), cursorVisible = false, surface, viewport)

    lines.foreach(line => drew(surface, line) shouldBe true)
  }

  it should "still draw the gutter and line numbers on an otherwise unchanged frame" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val state   = stateWith(lines)

    Renderer.render(state, cursorVisible = false, surface, viewport)
    surface.clear()
    Renderer.render(state, cursorVisible = false, surface, viewport)

    drawnText(surface).mkString should include("Line 1, Col 1")
  }

  "The repaint region" should "cover the whole canvas for the first frame" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)

    repaintRegionFor(surface, stateWith(lines)) shouldBe None
  }

  it should "be empty when the frame is identical to the one on screen" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val state   = stateWith(lines)

    val _ = repaintRegionFor(surface, state)

    repaintRegionFor(surface, state) shouldBe Some(PixelRect(0, 0, 0, 0))
  }

  it should "cover only the edited row when one line changes" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    val state   = stateWith(lines)
    // Keep every other state object identical, exactly as an edit does in the app: only the buffer is rebuilt, so the
    // chrome around the pane is provably unchanged and the repaint can stay bounded.
    val edited = state.copy(
      buffers = state.buffers.updated(bufferId, Buffer.fromString(bufferId, lines.updated(5, "zetaX").mkString("\n")))
    )

    val _      = repaintRegionFor(surface, state)
    val region = repaintRegionFor(surface, edited)

    region.map(_.heightPx).getOrElse(0) should be > 0
    region.map(_.heightPx).getOrElse(0) should be < viewport.height * 16
  }

  it should "cover the whole canvas when the chrome changed too" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)

    val _ = repaintRegionFor(surface, stateWith(lines, CursorPosition(0, 0)))

    repaintRegionFor(surface, stateWith(lines, CursorPosition(3, 2))) shouldBe None
  }

  private def repaintRegionFor(surface: MockRenderSurface, state: AppState): Option[PixelRect] =
    val font = new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
    Renderer.renderWithRepaintRegion(
      state,
      cursorVisible = false,
      surface,
      viewport,
      font,
      font,
      font,
      com.serenity.ui.layout.CellMetrics.fromFont(font),
      com.serenity.ui.layout.CellMetrics.fromFont(font),
      None
    )
