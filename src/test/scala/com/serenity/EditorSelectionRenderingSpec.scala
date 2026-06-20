package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{Layout, ViewportSize}
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EditorSelectionRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Renderer.render" should "highlight selected buffer text in the editor pane" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer = Buffer
      .fromString(bufferId, "Hello World")
      .copy(
        cursors = List(CursorPosition(0, 6)),
        selection = Some(Selection(CursorPosition(0, 6), CursorPosition(0, 11)))
      )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = com.serenity.config.AppConfig.default.withSyntaxHighlighting(false)
    )

    val surface = new MockRenderSurface(100, 30)

    Renderer.render(state, cursorVisible = false, surface, ViewportSize(100, 30))

    val selectedCells = for
      x <- 0 until surface.width
      if "World".contains(surface.getChar(x, 1))
      if surface.getBg(x, 1) == state.theme.highlighted.background
    yield x

    selectedCells should have size 5
  }

  it should "highlight every active selection in the editor pane" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val first    = Selection(CursorPosition(0, 0), CursorPosition(0, 5))
    val second   = Selection(CursorPosition(0, 11), CursorPosition(0, 16))
    val buffer = Buffer
      .fromString(bufferId, "alpha beta gamma")
      .copy(
        cursors = List(first.focus, second.focus),
        selection = Some(first),
        selections = List(first, second)
      )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = com.serenity.config.AppConfig.default.withSyntaxHighlighting(false)
    )

    val surface = new MockRenderSurface(100, 30)

    Renderer.render(state, cursorVisible = false, surface, ViewportSize(100, 30))

    val highlightedLetters = for
      x <- 0 until surface.width
      if surface.getBg(x, 1) == state.theme.highlighted.background
    yield surface.getChar(x, 1)

    highlightedLetters.mkString shouldBe "alphagamma"
  }

  it should "paint a subtle background on the hovered editor line" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, "alpha\nbeta")
    val pane     = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      hoveredEditorTarget = Some(HoveredEditorTarget(paneId, bufferId, CursorPosition(1, 0))),
      theme = Theme.light,
      config = com.serenity.config.AppConfig.default.withSyntaxHighlighting(false)
    )

    val surface = new MockRenderSurface(100, 30)

    Renderer.render(state, cursorVisible = false, surface, ViewportSize(100, 30))

    val hoveredLineBackgrounds = (0 until surface.width).count(x => surface.getBg(x, 2) == state.theme.panel.background)

    hoveredLineBackgrounds should be > 0
  }
