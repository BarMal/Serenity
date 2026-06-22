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

  it should "highlight authored document comment ranges without using the selection colour" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer = Buffer
      .fromString(bufferId, "alpha beta gamma")
      .copy(
        cursors = List(CursorPosition(0, 0)),
        documentComments = List(DocumentComment(CursorPosition(0, 6), CursorPosition(0, 10), "Review this"))
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

    val commentCells = for
      x <- 0 until surface.width
      if "beta".contains(surface.getChar(x, 1))
      if surface.getBg(x, 1) == state.theme.warning.background
    yield x

    commentCells should have size 4
    commentCells.foreach(x => surface.getBg(x, 1) should not be state.theme.highlighted.background)
  }

  it should "use the warning foreground for the active authored document comment range" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer = Buffer
      .fromString(bufferId, "alpha beta gamma")
      .copy(
        cursors = List(CursorPosition(0, 7)),
        documentComments = List(DocumentComment(CursorPosition(0, 6), CursorPosition(0, 10), "Review this"))
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

    val activeCommentLetters = for
      x <- 0 until surface.width
      if "beta".contains(surface.getChar(x, 1))
      if surface.getBg(x, 1) == state.theme.warning.background
    yield surface.getFg(x, 1)

    activeCommentLetters should have size 4
    activeCommentLetters.distinct shouldBe IndexedSeq(state.theme.warning.foreground)
  }

  it should "highlight authored document comments across multiple lines" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer = Buffer
      .fromString(bufferId, "first line\nsecond line\nthird line")
      .copy(
        cursors = List(CursorPosition(0, 0)),
        documentComments = List(DocumentComment(CursorPosition(0, 6), CursorPosition(2, 5), "Review this section"))
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

    val firstLineHighlights  = (0 until surface.width).count(x => surface.getBg(x, 1) == state.theme.warning.background)
    val secondLineHighlights = (0 until surface.width).count(x => surface.getBg(x, 2) == state.theme.warning.background)
    val thirdLineHighlights  = (0 until surface.width).count(x => surface.getBg(x, 3) == state.theme.warning.background)

    firstLineHighlights shouldBe 4
    secondLineHighlights shouldBe 11
    thirdLineHighlights shouldBe 5
  }

  it should "show a marker for zero-length authored document comments" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer = Buffer
      .fromString(bufferId, "alpha")
      .copy(
        cursors = List(CursorPosition(0, 0)),
        documentComments = List(DocumentComment(CursorPosition(0, 5), CursorPosition(0, 5), "Point note"))
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

    val alphaStart = (0 until surface.width)
      .find(x => surface.getRow(1).drop(x).startsWith("alpha"))
      .getOrElse(
        fail("Expected rendered buffer text")
      )

    surface.getBg(alphaStart + 5, 1) shouldBe state.theme.warning.background
  }
