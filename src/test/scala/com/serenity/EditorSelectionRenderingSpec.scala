package com.serenity

import java.awt.Font
import java.nio.file.Path

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
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

  it should "span and center the active buffer header across the workspace row" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromFile(bufferId, Path.of("notes.md"), "alpha")
    val pane     = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = com.serenity.config.AppConfig.default.copy(showLineNumbers = false)
    )
    val viewport = ViewportSize(80, 24)
    val surface  = new MockRenderSurface(viewport.width, viewport.height)

    Renderer.render(state, cursorVisible = false, surface, viewport)

    val title       = "notes.md"
    val cellMetrics = CellMetrics.fromFont(Font(Font.MONOSPACED, Font.PLAIN, 12))
    val uiFont      = Font(Font.SANS_SERIF, Font.PLAIN, 12).deriveFont(12.0f)
    val expectedPlacement = TextAlignment.placeLine(
      title,
      TextAreaPx(
        xPx = 0.0f,
        yPx = 0,
        widthPx = viewport.width * cellMetrics.charWidth.toFloat,
        heightPx = cellMetrics.lineHeight
      ),
      uiFont,
      cellMetrics.lineHeight,
      cellMetrics.ascent,
      TextHorizontalAlignment.Center,
      TextVerticalAlignment.Top,
      surface.fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())
    )
    val titleDraw = surface.drawRunPxCalls.find(_.s == title).getOrElse(fail("Expected measured title draw call"))

    surface.getBg(0, 0) shouldBe state.theme.highlighted.background
    surface.getBg(viewport.width - 1, 0) shouldBe state.theme.highlighted.background
    titleDraw.xPx shouldBe expectedPlacement.xPx +- 0.001f
    titleDraw.yPx shouldBe expectedPlacement.yPx
  }

  it should "center the active buffer header across the workspace row when line numbers are visible" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromFile(bufferId, Path.of("notes.md"), "alpha\nbeta\ngamma")
    val pane     = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = com.serenity.config.AppConfig.default.copy(showLineNumbers = true)
    )
    val viewport = ViewportSize(80, 24)
    val surface  = new MockRenderSurface(viewport.width, viewport.height)
    val layout   = LayoutEngine.calculateLayout(state, viewport)

    Renderer.render(state, cursorVisible = false, surface, viewport)

    val title = "notes.md"
    val headerRects = List(
      Some(layout.leftSpacerRect),
      layout.lineNumberRect,
      Some(layout.editorPanelRect),
      Some(layout.rightSpacerRect)
    ).flatten
    val headerLeft  = headerRects.map(_.x).min
    val headerRight = headerRects.map(_.right).max
    val cellMetrics = CellMetrics.fromFont(Font(Font.MONOSPACED, Font.PLAIN, 12))
    val uiFont      = Font(Font.SANS_SERIF, Font.PLAIN, 12).deriveFont(12.0f)
    val expectedPlacement = TextAlignment.placeLine(
      title,
      TextAreaPx(
        xPx = cellMetrics.toPixelX(headerLeft).toFloat,
        yPx = cellMetrics.toPixelY(0),
        widthPx = (headerRight - headerLeft) * cellMetrics.charWidth.toFloat,
        heightPx = cellMetrics.lineHeight
      ),
      uiFont,
      cellMetrics.lineHeight,
      cellMetrics.ascent,
      TextHorizontalAlignment.Center,
      TextVerticalAlignment.Top,
      surface.fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())
    )
    val titleDraw = surface.drawRunPxCalls.find(_.s == title).getOrElse(fail("Expected measured title draw call"))

    titleDraw.xPx shouldBe expectedPlacement.xPx +- 0.001f
    titleDraw.yPx shouldBe expectedPlacement.yPx
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

  it should "not repaint document rows for passive editor hover" in {
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
      config = com.serenity.config.AppConfig.default
        .withSyntaxHighlighting(false)
        .withLineNumbers(false)
        .withGutter(false)
    )

    val surface = new MockRenderSurface(100, 30)

    Renderer.render(state, cursorVisible = false, surface, ViewportSize(100, 30))

    val hoveredLineBackgrounds = (0 until surface.width).count(x => surface.getBg(x, 2) == state.theme.panel.background)

    hoveredLineBackgrounds shouldBe 0
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

    val surface           = new MockRenderSurface(100, 30)
    val commentBackground = Renderer.commentHighlightBackground(state.theme)

    Renderer.render(state, cursorVisible = false, surface, ViewportSize(100, 30))

    val commentCells = for
      x <- 0 until surface.width
      if "beta".contains(surface.getChar(x, 1))
      if surface.getBg(x, 1) == commentBackground
    yield x

    commentCells should have size 4
    commentCells.foreach(x => surface.getBg(x, 1) should not be state.theme.highlighted.background)
    commentCells.foreach(x => surface.getBg(x, 1) should not be state.theme.warning.background)
  }

  it should "keep active authored document comments readable with the document foreground" in {
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

    val surface           = new MockRenderSurface(100, 30)
    val commentBackground = Renderer.commentHighlightBackground(state.theme)

    Renderer.render(state, cursorVisible = false, surface, ViewportSize(100, 30))

    val activeCommentLetters = for
      x <- 0 until surface.width
      if "beta".contains(surface.getChar(x, 1))
      if surface.getBg(x, 1) == commentBackground
    yield surface.getFg(x, 1)

    activeCommentLetters should have size 4
    activeCommentLetters.distinct shouldBe IndexedSeq(state.theme.foreground)
    activeCommentLetters.distinct should not be IndexedSeq(state.theme.warning.foreground)
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

    val surface           = new MockRenderSurface(100, 30)
    val commentBackground = Renderer.commentHighlightBackground(state.theme)

    Renderer.render(state, cursorVisible = false, surface, ViewportSize(100, 30))

    val highlightedRuns = surface.drawRunPxCalls.filter(_.background == commentBackground).map(_.s)

    highlightedRuns should contain("line")
    highlightedRuns should contain("second line")
    highlightedRuns should contain("third")
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

    val surface           = new MockRenderSurface(100, 30)
    val commentBackground = Renderer.commentHighlightBackground(state.theme)

    Renderer.render(state, cursorVisible = false, surface, ViewportSize(100, 30))

    val alphaStart = (0 until surface.width)
      .find(x => surface.getRow(1).drop(x).startsWith("alpha"))
      .getOrElse(
        fail("Expected rendered buffer text")
      )

    surface.getBg(alphaStart + 5, 1) shouldBe commentBackground
    surface.getBg(alphaStart + 5, 1) should not be state.theme.warning.background
  }
