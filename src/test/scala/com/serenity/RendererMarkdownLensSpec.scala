package com.serenity

import java.awt.Font

import com.serenity.config.MarkdownViewMode
import com.serenity.lsp.config.LanguageId
import com.serenity.markdown.MarkdownDocumentPreview
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.Renderer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererMarkdownLensSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Renderer markdown lens" should "render the active markdown block as raw source" in {
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val buffer = Buffer
      .fromString(bufferId, "# Lens\n\n# Raw\ncontinued")
      .copy(
        language = Some(LanguageId.Markdown),
        cursors = List(CursorPosition(2, 0)),
        viewport = Viewport.default.copy(visibleLines = 10)
      )
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      config = AppState.empty.config
        .withSyntaxHighlighting(true)
        .withLineNumbers(false)
        .withGutter(false)
        .withMarkdownViewMode(MarkdownViewMode.InlineLens)
    )
    val surface = new MockRenderSurface(80, 24)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(80, 24),
      codeFont = font,
      textFont = font,
      cellMetrics = CellMetrics.fromFont(font),
      cursorColor = None
    )

    val renderedRows = rows(surface)
    surface.drawImageCalls should have size 1
    renderedRows.exists(_.contains("# Raw")) shouldBe true
    renderedRows.exists(_.contains("continued")) shouldBe false
  }

  it should "render markdown preview as the inline lens base when the cursor is outside the viewport" in {
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val buffer = Buffer
      .fromString(bufferId, "# One\n# Two")
      .copy(
        language = Some(LanguageId.Markdown),
        cursors = List(CursorPosition(10, 0)),
        viewport = Viewport.default.copy(visibleLines = 10)
      )
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      config = AppState.empty.config
        .withSyntaxHighlighting(true)
        .withLineNumbers(false)
        .withGutter(false)
        .withMarkdownViewMode(MarkdownViewMode.InlineLens)
    )
    val surface = new MockRenderSurface(80, 24)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(80, 24),
      codeFont = font,
      textFont = font,
      cellMetrics = CellMetrics.fromFont(font),
      cursorColor = None
    )

    surface.drawImageCalls should have size 1
    rows(surface).exists(_.contains("# One")) shouldBe false
    rows(surface).exists(_.contains("# Two")) shouldBe false
  }

  it should "keep an inactive heading in the first editor row when focus moves to the following blank line" in {
    val (state, surface, metrics) = renderMarkdownLens(
      "# Hello world!\n\nParagraph",
      CursorPosition(1, 0)
    )

    val image = surface.drawImageCalls.head.image
    val firstContentRow = (0 until image.getHeight)
      .find(row =>
        (0 until image.getWidth).exists(column => image.getRGB(column, row) != state.theme.background.getRGB)
      )

    firstContentRow should not be empty
    firstContentRow.getOrElse(fail("Expected rendered heading pixels")) should be < metrics.lineHeight
  }

  it should "render every active cursor markdown block as raw source" in {
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val buffer = Buffer
      .fromString(bufferId, "# First\nfirst body\n\n# Second\nsecond body\n\n# Third")
      .copy(
        language = Some(LanguageId.Markdown),
        cursors = List(CursorPosition(0, 0), CursorPosition(3, 0)),
        viewport = Viewport.default.copy(visibleLines = 10)
      )
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      config = AppState.empty.config
        .withSyntaxHighlighting(true)
        .withLineNumbers(false)
        .withGutter(false)
        .withMarkdownViewMode(MarkdownViewMode.InlineLens)
    )
    val surface = new MockRenderSurface(80, 24)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(80, 24),
      codeFont = font,
      textFont = font,
      cellMetrics = CellMetrics.fromFont(font),
      cursorColor = None
    )

    val renderedRows = rows(surface)
    surface.drawImageCalls should have size 1
    renderedRows.exists(_.contains("# First")) shouldBe true
    renderedRows.exists(_.contains("first body")) shouldBe false
    renderedRows.exists(_.contains("# Second")) shouldBe true
    renderedRows.exists(_.contains("second body")) shouldBe false
    renderedRows.exists(_.contains("# Third")) shouldBe false
  }

  it should "not pull an adjacent heading into a raw paragraph lens when there is no blank line" in {
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val buffer = Buffer
      .fromString(bufferId, "# Title\nParagraph immediately after the heading")
      .copy(
        language = Some(LanguageId.Markdown),
        cursors = List(CursorPosition(1, 0)),
        viewport = Viewport.default.copy(visibleLines = 10)
      )
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      config = AppState.empty.config
        .withSyntaxHighlighting(true)
        .withLineNumbers(false)
        .withGutter(false)
        .withMarkdownViewMode(MarkdownViewMode.InlineLens)
    )
    val surface = new MockRenderSurface(80, 24)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(80, 24),
      codeFont = font,
      textFont = font,
      cellMetrics = CellMetrics.fromFont(font),
      cursorColor = None
    )

    val renderedRows = rows(surface)
    surface.drawImageCalls should have size 1
    renderedRows.exists(_.contains("Paragraph immediately after the heading")) shouldBe true
  }

  it should "draw a visible border around the raw source lens" in {
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val buffer = Buffer
      .fromString(bufferId, "# Preview\n\nRaw paragraph\ncontinued")
      .copy(
        language = Some(LanguageId.Markdown),
        cursors = List(CursorPosition(2, 0)),
        viewport = Viewport.default.copy(visibleLines = 10)
      )
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      config = AppState.empty.config
        .withSyntaxHighlighting(true)
        .withLineNumbers(false)
        .withGutter(false)
        .withMarkdownViewMode(MarkdownViewMode.InlineLens)
    )
    val surface = new MockRenderSurface(80, 24)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(80, 24),
      codeFont = font,
      textFont = font,
      cellMetrics = CellMetrics.fromFont(font),
      cursorColor = None
    )

    surface.strokeRoundRectCalls should not be empty
    val border = surface.strokeRoundRectCalls.head
    border.color shouldBe state.theme.border
    val paneRect =
      LayoutEngine.calculatePaneLayouts(state, LayoutEngine.calculateLayout(state, ViewportSize(80, 24)))(paneId)
    border.x shouldBe paneRect.x
    border.w shouldBe paneRect.width
    border.h shouldBe 2
  }

  it should "size an active table lens to cover the rendered preview table" in {
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val buffer = Buffer
      .fromString(
        bufferId,
        """Before
          || Task | Owner |
          || ---- | ----- |
          || Ship | Codex |
          |After""".stripMargin
      )
      .copy(
        language = Some(LanguageId.Markdown),
        cursors = List(CursorPosition(1, 0)),
        viewport = Viewport.default.copy(visibleLines = 10)
      )
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      config = AppState.empty.config
        .withSyntaxHighlighting(true)
        .withLineNumbers(false)
        .withGutter(false)
        .withMarkdownViewMode(MarkdownViewMode.InlineLens)
    )
    val surface = new MockRenderSurface(80, 24)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(80, 24),
      codeFont = font,
      textFont = font,
      cellMetrics = metrics,
      cursorColor = None
    )

    surface.strokeRoundRectCalls should not be empty
    surface.strokeRoundRectCalls.head.h shouldBe 5
  }

  it should "align the raw source lens to the scrolled preview window" in {
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val buffer = Buffer
      .fromString(
        bufferId,
        """# Intro
          |
          |Opening
          |
          |Before
          || Task | Owner |
          || ---- | ----- |
          || Ship | Codex |
          |
          |After""".stripMargin
      )
      .copy(
        language = Some(LanguageId.Markdown),
        cursors = List(CursorPosition(6, 0)),
        viewport = Viewport.default.copy(topLine = 5, visibleLines = 10)
      )
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      config = AppState.empty.config
        .withSyntaxHighlighting(true)
        .withLineNumbers(false)
        .withGutter(false)
        .withMarkdownViewMode(MarkdownViewMode.InlineLens)
    )
    val surface = new MockRenderSurface(80, 24)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(80, 24),
      codeFont = font,
      textFont = font,
      cellMetrics = metrics,
      cursorColor = None
    )

    val paneRect =
      LayoutEngine.calculatePaneLayouts(state, LayoutEngine.calculateLayout(state, ViewportSize(80, 24)))(paneId)
    surface.strokeRoundRectCalls should not be empty
    surface.strokeRoundRectCalls.head.y shouldBe paneRect.y + 1
  }

  it should "align heading paragraph list and table lenses to the active preview block" in {
    val cases = List(
      (
        "heading",
        "# Heading\n\nParagraph",
        CursorPosition(0, 0),
        1
      ),
      (
        "paragraph",
        "# Heading\n\nParagraph line\ncontinued",
        CursorPosition(2, 0),
        2
      ),
      (
        "list",
        "# Heading\n\n- one\n  detail\n- two",
        CursorPosition(3, 0),
        3
      ),
      (
        "table",
        "# Heading\n\n| Task | Owner |\n| ---- | ----- |\n| Ship | Codex |",
        CursorPosition(3, 0),
        5
      )
    )

    cases.foreach {
      case (name, source, cursor, expectedHeightRows) =>
        val (state, surface, metrics) = renderMarkdownLens(source, cursor)
        val paneRect =
          LayoutEngine.calculatePaneLayouts(state, LayoutEngine.calculateLayout(state, ViewportSize(80, 24)))(
            PaneId(1)
          )

        withClue(s"$name lens: ") {
          surface.strokeRoundRectCalls should not be empty
          val border = surface.strokeRoundRectCalls.head
          border.y shouldBe paneRect.y + 1
          border.h shouldBe expectedHeightRows
        }
    }
  }

  it should "keep visible preview context above the active lens" in {
    val (state, surface, metrics) = renderMarkdownLens(
      "# Intro\n\nOpening paragraph\n\nActive paragraph\ncontinued",
      CursorPosition(4, 0),
      topLine = Some(0)
    )
    val paneRect =
      LayoutEngine.calculatePaneLayouts(state, LayoutEngine.calculateLayout(state, ViewportSize(80, 24)))(
        PaneId(1)
      )

    val renderedRows = rows(surface)
    surface.strokeRoundRectCalls should not be empty
    val border = surface.strokeRoundRectCalls.head
    renderedRows.exists(_.contains("Active paragraph")) shouldBe true
    renderedRows.exists(_.contains("continued")) shouldBe true
    border.y should be > paneRect.y + 1
    border.h shouldBe 2
  }

  it should "align the active lens after expanded preview context" in {
    val source =
      "| Task | Owner |\n| ---- | ----- |\n| Ship | Codex |\n\nActive paragraph\ncontinued"
    val cursor                    = CursorPosition(4, 0)
    val (state, surface, metrics) = renderMarkdownLens(source, cursor, topLine = Some(0))
    val paneRect =
      LayoutEngine.calculatePaneLayouts(state, LayoutEngine.calculateLayout(state, ViewportSize(80, 24)))(
        PaneId(1)
      )

    val lines = source.linesIterator.toVector
    val previewWindow = MarkdownDocumentPreview.PreviewWindow(
      firstSourceLine = 0,
      firstPreviewRow = MarkdownDocumentPreview.previewRowForSourceLine(lines, 0).getOrElse(0),
      source = source
    )
    val expectedTopRows =
      MarkdownDocumentPreview
        .previewRowsForSourceRange(lines, cursor.line to (cursor.line + 1))
        .map(_.start - previewWindow.firstPreviewRow)
        .getOrElse(cursor.line - previewWindow.firstSourceLine)

    surface.strokeRoundRectCalls should not be empty
    val border = surface.strokeRoundRectCalls.head
    border.y shouldBe paneRect.y + 1 + expectedTopRows
    border.h shouldBe 2
  }

  private def renderMarkdownLens(
    source: String,
    cursor: CursorPosition,
    topLine: Option[Int] = None
  ): (AppState, MockRenderSurface, CellMetrics) =
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val buffer = Buffer
      .fromString(bufferId, source)
      .copy(
        language = Some(LanguageId.Markdown),
        cursors = List(cursor),
        viewport = Viewport.default.copy(topLine = topLine.getOrElse(cursor.line).max(0), visibleLines = 10)
      )
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      config = AppState.empty.config
        .withSyntaxHighlighting(true)
        .withLineNumbers(false)
        .withGutter(false)
        .withMarkdownViewMode(MarkdownViewMode.InlineLens)
    )
    val surface = new MockRenderSurface(80, 24)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(80, 24),
      codeFont = font,
      textFont = font,
      cellMetrics = metrics,
      cursorColor = None
    )

    (state, surface, metrics)

  private def rows(surface: MockRenderSurface): List[String] =
    (0 until surface.height).map(surface.getRow).map(_.trim).filter(_.nonEmpty).toList
