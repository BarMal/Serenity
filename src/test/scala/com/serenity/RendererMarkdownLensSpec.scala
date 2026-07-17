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

  it should "render adjacent heading and paragraph preview rows separately" in {
    val (state, surface, metrics) = renderMarkdownLens(
      "# Heading\nParagraph immediately after the heading",
      CursorPosition(10, 0),
      topLine = Some(0)
    )

    val image = surface.drawImageCalls.head.image
    val contentRows = (0 until image.getHeight).filter(row =>
      (0 until image.getWidth).exists(column => image.getRGB(column, row) != state.theme.background.getRGB)
    )
    val contentBands = contentRows.foldLeft(Vector.empty[Vector[Int]]) { (bands, row) =>
      bands.lastOption match
        case Some(lastBand) if row == lastBand.last + 1 => bands.init :+ (lastBand :+ row)
        case _                                          => bands :+ Vector(row)
    }

    contentBands should have size 2
    contentBands(1).head should be >= metrics.lineHeight
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

  it should "not draw a border around the raw source lens" in {
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

    surface.strokeRoundRectCalls shouldBe empty
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

    val paneRect =
      LayoutEngine.calculatePaneLayouts(state, LayoutEngine.calculateLayout(state, ViewportSize(80, 24)))(paneId)
    panelRows(surface, state, paneRect) should have size 5
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
    rawSourceRow(surface, "| Task | Owner |") shouldBe paneRect.y + 1
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
        2
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
        val (state, surface, _) = renderMarkdownLens(source, cursor)
        val paneRect =
          LayoutEngine.calculatePaneLayouts(state, LayoutEngine.calculateLayout(state, ViewportSize(80, 24)))(
            PaneId(1)
          )

        withClue(s"$name lens: ") {
          rawSourceRow(surface, source.linesIterator.toVector(cursor.line)) shouldBe paneRect.y + 1
          panelRows(surface, state, paneRect) should have size expectedHeightRows
        }
    }
  }

  it should "keep the full document previewed while caret movement reveals only each active source unit" in {
    val source =
      """# Serenity document preview
        |
        |This paragraph should be rendered as readable prose while the cursor is elsewhere.
        |
        |## Navigation and alignment
        |
        |- The active block should reveal its Markdown source.
        |- Rendered blocks should remain aligned around it.
        |- Moving the caret should replace, not displace, the rendered block.
        |
        || Area | Expected behaviour |
        || --- | --- |
        || Heading | Large and aligned |
        || Paragraph | Readable prose |
        || Table | Stable rows and borders |
        |
        |Final paragraph after the table.""".stripMargin
    val cases = List(
      CursorPosition(0, 0)  -> 1,
      CursorPosition(2, 0)  -> 2,
      CursorPosition(6, 0)  -> 1,
      CursorPosition(10, 0) -> 7,
      CursorPosition(15, 0) -> 2
    )

    cases.foreach {
      case (cursor, expectedLensHeight) =>
        val (state, surface, _) = renderMarkdownLens(source, cursor, topLine = Some(0))
        val paneRect =
          LayoutEngine.calculatePaneLayouts(state, LayoutEngine.calculateLayout(state, ViewportSize(80, 24)))(
            PaneId(1)
          )

        withClue(s"cursor at source line ${cursor.line}: ") {
          rawSourceRow(surface, source.linesIterator.toVector(cursor.line).take(40)) should be >= paneRect.y
          panelRows(surface, state, paneRect) should have size expectedLensHeight
        }
    }
  }

  it should "retain rendered context below an active source unit near the preview window boundary" in {
    val sourceLines =
      Vector("# Start") ++
        Vector.tabulate(34)(index => s"Context before $index") ++
        Vector("# Active heading") ++
        Vector.tabulate(9)(index => s"Context after $index") ++
        Vector("Later rendered context")
    val source = sourceLines.mkString("\n")
    val (state, surface, metrics) = renderMarkdownLens(
      source,
      CursorPosition(35, 0),
      topLine = Some(0),
      viewportHeight = 60
    )
    val actual = surface.drawImageCalls.head.image
    val expected = MarkdownDocumentPreview.renderInlineImage(
      sourceLines = sourceLines,
      firstSourceLine = 0,
      maxSourceLines = sourceLines.length,
      title = "Untitled",
      widthPx = actual.getWidth,
      heightPx = actual.getHeight,
      theme = state.theme,
      font = MarkdownDocumentPreview.inlineLensFont(
        Font(Font.MONOSPACED, Font.PLAIN, 12),
        metrics.lineHeight,
        deviceScale = 1.0
      ),
      inlineLineHeightPx = metrics.lineHeight
    )

    MarkdownDocumentPreview.inlinePreviewRows(sourceLines, 0, sourceLines.length).map(_.text) should contain(
      "Later rendered context"
    )
    samePixels(actual, expected) shouldBe true
  }

  it should "keep the preview window bounded when the active cursor is far below the viewport" in {
    val sourceLines = Vector.tabulate(120)(index => s"Document line $index")
    val (state, surface, metrics) = renderMarkdownLens(
      sourceLines.mkString("\n"),
      CursorPosition(119, 0),
      topLine = Some(0),
      viewportHeight = 60
    )
    val actual = surface.drawImageCalls.head.image
    val expected = MarkdownDocumentPreview.renderInlineImage(
      sourceLines = sourceLines,
      firstSourceLine = 0,
      maxSourceLines = 40,
      title = "Untitled",
      widthPx = actual.getWidth,
      heightPx = actual.getHeight,
      theme = state.theme,
      font = MarkdownDocumentPreview.inlineLensFont(
        Font(Font.MONOSPACED, Font.PLAIN, 12),
        metrics.lineHeight,
        deviceScale = 1.0
      ),
      inlineLineHeightPx = metrics.lineHeight
    )

    samePixels(actual, expected) shouldBe true
  }

  it should "keep the preview window bounded for a very tall active block" in {
    val sourceLines = Vector("```text") ++ Vector.tabulate(100)(index => s"Code line $index") ++ Vector("```")
    val (state, surface, metrics) = renderMarkdownLens(
      sourceLines.mkString("\n"),
      CursorPosition(1, 0),
      topLine = Some(0),
      viewportHeight = 60
    )
    val actual = surface.drawImageCalls.head.image
    val expected = MarkdownDocumentPreview.renderInlineImage(
      sourceLines = sourceLines,
      firstSourceLine = 0,
      maxSourceLines = 40,
      title = "Untitled",
      widthPx = actual.getWidth,
      heightPx = actual.getHeight,
      theme = state.theme,
      font = MarkdownDocumentPreview.inlineLensFont(
        Font(Font.MONOSPACED, Font.PLAIN, 12),
        metrics.lineHeight,
        deviceScale = 1.0
      ),
      inlineLineHeightPx = metrics.lineHeight
    )

    rawSourceRow(surface, "Code line 0") should be >= 0
    samePixels(actual, expected) shouldBe true

    val (_, scrolledSurface, _) = renderMarkdownLens(
      sourceLines.mkString("\n"),
      CursorPosition(80, 0),
      topLine = Some(80),
      viewportHeight = 60
    )
    rawSourceRow(scrolledSurface, "Code line 79") should be >= 0
  }

  it should "reveal every markdown source unit touched by a selection" in {
    val source =
      """# First heading
        |
        |First paragraph.
        |
        |# Second heading
        |
        |Second paragraph.""".stripMargin
    val (state, surface, _) = renderMarkdownLens(
      source,
      CursorPosition(0, 0),
      selection = Some(Selection(CursorPosition(0, 0), CursorPosition(4, 0)))
    )
    val paneRect =
      LayoutEngine.calculatePaneLayouts(state, LayoutEngine.calculateLayout(state, ViewportSize(80, 24)))(PaneId(1))

    val firstHeadingRow = rawSourceRow(surface, "# First heading")
    firstHeadingRow should be >= paneRect.y
    rawSourceRow(surface, "First paragraph.") should be >= paneRect.y
    rawSourceRow(surface, "# Second heading") should be >= paneRect.y
    surface.getBg(paneRect.x + 1, firstHeadingRow) shouldBe state.theme.highlighted.background
    panelRows(surface, state, paneRect) should have size 5
  }

  it should "render the scrolled document window when the caret remains above it" in {
    val source =
      (Vector.fill(32)("") ++ Vector("# Reached after scrolling", "", "Visible prose at the viewport.")).mkString("\n")
    val (state, surface, metrics) = renderMarkdownLens(
      source,
      CursorPosition(0, 0),
      topLine = Some(32)
    )
    val sourceLines = source.linesIterator.toVector
    val expectedRows = MarkdownDocumentPreview.inlinePreviewRows(
      sourceLines,
      firstSourceLine = 32,
      maxSourceLines = sourceLines.length - 32
    )
    val actual = surface.drawImageCalls.head.image
    val expected = MarkdownDocumentPreview.renderInlineImage(
      sourceLines = sourceLines,
      firstSourceLine = 32,
      maxSourceLines = sourceLines.length - 32,
      title = "Untitled",
      widthPx = actual.getWidth,
      heightPx = actual.getHeight,
      theme = state.theme,
      font = MarkdownDocumentPreview.inlineLensFont(
        Font(Font.MONOSPACED, Font.PLAIN, 12),
        metrics.lineHeight,
        deviceScale = 1.0
      ),
      inlineLineHeightPx = metrics.lineHeight
    )
    val offscreenCaretWindow = MarkdownDocumentPreview.renderInlineImage(
      sourceLines = sourceLines,
      firstSourceLine = 0,
      maxSourceLines = 32,
      title = "Untitled",
      widthPx = actual.getWidth,
      heightPx = actual.getHeight,
      theme = state.theme,
      font = MarkdownDocumentPreview.inlineLensFont(
        Font(Font.MONOSPACED, Font.PLAIN, 12),
        metrics.lineHeight,
        deviceScale = 1.0
      ),
      inlineLineHeightPx = metrics.lineHeight
    )

    surface.drawImageCalls should have size 1
    expectedRows.map(_.text) should contain allOf ("Reached after scrolling", "Visible prose at the viewport.")
    samePixels(actual, expected) shouldBe true
    samePixels(actual, offscreenCaretWindow) shouldBe false
  }

  it should "keep visible preview context above the active lens" in {
    val (state, surface, _) = renderMarkdownLens(
      "# Intro\n\nOpening paragraph\n\nActive paragraph\ncontinued",
      CursorPosition(4, 0),
      topLine = Some(0)
    )
    val paneRect =
      LayoutEngine.calculatePaneLayouts(state, LayoutEngine.calculateLayout(state, ViewportSize(80, 24)))(
        PaneId(1)
      )

    val renderedRows = rows(surface)
    renderedRows.exists(_.contains("Active paragraph")) shouldBe true
    renderedRows.exists(_.contains("continued")) shouldBe true
    rawSourceRow(surface, "Active paragraph") should be > paneRect.y + 1
    panelRows(surface, state, paneRect) should have size 2
  }

  it should "align the active lens after expanded preview context" in {
    val source =
      "| Task | Owner |\n| ---- | ----- |\n| Ship | Codex |\n\nActive paragraph\ncontinued"
    val cursor              = CursorPosition(4, 0)
    val (state, surface, _) = renderMarkdownLens(source, cursor, topLine = Some(0))
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

    rawSourceRow(surface, "Active paragraph") shouldBe paneRect.y + 1 + expectedTopRows
    panelRows(surface, state, paneRect) should have size 2
  }

  private def renderMarkdownLens(
    source: String,
    cursor: CursorPosition,
    topLine: Option[Int] = None,
    selection: Option[Selection] = None,
    viewportHeight: Int = 24
  ): (AppState, MockRenderSurface, CellMetrics) =
    val bufferId = BufferId(1)
    val paneId   = PaneId(1)
    val buffer = Buffer
      .fromString(bufferId, source)
      .copy(
        language = Some(LanguageId.Markdown),
        cursors = List(cursor),
        selection = selection,
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
    val surface = new MockRenderSurface(80, viewportHeight)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)

    Renderer.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(80, viewportHeight),
      codeFont = font,
      textFont = font,
      cellMetrics = metrics,
      cursorColor = None
    )

    (state, surface, metrics)

  private def samePixels(left: java.awt.image.BufferedImage, right: java.awt.image.BufferedImage): Boolean =
    left.getWidth == right.getWidth &&
      left.getHeight == right.getHeight &&
      (0 until left.getHeight).forall { row =>
        (0 until left.getWidth).forall(column => left.getRGB(column, row) == right.getRGB(column, row))
      }

  private def rows(surface: MockRenderSurface): List[String] =
    (0 until surface.height).map(surface.getRow).map(_.trim).filter(_.nonEmpty).toList

  private def rawSourceRow(surface: MockRenderSurface, source: String): Int =
    (0 until surface.height)
      .find(row => surface.getRow(row).contains(source))
      .getOrElse(fail(s"Expected raw source row for: $source"))

  private def panelRows(surface: MockRenderSurface, state: AppState, paneRect: LayoutRect): Vector[Int] =
    (paneRect.y until paneRect.bottom)
      .filter(row =>
        (paneRect.x until paneRect.right).exists(column => surface.getBg(column, row) == state.theme.panel.background)
      )
      .toVector
