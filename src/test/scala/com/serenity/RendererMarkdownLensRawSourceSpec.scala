package com.serenity

import java.awt.Font

import com.serenity.config.MarkdownViewMode
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.RendererEntryPoints
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererMarkdownLensRawSourceSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Renderer markdown lens" should "render the active markdown block as raw source" in {
    val bufferId   = BufferId(1)
    val paneId     = PaneId(1)
    val baseBuffer = Buffer.fromString(bufferId, "# Lens\n\n# Raw\ncontinued")
    val buffer = baseBuffer.copy(
      document = baseBuffer.document.copy(language = Some(LanguageId.Markdown)),
      editing = baseBuffer.editing.copy(cursors = List(CursorPosition(2, 0))),
      viewport = Viewport.default.copy(visibleLines = 10)
    )
    val state = AppState.empty.copy(persisted =
      AppState.empty.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = AppState.empty.persisted.config
          .withSyntaxHighlighting(true)
          .withLineNumbers(false)
          .withGutter(false)
          .withMarkdownViewMode(MarkdownViewMode.InlineLens)
      )
    )
    val surface = new MockRenderSurface(80, 24)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)

    RendererEntryPoints.render(
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

  it should "keep inactive blockquote children rendered when editing one child" in {
    val source =
      """# Before
        |
        |> First quoted paragraph
        |>
        |> Second quoted paragraph
        |
        |After""".stripMargin
    val (_, surface, _) = renderMarkdownLens(source, CursorPosition(2, 2), topLine = Some(0))

    val renderedRows = rows(surface)
    renderedRows.exists(_.contains("> First quoted paragraph")) shouldBe true
    renderedRows.exists(_.contains("> Second quoted paragraph")) shouldBe false
  }

  it should "render markdown preview as the inline lens base when the cursor is outside the viewport" in {
    val bufferId   = BufferId(1)
    val paneId     = PaneId(1)
    val baseBuffer = Buffer.fromString(bufferId, "# One\n# Two")
    val buffer = baseBuffer.copy(
      document = baseBuffer.document.copy(language = Some(LanguageId.Markdown)),
      editing = baseBuffer.editing.copy(cursors = List(CursorPosition(10, 0))),
      viewport = Viewport.default.copy(visibleLines = 10)
    )
    val state = AppState.empty.copy(persisted =
      AppState.empty.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = AppState.empty.persisted.config
          .withSyntaxHighlighting(true)
          .withLineNumbers(false)
          .withGutter(false)
          .withMarkdownViewMode(MarkdownViewMode.InlineLens)
      )
    )
    val surface = new MockRenderSurface(80, 24)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)

    RendererEntryPoints.render(
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
        (0 until image.getWidth).exists(column => image.getRGB(column, row) != state.persisted.theme.background.getRGB)
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
      (0 until image.getWidth).exists(column => image.getRGB(column, row) != state.persisted.theme.background.getRGB)
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
    val bufferId   = BufferId(1)
    val paneId     = PaneId(1)
    val baseBuffer = Buffer.fromString(bufferId, "# First\nfirst body\n\n# Second\nsecond body\n\n# Third")
    val buffer = baseBuffer.copy(
      document = baseBuffer.document.copy(language = Some(LanguageId.Markdown)),
      editing = baseBuffer.editing.copy(cursors = List(CursorPosition(0, 0), CursorPosition(3, 0))),
      viewport = Viewport.default.copy(visibleLines = 10)
    )
    val state = AppState.empty.copy(persisted =
      AppState.empty.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = AppState.empty.persisted.config
          .withSyntaxHighlighting(true)
          .withLineNumbers(false)
          .withGutter(false)
          .withMarkdownViewMode(MarkdownViewMode.InlineLens)
      )
    )
    val surface = new MockRenderSurface(80, 24)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)

    RendererEntryPoints.render(
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
    val bufferId   = BufferId(1)
    val paneId     = PaneId(1)
    val baseBuffer = Buffer.fromString(bufferId, "# Title\nParagraph immediately after the heading")
    val buffer = baseBuffer.copy(
      document = baseBuffer.document.copy(language = Some(LanguageId.Markdown)),
      editing = baseBuffer.editing.copy(cursors = List(CursorPosition(1, 0))),
      viewport = Viewport.default.copy(visibleLines = 10)
    )
    val state = AppState.empty.copy(persisted =
      AppState.empty.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = AppState.empty.persisted.config
          .withSyntaxHighlighting(true)
          .withLineNumbers(false)
          .withGutter(false)
          .withMarkdownViewMode(MarkdownViewMode.InlineLens)
      )
    )
    val surface = new MockRenderSurface(80, 24)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)

    RendererEntryPoints.render(
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
    val bufferId   = BufferId(1)
    val paneId     = PaneId(1)
    val baseBuffer = Buffer.fromString(bufferId, "# Preview\n\nRaw paragraph\ncontinued")
    val buffer = baseBuffer.copy(
      document = baseBuffer.document.copy(language = Some(LanguageId.Markdown)),
      editing = baseBuffer.editing.copy(cursors = List(CursorPosition(2, 0))),
      viewport = Viewport.default.copy(visibleLines = 10)
    )
    val state = AppState.empty.copy(persisted =
      AppState.empty.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = AppState.empty.persisted.config
          .withSyntaxHighlighting(true)
          .withLineNumbers(false)
          .withGutter(false)
          .withMarkdownViewMode(MarkdownViewMode.InlineLens)
      )
    )
    val surface = new MockRenderSurface(80, 24)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)

    RendererEntryPoints.render(
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

  it should "reveal only a thematic break when the caret is on it" in {
    val source =
      """- Previous item
        |---
        |After paragraph""".stripMargin
    val (_, surface, _) = renderMarkdownLens(source, CursorPosition(1, 0), topLine = Some(0))

    rawSourceRow(surface, "---") should be >= 0
    rows(surface).exists(_.contains("After paragraph")) shouldBe false
  }

  private def renderMarkdownLens(
    source: String,
    cursor: CursorPosition,
    topLine: Option[Int] = None,
    selection: Option[Selection] = None,
    viewportHeight: Int = 24
  ): (AppState, MockRenderSurface, CellMetrics) =
    val bufferId   = BufferId(1)
    val paneId     = PaneId(1)
    val baseBuffer = Buffer.fromString(bufferId, source)
    val buffer = baseBuffer.copy(
      document = baseBuffer.document.copy(language = Some(LanguageId.Markdown)),
      editing = baseBuffer.editing.copy(cursors = List(cursor), selection = selection),
      viewport = Viewport.default.copy(topLine = topLine.getOrElse(cursor.line).max(0), visibleLines = 10)
    )
    val state = AppState.empty.copy(persisted =
      AppState.empty.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = AppState.empty.persisted.config
          .withSyntaxHighlighting(true)
          .withLineNumbers(false)
          .withGutter(false)
          .withMarkdownViewMode(MarkdownViewMode.InlineLens)
      )
    )
    val surface = new MockRenderSurface(80, viewportHeight)
    val font    = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)

    RendererEntryPoints.render(
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

  private def rows(surface: MockRenderSurface): List[String] =
    (0 until surface.height).map(surface.getRow).map(_.trim).filter(_.nonEmpty).toList

  private def rawSourceRow(surface: MockRenderSurface, source: String): Int =
    (0 until surface.height)
      .find(row => surface.getRow(row).contains(source))
      .getOrElse(fail(s"Expected raw source row for: $source"))
