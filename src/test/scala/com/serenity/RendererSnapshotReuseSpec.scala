package com.serenity

import java.awt.Font
import java.util.concurrent.atomic.AtomicInteger

import com.serenity.config.{AppConfig, MarkdownViewMode}
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.{RichTextDocument, RichTextParagraph}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Verifies that rendering a buffer with a cursor produces consistent pixel coordinates — text placement and cursor
  * placement derive from the same snapshot, so the cursor cannot drift.
  */
class RendererSnapshotReuseSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  final case class NonCollectingRope(delegate: Rope) extends Rope:
    override def weight: Int =
      delegate.weight

    override def height: Int =
      delegate.height

    override def newlineCount: Int =
      delegate.newlineCount

    override def lastLineLength: Int =
      delegate.lastLineLength

    override def endsWithNewline: Boolean =
      delegate.endsWithNewline

    override def isWeightBalanced: Boolean =
      delegate.isWeightBalanced

    override def isHeightBalanced: Boolean =
      delegate.isHeightBalanced

    override def rebalance: Rope =
      this

    override def index(i: Int): Option[Char] =
      delegate.index(i)

    override def splitAt(index: Int): Option[(Rope, Rope)] =
      delegate.splitAt(index)

    override def lineCount: Int =
      delegate.lineCount

    override def getLine(lineIndex: Int): Option[String] =
      delegate.getLine(lineIndex)

    override def lineColumnToOffset(line: Int, column: Int): Int =
      delegate.lineColumnToOffset(line, column)

    override def collect(): String =
      throw AssertionError("plain rendering should not materialise the whole buffer")

  final case class CountingAccessRope(
      delegate: Rope,
      lineReads: AtomicInteger = AtomicInteger(0),
      collects: AtomicInteger = AtomicInteger(0)
  ) extends Rope:
    override def weight: Int =
      delegate.weight

    override def height: Int =
      delegate.height

    override def newlineCount: Int =
      delegate.newlineCount

    override def lastLineLength: Int =
      delegate.lastLineLength

    override def endsWithNewline: Boolean =
      delegate.endsWithNewline

    override def isWeightBalanced: Boolean =
      delegate.isWeightBalanced

    override def isHeightBalanced: Boolean =
      delegate.isHeightBalanced

    override def rebalance: Rope =
      this

    override def index(i: Int): Option[Char] =
      delegate.index(i)

    override def splitAt(index: Int): Option[(Rope, Rope)] =
      delegate.splitAt(index)

    override def lineCount: Int =
      delegate.lineCount

    override def getLine(lineIndex: Int): Option[String] =
      lineReads.incrementAndGet()
      delegate.getLine(lineIndex)

    override def linesIteratorFrom(lineIndex: Int): Iterator[(Int, String)] =
      delegate.linesIteratorFrom(lineIndex).map { line =>
        lineReads.incrementAndGet()
        line
      }

    override def lineColumnToOffset(line: Int, column: Int): Int =
      delegate.lineColumnToOffset(line, column)

    override def collect(): String =
      collects.incrementAndGet()
      delegate.collect()

  private val monoFont     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val cellMetrics  = CellMetrics.fromFont(monoFont)
  private val viewportSize = ViewportSize(80, 24)

  private def buildState(content: String, cursorCol: Int): AppState =
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, content).copy(cursors = List(CursorPosition(0, cursorCol)))
    val pane     = EditorPane.withBuffer(paneId, bufferId)
    AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = AppConfig.default.withLineNumbers(false).withGutter(false)
    )

  "Renderer" should "use consistent pixel coordinates for text and cursor on a monospaced buffer" in {
    val state   = buildState("hello", cursorCol = 2)
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)
    Renderer.render(state, cursorVisible = true, surface, viewportSize, monoFont, monoFont, cellMetrics, None)

    val cursorRects = surface.fillPixelRectCalls
    cursorRects should not be empty

    val layout       = LayoutEngine.calculateLayout(state, viewportSize)
    val paneLayouts  = LayoutEngine.calculatePaneLayouts(state, layout)
    val paneRect     = paneLayouts(PaneId(0))
    val panelWidthPx = paneRect.width * cellMetrics.charWidth
    val snapshot = TextLayoutSnapshot.fromBuffer(
      state.buffers(BufferId(1)),
      panelWidthPx,
      monoFont,
      surface.fontRenderContext.getOrElse(fail("missing frc"))
    )
    val expectedXPx = cellMetrics.toPixelX(paneRect.x) + math.round(
      snapshot.xPxForCursor(CursorPosition(0, 2)).getOrElse(fail("missing cursor x"))
    )
    cursorRects.last.xPx shouldBe expectedXPx
  }

  it should "draw the content text via drawRunPx when measured code-font layout is required" in {
    val state   = buildState("hello", 0)
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)
    Renderer.render(state, cursorVisible = true, surface, viewportSize, monoFont, monoFont, cellMetrics, None)
    surface.drawRunPxCalls.exists(_.s.contains("hello")) shouldBe true
  }

  it should "project only visible lines from a high-count annotation index" in {
    val indexed = (0 until 100000).iterator.map(line => line -> List(line)).toMap
    val visible = Set(50000, 50001, 50002)

    val projected = Renderer.visibleAnnotationLines(visible, indexed)

    projected.keySet shouldBe visible
    projected.values.flatten.toSet shouldBe visible
  }

  it should "construct a compact scene annotation index for a high-count buffer" in {
    val bufferId = BufferId(1)
    val comment  = DocumentComment(CursorPosition(0, 0), CursorPosition(100000, 0), "wide")
    val buffer   = Buffer.fromString(bufferId, "content").copy(documentComments = List.fill(10000)(comment))
    val state    = buildState("content", 0).copy(buffers = Map(bufferId -> buffer))

    val index = state.annotationIndexByBuffer(bufferId)()

    index.comments should have size 10000
    index.commentsByLine(Set(100000)).values.flatten should contain only comment
  }

  it should "query a visible comment without scanning unrelated indexed ranges" in {
    val bufferId = BufferId(1)
    val unrelated =
      (0 until 50000).map(line => DocumentComment(CursorPosition(line, 0), CursorPosition(line, 0), "offscreen"))
    val visible = DocumentComment(CursorPosition(100000, 0), CursorPosition(100000, 0), "visible")
    val buffer  = Buffer.fromString(bufferId, "content").copy(documentComments = (unrelated :+ visible).toList)
    val state   = buildState("content", 0).copy(buffers = Map(bufferId -> buffer))

    val result = state.annotationIndexByBuffer(bufferId)().commentsByLine(Set(100000))

    result.values.flatten.toList shouldBe List(visible)
  }

  it should "render a plain large buffer without materialising the whole rope" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val content  = NonCollectingRope(Rope("{" + (1 to 5000).map(i => s""""k$i":$i""").mkString(",") + "}"))
    val buffer = Buffer(bufferId, content)
      .copy(
        language = Some(com.serenity.lsp.config.LanguageId.JsonLang),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 80, visibleLines = 20)
      )
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = AppConfig.default.withLineNumbers(false).withGutter(false).withWordWrap(false)
    )
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)

    noException should be thrownBy Renderer.render(
      state,
      cursorVisible = true,
      surface,
      viewportSize,
      monoFont,
      monoFont,
      cellMetrics,
      None
    )
  }

  it should "reuse the active pane snapshot when rendering line numbers" in {
    val paneId    = PaneId(0)
    val bufferId  = BufferId(1)
    val lineReads = AtomicInteger(0)
    val content   = CountingAccessRope(Rope((1 to 20).map(i => s"line-$i").mkString("\n")), lineReads = lineReads)
    val buffer = Buffer(bufferId, content).copy(
      viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 80, visibleLines = 5)
    )
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = AppConfig.default.withLineNumbers(true).withGutter(false).withWordWrap(false)
    )
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)
    val layout  = LayoutEngine.calculateLayout(state, viewportSize)
    val paneContentHeight =
      LayoutEngine.calculateEditorPaneLayouts(state, layout)(paneId).contentRect.height

    Renderer.render(state, cursorVisible = true, surface, viewportSize, monoFont, monoFont, cellMetrics, None)

    lineReads.get() shouldBe math.min(buffer.content.lineCount, paneContentHeight) + 1
  }

  it should "render rich text visible lines without materialising the whole rope" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val collects = AtomicInteger(0)
    val document = RichTextDocument((1 to 8).map(i => RichTextParagraph.plain(s"paragraph-$i")).toList)
    val content  = CountingAccessRope(Rope(document.plainText), collects = collects)
    val buffer = Buffer(bufferId, content).copy(
      richTextDocument = Some(document),
      viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 80, visibleLines = 6)
    )
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = AppConfig.default.withLineNumbers(false).withGutter(false).withWordWrap(false)
    )
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)

    Renderer.render(state, cursorVisible = true, surface, viewportSize, monoFont, monoFont, cellMetrics, None)

    collects.get() shouldBe 0
  }

  it should "reject structurally stale rich text metadata without materialising the whole rope" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val collects = AtomicInteger(0)
    val document = RichTextDocument(List(RichTextParagraph.plain("paragraph-1")))
    val content  = CountingAccessRope(Rope("paragraph-1\nparagraph-2"), collects = collects)
    val buffer = Buffer(bufferId, content).copy(
      richTextDocument = Some(document),
      viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 80, visibleLines = 6)
    )
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = AppConfig.default.withLineNumbers(false).withGutter(false).withWordWrap(false)
    )
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)

    Renderer.render(state, cursorVisible = true, surface, viewportSize, monoFont, monoFont, cellMetrics, None)

    collects.get() shouldBe 0
    surface.drawRunPxCalls.map(_.s) should contain("paragraph-1")
  }

  it should "share markdown lens source lines between content and cursor rendering" in {
    val paneId    = PaneId(0)
    val bufferId  = BufferId(1)
    val lineReads = AtomicInteger(0)
    val markdown  = (1 to 2_000).map(i => s"# Heading $i").mkString("\n")
    val content   = CountingAccessRope(Rope(markdown), lineReads = lineReads)
    val buffer = Buffer(bufferId, content).copy(
      language = Some(LanguageId.Markdown),
      cursors = List(CursorPosition(0, 0)),
      viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 80, visibleLines = 6)
    )
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = AppConfig.default
        .withLineNumbers(false)
        .withGutter(false)
        .withWordWrap(false)
        .withMarkdownViewMode(MarkdownViewMode.InlineLens)
    )
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)
    val layout  = LayoutEngine.calculateLayout(state, viewportSize)
    val paneContentHeight =
      LayoutEngine.calculateEditorPaneLayouts(state, layout)(paneId).contentRect.height

    Renderer.render(state, cursorVisible = true, surface, viewportSize, monoFont, monoFont, cellMetrics, None)

    lineReads.get() should be < 200
  }

  it should "bound renderer reads for a long fenced block in a large document" in {
    val paneId    = PaneId(0)
    val bufferId  = BufferId(1)
    val lineReads = AtomicInteger(0)
    val markdown =
      (Vector.fill(5_000)("unrelated prose") ++
        Vector("```scala") ++
        (1 to 1_000).map(index => s"val result = $index") ++
        Vector("```") ++
        Vector.fill(5_000)("trailing prose")).mkString("\n")
    val content = CountingAccessRope(Rope(markdown), lineReads = lineReads)
    val buffer = Buffer(bufferId, content).copy(
      language = Some(LanguageId.Markdown),
      cursors = List(CursorPosition(5_500, 0)),
      viewport = Viewport(topLine = 5_500, leftColumn = 0, visibleColumns = 80, visibleLines = 6)
    )
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = AppConfig.default
        .withLineNumbers(false)
        .withGutter(false)
        .withWordWrap(false)
        .withMarkdownViewMode(MarkdownViewMode.InlineLens)
    )
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)

    noException should be thrownBy Renderer.render(
      state,
      cursorVisible = true,
      surface,
      viewportSize,
      monoFont,
      monoFont,
      cellMetrics,
      None
    )

    lineReads.get() should be < 20_000
  }

  it should "bound renderer reads for a long paragraph in a large document" in {
    val paneId    = PaneId(0)
    val bufferId  = BufferId(1)
    val lineReads = AtomicInteger(0)
    val markdown =
      (Vector.fill(1_000)("paragraph content") ++
        Vector("") ++
        Vector.fill(1_000)("trailing prose")).mkString("\n")
    val content = CountingAccessRope(Rope(markdown), lineReads = lineReads)
    val buffer = Buffer(bufferId, content).copy(
      language = Some(LanguageId.Markdown),
      cursors = List(CursorPosition(500, 0)),
      viewport = Viewport(topLine = 500, leftColumn = 0, visibleColumns = 80, visibleLines = 6)
    )
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = AppConfig.default
        .withLineNumbers(false)
        .withGutter(false)
        .withWordWrap(false)
        .withMarkdownViewMode(MarkdownViewMode.InlineLens)
    )
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)

    noException should be thrownBy Renderer.render(
      state,
      cursorVisible = true,
      surface,
      viewportSize,
      monoFont,
      monoFont,
      cellMetrics,
      None
    )

    // Full-block Markdown lens resolution may inspect the enclosing semantic block.
    lineReads.get() should be < 20_000
  }

  it should "bound bare fence classification reads after a long prose prefix" in {
    val paneId    = PaneId(0)
    val bufferId  = BufferId(1)
    val lineReads = AtomicInteger(0)
    val markdown =
      (Vector.fill(1_000)("unrelated prose") ++
        Vector("```scala") ++
        Vector.fill(499)("fenced content") ++
        Vector("```") ++
        Vector.fill(1_000)("trailing prose")).mkString("\n")
    val content = CountingAccessRope(Rope(markdown), lineReads = lineReads)
    val buffer = Buffer(bufferId, content).copy(
      language = Some(LanguageId.Markdown),
      cursors = List(CursorPosition(1_500, 0)),
      viewport = Viewport(topLine = 1_500, leftColumn = 0, visibleColumns = 80, visibleLines = 6)
    )
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = AppConfig.default
        .withLineNumbers(false)
        .withGutter(false)
        .withWordWrap(false)
        .withMarkdownViewMode(MarkdownViewMode.InlineLens)
    )
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)

    noException should be thrownBy Renderer.render(
      state,
      cursorVisible = true,
      surface,
      viewportSize,
      monoFont,
      monoFont,
      cellMetrics,
      None
    )

    lineReads.get() should be < 2_000
  }
