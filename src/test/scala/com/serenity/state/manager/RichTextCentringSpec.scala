package com.serenity.state.manager

import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph, RichTextRun, RichTextStyle}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{
  LayoutEngine,
  ViewportSize,
  VisualRowCounts,
  WorkspaceNode,
  WorkspaceNodeId,
  WorkspaceTree,
  WrappedLineCache
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The cursor stays on the centre row of a rich-text buffer: the row counts centring places the viewport with are the
  * rows the painted wrap folds each paragraph into -- per-run fonts and drop-cap insets included (#1917).
  */
class RichTextCentringSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private val sentence =
    "lorem ipsum dolor sit amet consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore et dolore magna"

  private val large  = RichTextStyle.empty.withFontSize(30.0f).withMark(com.serenity.richtext.InlineMark.Bold)
  private val serif  = RichTextStyle.empty.withFontFamily("Serif").withFontSize(22.0f)
  private val italic = RichTextStyle.empty.withMark(com.serenity.richtext.InlineMark.Italic)

  /** Paragraph `index` takes a different shape in turn: plain, large, mixed-run, drop cap, heading. */
  private def paragraph(index: Int): RichTextParagraph =
    index % 5 match
      case 0 => RichTextParagraph.plain(sentence + " " + sentence)
      case 1 => RichTextParagraph(List(RichTextRun(sentence + " " + sentence, large)))
      case 2 =>
        RichTextParagraph(List(RichTextRun(sentence, italic), RichTextRun(" " + sentence, serif)))
      case 3 => RichTextParagraph.plain(sentence + " " + sentence, role = ParagraphRole.dropCap(lines = 3))
      case _ => RichTextParagraph.plain(sentence, role = ParagraphRole.Heading(2))

  private def richDocument(count: Int): RichTextDocument = RichTextDocument(List.tabulate(count)(paragraph))

  private def stateWith(document: RichTextDocument, cursor: CursorPosition, columns: Int): AppState =
    val buffer = Buffer
      .fromString(bufferId, document.plainText)
      .copy(
        richText = RichTextState().withSyncedDocument(Some(document), 0L),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = columns, visibleLines = 24),
        editing = EditingState(List(cursor))
      )
    val base    = AppState.initial
    val surface = base.persisted.config.surfaceConfig.copy(wordWrapEnabled = true, typewriterScrollingEnabled = false)
    base.copy(
      persisted = base.persisted.copy(
        config = base.persisted.config.withSurfaceConfig(surface),
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = base.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId)
      ),
      runtime = base.runtime.copy(viewportSize = Some(ViewportSize(columns + 10, 30)))
    )

  private def buffer(state: AppState): Buffer = state.persisted.buffers(bufferId)

  /** `state` with the viewport sized to its pane and placed by the real placement, as a cursor move leaves it. */
  private def placed(state: AppState): AppState =
    val size   = state.runtime.viewportSize.getOrElse(fail("no viewport size"))
    val layout = LayoutEngine.calculateLayoutWithUI(state, size)
    val rect   = LayoutEngine.calculateEditorPaneLayouts(state, layout)(paneId).contentRect
    val sized  = buffer(state).copy(viewport = LayoutEngine.updateBufferViewportDimensions(buffer(state), rect, true))
    val sizedState = state.copy(persisted = state.persisted.copy(buffers = Map(bufferId -> sized)))
    val viewport   = CursorViewport.adjustForCursor(sized, sizedState, sized.editing.cursorPositions.head)
    sizedState.copy(persisted =
      sizedState.persisted.copy(buffers = Map(bufferId -> sized.copy(viewport = viewport)))
    )

  private def paintedSnapshot(state: AppState) =
    val config = state.persisted.config.editorConfig.fontConfig
    RenderCaches
      .create()
      .authoritativeScene
      .forState(
        state,
        state.runtime.viewportSize.getOrElse(fail("no viewport size")),
        FontLoader.previewCodeFont(config),
        FontLoader.previewTextFont(config)
      )
      .textSnapshot(paneId)
      .getOrElse(fail("no text snapshot"))

  "A rich-text buffer" should "paint the cursor on the centre row wherever the cursor sits" in {
    val document = richDocument(60)
    for
      columns   <- List(40, 60, 80)
      line      <- List(21, 22, 23, 24, 25, 33)
      fraction  <- List(0.0, 0.5, 0.95)
    do
      val text   = document.plainText.split("\n", -1)
      val cursor = CursorPosition(line, (text(line).length * fraction).toInt)
      val state  = placed(stateWith(document, cursor, columns))
      withClue(s"columns=$columns line=$line fraction=$fraction: ") {
        paintedSnapshot(state).navigationGeometry.visualRowIndexFor(cursor) shouldBe Some(
          buffer(state).viewport.visibleLines / 2
        )
      }
  }

  "Row counts for centring" should "follow a style-only change that leaves the text untouched" in {
    val plain  = RichTextDocument(List.fill(8)(RichTextParagraph.plain(sentence)))
    val styled = RichTextDocument(List.fill(8)(RichTextParagraph(List(RichTextRun(sentence, large)))))
    val base   = buffer(stateWith(plain, CursorPosition(0, 0), 60))
    val cache  = WrappedLineCache.bounded()
    val font   = FontLoader.previewCodeFont(AppState.initial.persisted.config.editorConfig.fontConfig)
    def rows(document: RichTextDocument): Int =
      val b = base.copy(richText = base.richText.withSyncedDocument(Some(document), 0L))
      VisualRowCounts.forBuffer(b, 400, font, None, false, cache).rowsBetween(0, 8)
    val plainRows  = rows(plain)
    val styledRows = rows(styled)
    withClue(s"plain=$plainRows styled=$styledRows: ")(styledRows should be > plainRows)
  }
