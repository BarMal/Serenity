package com.serenity.state.manager

import com.serenity.config.{AppConfig, HeadingsConfig}
import com.serenity.frontend.FrontendCapabilities
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{
  CellMetrics,
  LayoutEngine,
  LayoutMemos,
  TextLayoutSnapshot,
  ViewportSize,
  WorkspaceNode,
  WorkspaceNodeId,
  WorkspaceTree,
  WrappedLineCache
}
import com.serenity.ui.renderer.ShapedTextCache
import com.serenity.ui.theme.RichTextStyling

/** A multi-column pane over a rich document whose rows are not all one line tall: headings at three sizes with space
  * around them, drop caps both taller and shorter than the rows beside them. `scale` multiplies the fonts, as a 2x
  * display would.
  */
final class ColumnPagingFixture(
    val scale: Int,
    val columnCount: Int,
    val gridRows: Int = ColumnPagingFixture.DefaultGridRows,
    val source: RichTextDocument = ColumnPagingFixture.document
):

  given Balance = Balance.default

  import ColumnPagingFixture.*

  val fontConfig: FontLoader.FontConfig = FontLoader.FontConfig(textFontSize = 14f * scale, fontSize = 12f * scale)

  val state: AppState =
    val config = AppConfig.default
      .withFontConfig(fontConfig)
      .withWordWrap(true)
      .withColumnMode(true)
      .withColumnCount(Some(columnCount))
      .withHeadingsConfig(spacing)
    val base = AppState.initial
    val fresh =
      Buffer.fromString(bufferId, source.plainText).copy(richText = RichTextState(richTextDocument = Some(source)))
    val unsynced = base.copy(
      persisted = base.persisted.copy(
        buffers = Map(fresh.id -> fresh),
        bufferOrder = List(fresh.id),
        layout = base.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, fresh.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = config
      ),
      runtime = base.runtime.copy(capabilities = FrontendCapabilities.gui, viewportSize = Some(viewportSize))
    )
    val synced = LayoutEngine.syncViewportDimensions(unsynced, viewportSize)
    val sized  = synced.persisted.buffers(bufferId)
    synced.copy(persisted =
      synced.persisted.copy(buffers =
        synced.persisted.buffers + (bufferId -> sized.copy(viewport = sized.viewport.copy(visibleLines = gridRows)))
      )
    )

  val buffer: Buffer = state.persisted.buffers(bufferId)

  val font: java.awt.Font = FontLoader.previewFontForRole(fontConfig, buffer.typographyRole)

  /** The cells one column wraps its text in, as `CursorViewport` and the painter both derive them. */
  val textColumns: Int = math.max(1, buffer.viewport.visibleColumns - LayoutEngine.perColumnGutterWidth(state))

  /** Every layout here measures its wraps afresh, over one shaper that stays warm, as when the shaped-text cache was
    * process-wide.
    */
  private val shaping = ShapedTextCache()

  private val memos = LayoutMemos()

  private def uncached: WrappedLineCache = WrappedLineCache.measuring(shaping, memos)

  val widthPx: Int = TextLayoutSnapshot.gridWrapWidthPx(textColumns, fontConfig)

  /** The rows one column draws, sized the way the renderer sizes the pane it paints. */
  val rowsPerColumn: Int =
    val codeHeight = CellMetrics.fromFont(FontLoader.previewCodeFont(fontConfig)).lineHeight
    val textHeight = CellMetrics.fromFont(FontLoader.previewTextFont(fontConfig)).lineHeight
    math.max(1, gridRows * codeHeight / textHeight)

  /** The pixels the pane gives one column: its rows of the cell grid the pane is divided into. */
  val columnHeightPx: Int = rowsPerColumn * CellMetrics.fromFont(FontLoader.previewTextFont(fontConfig)).lineHeight

  /** The columns of the page `viewport` anchors, laid out as the painter lays them out. */
  def columnsAt(viewport: Viewport, count: Int = columnCount, rows: Int = rowsPerColumn): Vector[TextLayoutSnapshot] =
    TextLayoutSnapshot.fromBufferColumns(
      buffer.copy(viewport = viewport.copy(visibleLines = rows)),
      widthPx,
      font,
      proseScale = RichTextStyling.proseZoom(font.getSize2D),
      columnCount = count,
      headings = state.persisted.config.headingSettings,
      wrapCache = uncached
    )

  /** Every row of the document in order, laid out as one endless column. */
  val truth: Vector[RowId] =
    columnsAt(Viewport(0, 0, textColumns, 1), 1, rows = 100000).flatMap(_.visualLines).map(rowId)

  def placed(from: Buffer, id: RowId): Buffer =
    val cursor = CursorPosition(id._1, id._2)
    from.copy(
      editing = EditingState(List(cursor)),
      viewport = CursorViewport.adjustForCursorColumnMode(from, state, cursor, uncached)
    )

  /** The pages the cursor can anchor, in document order, each placed from a cold viewport. */
  val pages: Vector[Page] =
    val anchors = truth.map(id => placed(buffer, id).viewport)
    val distinct = anchors.foldLeft(Vector.empty[Viewport]) { (seen, viewport) =>
      if seen.lastOption.exists(sameTop(_, viewport)) then seen else seen :+ viewport
    }
    distinct.map(viewport => Page(viewport, columnsAt(viewport)))

  /** The index of the column, counted from the first column of the document, that shows `id`. */
  def columnOrdinal(id: RowId): Option[Int] =
    val shown = for
      (page, pageIndex)     <- pages.zipWithIndex
      (column, columnIndex) <- page.columns.zipWithIndex
      if column.visualLines.exists(line => rowId(line) == id)
    yield pageIndex * columnCount + columnIndex
    shown.headOption

object ColumnPagingFixture:

  val DefaultGridRows: Int = 14

  type RowId = (Int, Int)

  final case class Page(viewport: Viewport, columns: Vector[TextLayoutSnapshot])

  val paneId: PaneId     = PaneId(0)
  val bufferId: BufferId = BufferId(1)
  val viewportSize       = ViewportSize(70, 60)

  val spacing: HeadingsConfig = HeadingsConfig()
    .withLevel(1)(_.copy(spaceBefore = Some(1.5f), spaceAfter = Some(1.0f)))
    .withLevel(2)(_.copy(spaceBefore = Some(1.0f), spaceAfter = Some(0.5f)))
    .withLevel(3)(_.copy(spaceBefore = Some(0.5f), spaceAfter = Some(0.5f)))

  private def paragraph(text: String, role: ParagraphRole = ParagraphRole.Body): RichTextParagraph =
    RichTextParagraph.plain(text, role = role)

  val document: RichTextDocument = RichTextDocument(
    (1 to 12).toList.flatMap { chapter =>
      List(
        paragraph(s"Chapter $chapter", ParagraphRole.Heading(1)),
        paragraph(
          s"The chapter $chapter opens here. " + "words that fill the column up " * 8,
          ParagraphRole.dropCap(3)
        ),
        paragraph(s"Section $chapter.1 with a long enough title to wrap", ParagraphRole.Heading(2)),
        paragraph(s"Section text $chapter. " + "and some more words here " * 9),
        paragraph("Brief.", ParagraphRole.dropCap(4)),
        paragraph("short body"),
        paragraph(s"Sub $chapter.1.1", ParagraphRole.Heading(3)),
        paragraph(s"Closing words of $chapter. " + "the end of the part " * 5)
      )
    } ++ List(
      // Every chapter's headings are kept with the paragraph after them, which can leave all of them at a column's top;
      // this one follows the short last column of the document, so a heading with room above it is always drawn.
      paragraph("Coda", ParagraphRole.Heading(2)),
      paragraph("The end.")
    )
  )

  /** A paragraph that wraps to several rows, so the row that follows a heading is only its first. */
  val bodyFollower: RichTextParagraph = paragraph("Following text. " + "words that fill the column up " * 8)

  val dropCapFollower: RichTextParagraph =
    paragraph("The opening words. " + "words that fill the column up " * 8, ParagraphRole.dropCap(3))

  def heading(level: Int, text: String): RichTextParagraph = paragraph(text, ParagraphRole.Heading(level))

  /** `fillers` one-row paragraphs, then `lead`, then `follower`, then enough body to run on into further columns. */
  def leadAfterFillers(
    fillers: Int,
    lead: List[RichTextParagraph],
    follower: RichTextParagraph
  ): RichTextDocument =
    RichTextDocument(
      (0 until fillers).toList.map(n => paragraph(s"f$n")) ++ lead ++ List(follower) ++
        (0 until 40).toList.map(n => paragraph(s"tail $n"))
    )

  /** Every distance a heading run can sit from the bottom of a column: `fillers` runs from 0 to 20 rows before each
    * pair of headings, whichever the height of a row.
    */
  val sweptHeadings: RichTextDocument =
    RichTextDocument(
      (0 to 20).toList.flatMap { fillers =>
        (0 until fillers).toList.map(n => paragraph(s"filler $fillers.$n")) ++ List(
          heading(1, s"Part $fillers"),
          heading(2, s"Chapter $fillers"),
          if fillers % 2 == 0 then bodyFollower else dropCapFollower,
          paragraph("Brief.", ParagraphRole.dropCap(4))
        )
      }
    )

  def sameTop(a: Viewport, b: Viewport): Boolean = a.topLine == b.topLine && a.topVisualLine == b.topVisualLine

  def rowId(line: TextVisualLine): RowId = (line.bufferLine, line.startColumn)

  /** What a column's rows add up to when stacked: each row's text box and the room it reserves, plus its heading space.
    */
  def stackedPx(column: TextLayoutSnapshot): Int =
    column.visualLines
      .zip(column.rowHeightsPx)
      .map((line, height) => line.spaceBeforePx + height + line.spaceAfterPx)
      .sum
