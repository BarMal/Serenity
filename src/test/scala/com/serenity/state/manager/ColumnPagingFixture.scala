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
  TextLayoutSnapshot,
  ViewportSize,
  WorkspaceNode,
  WorkspaceNodeId,
  WorkspaceTree
}
import com.serenity.ui.theme.RichTextStyling

/** A multi-column pane over a rich document whose rows are not all one line tall: headings at three sizes with space
  * around them, drop caps both taller and shorter than the rows beside them. `scale` multiplies the fonts, as a 2x
  * display would.
  */
final class ColumnPagingFixture(val scale: Int, val columnCount: Int, val gridRows: Int = 14):

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
      Buffer.fromString(bufferId, document.plainText).copy(richText = RichTextState(richTextDocument = Some(document)))
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
      headings = state.persisted.config.headingSettings
    )

  /** Every row of the document in order, laid out as one endless column. */
  val truth: Vector[RowId] =
    columnsAt(Viewport(0, 0, textColumns, 1), 1, rows = 100000).flatMap(_.visualLines).map(rowId)

  def placed(from: Buffer, id: RowId): Buffer =
    val cursor = CursorPosition(id._1, id._2)
    from.copy(
      editing = EditingState(List(cursor)),
      viewport = CursorViewport.adjustForCursorColumnMode(from, state, cursor)
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
