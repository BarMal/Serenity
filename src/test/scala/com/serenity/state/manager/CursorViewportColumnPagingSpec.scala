package com.serenity.state.manager

import com.serenity.config.AppConfig
import com.serenity.frontend.FrontendCapabilities
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A column-mode page is `columnCount` columns of the rows the pane's own font actually draws per column
  * (`RendererPaneSetup`, `UiSceneBuilder`: pane height over the buffer font's line height). `viewport.visibleLines` is
  * the code grid's row count, so a document-font pane whose font is taller than the code font draws fewer rows per
  * column; the page the viewport anchors to has to be that many rows, or the caret walks off the bottom of the last
  * drawn column without the page turning.
  */
class CursorViewportColumnPagingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId       = PaneId(0)
  private val bufferId     = BufferId(1)
  private val viewportSize = ViewportSize(400, 40)
  private val fontConfig   = FontLoader.FontConfig(textFontSize = 40f, fontSize = 10f)
  private val gridRows     = 10

  private val buffer =
    Buffer
      .fromString(bufferId, (0 until 80).map(index => s"line $index").mkString("\n"))
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 20, visibleLines = gridRows))

  private def guiState(columnCount: Int): AppState =
    val config = AppConfig.default
      .withFontConfig(fontConfig)
      .withWordWrap(true)
      .withColumnMode(true)
      .withColumnCount(Some(columnCount))
    val base = AppState.initial
    base.copy(
      persisted = base.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = base.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = config
      ),
      runtime = base.runtime.copy(capabilities = FrontendCapabilities.gui, viewportSize = Some(viewportSize))
    )

  /** The rows one column draws, computed the way the renderer sizes the pane it paints. */
  private val drawnRows =
    val codeHeight = CellMetrics.fromFont(FontLoader.previewCodeFont(fontConfig)).lineHeight
    val textHeight = CellMetrics.fromFont(FontLoader.previewTextFont(fontConfig)).lineHeight
    math.max(1, gridRows * codeHeight / textHeight)

  private def placed(state: AppState, from: Buffer, line: Int): Buffer =
    val cursor = CursorPosition(line, 0)
    from.copy(
      editing = EditingState(List(cursor)),
      viewport = CursorViewport.adjustForCursorColumnMode(from, state, cursor)
    )

  "the test setup" should "draw fewer rows per column than the code grid counts" in {
    drawnRows should be > 1
    drawnRows should be < gridRows
    buffer.typographyRole shouldBe TypographyRole.Prose
  }

  "adjustForCursorColumnMode in GUI mode with one column" should
    "turn the page on the first row past the rows the column draws" in {
      val state = guiState(columnCount = 1)

      placed(state, buffer, drawnRows - 1).viewport.topLine shouldBe 0
      placed(state, buffer, drawnRows).viewport.topLine shouldBe drawnRows
    }

  "adjustForCursorColumnMode in GUI mode with two columns" should
    "turn the page on the first row past the last drawn column" in {
      val state = guiState(columnCount = 2)

      placed(state, buffer, 2 * drawnRows - 1).viewport.topLine shouldBe 0
      placed(state, buffer, 2 * drawnRows).viewport.topLine shouldBe 2 * drawnRows
    }

  it should "keep the caret inside the drawn page walking down and back up one line at a time" in {
    val state    = guiState(columnCount = 2)
    val pageRows = 2 * drawnRows
    val lines    = (0 until 60).toList ++ (59 to 0 by -1).toList

    val outside = lines
      .scanLeft((buffer, 0))((walk, line) => (placed(state, walk._1, line), line))
      .drop(1)
      .collect {
        case (walked, line) if line - walked.viewport.topLine < 0 || line - walked.viewport.topLine >= pageRows =>
          s"caret on line $line with the page at line ${walked.viewport.topLine}"
      }

    outside.take(5) shouldBe Nil
  }
