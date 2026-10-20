package com.serenity.state.manager

import com.serenity.config.AppConfig
import com.serenity.frontend.FrontendCapabilities
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Where the scene the renderer and the mouse share places each pane in its document, counted in the visual rows its
  * lines wrap into.
  */
class UiSceneScrollSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewportSize = ViewportSize(100, 30)
  private val paneId       = PaneId(0)

  private def stateOf(lines: Vector[String], topLine: Int = 0, config: AppConfig = AppConfig.conventional): AppState =
    val buffer   = Buffer.fromString(BufferId(1), lines.mkString("\n"))
    val scrolled = buffer.copy(viewport = buffer.viewport.scrolledTo(topLine, 0, 0))
    val initial  = AppState.initial(AppConfig.conventional)
    val state = initial.copy(
      runtime = initial.runtime.copy(viewportSize = Some(viewportSize)),
      persisted = initial.persisted.copy(
        buffers = Map(buffer.id -> scrolled),
        bufferOrder = List(buffer.id),
        layout = initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = config
      )
    )
    LayoutEngine.syncViewportDimensions(state, viewportSize)

  private def sceneOf(state: AppState): UiSceneSnapshot = AuthoritativeUiScene().forState(state, viewportSize)

  private val shortLines = Vector.tabulate(300)(line => s"line $line")
  private val paragraphs = Vector.tabulate(60)(line => s"Paragraph $line " + "lorem ipsum dolor sit amet " * 40)

  "A pane's scroll" should "count a row per line while the lines do not wrap" in {
    val scroll = sceneOf(stateOf(shortLines, topLine = 40)).paneScrolls.getOrElse(paneId, fail("expected a scroll"))

    scroll.extent.totalRows shouldBe 300
    scroll.extent.firstRow shouldBe 40
    scroll.extent.visibleRows should be > 10
  }

  it should "count the rows a wrapped line takes, not the line" in {
    val scroll = sceneOf(stateOf(paragraphs, topLine = 10)).paneScrolls.getOrElse(paneId, fail("expected a scroll"))

    scroll.extent.totalRows should be > paragraphs.size * 2
    scroll.extent.firstRow should be > 20
  }

  it should "count a row per line with word wrap off, however long the lines" in {
    val unwrapped = stateOf(paragraphs, config = AppConfig.conventional.withWordWrap(false))
    val scroll    = sceneOf(unwrapped).paneScrolls.getOrElse(paneId, fail("expected a scroll"))

    scroll.extent.totalRows shouldBe paragraphs.size
  }

  it should "be there in a terminal too, for the bar in its reserved column" in {
    val tui = stateOf(shortLines)

    sceneOf(tui.copy(runtime = tui.runtime.copy(capabilities = FrontendCapabilities.tui()))).paneScrolls.keySet shouldBe
      Set(paneId)
  }

  it should "be absent from a pane laid out in pages of columns" in {
    val surface = AppConfig.conventional.surfaceConfig.copy(columnModeEnabled = true, wordWrapEnabled = true)
    val paged   = stateOf(shortLines, config = AppConfig.conventional.copy(surfaceConfig = surface))

    sceneOf(paged).paneScrolls shouldBe empty
  }
