package com.serenity

import com.serenity.config.CursorMode
import com.serenity.state.models.*
import com.serenity.ui.layout.{ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import com.serenity.ui.renderer.{HardwareCursor, HardwareCursorStyle}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1170: on a surface with a hardware cursor (a terminal), the caret is delegated to it -- shown at the editing
  * position on a visible frame, hidden on a frame whose caret is not visible.
  */
class RendererCursorHardwareSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val paneId      = PaneId(0)
  private val bufferId    = BufferId(1)
  private val viewport    = ViewportSize(80, 24)
  private val codeFont    = java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
  private val cellMetrics = com.serenity.ui.layout.CellMetrics.fromFont(codeFont)

  final private class FakeHardwareCursor extends HardwareCursor:
    private val presentCallsBuffer = scala.collection.mutable.ListBuffer.empty[(Int, Int, HardwareCursorStyle)]
    private val hideCallsCounter   = new java.util.concurrent.atomic.AtomicInteger(0)

    def present(cellX: Int, cellY: Int, style: HardwareCursorStyle): Unit =
      presentCallsBuffer += ((cellX, cellY, style))

    def hide(): Unit =
      val _ = hideCallsCounter.incrementAndGet()

    def presentCallCount: Int = presentCallsBuffer.size
    def hideCallCount: Int    = hideCallsCounter.get()

  private def editorState(mode: CursorMode = CursorMode.Blink): AppState =
    val buffer =
      Buffer.fromString(bufferId, "hello world").copy(editing = EditingState(List(CursorPosition(0, 3))))
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        theme = Theme.light,
        config = AppState.initial.persisted.config.withCursorMode(mode)
      )
    )

  private def renderWith(hardwareCursor: HardwareCursor, cursorVisible: Boolean): Unit =
    val surface = new MockRenderSurface(80, 24, persistentContent = true, hardwareCursorOverride = Some(hardwareCursor))
    val _ = com.serenity.ui.renderer.RendererCursorOverlay.renderCursorOnly(
      editorState(),
      cursorVisible = cursorVisible,
      surface,
      viewport,
      codeFont,
      codeFont,
      codeFont,
      cellMetrics,
      cellMetrics,
      None,
      com.serenity.state.manager.RenderCaches.create()
    )

  "A surface with a hardware cursor" should "show the terminal cursor on a visible frame" in {
    val hardwareCursor = new FakeHardwareCursor
    renderWith(hardwareCursor, cursorVisible = true)

    hardwareCursor.presentCallCount shouldBe 1
    hardwareCursor.hideCallCount shouldBe 0
  }

  it should "hide the terminal cursor on a frame whose caret is not visible" in {
    val hardwareCursor = new FakeHardwareCursor
    renderWith(hardwareCursor, cursorVisible = false)

    hardwareCursor.presentCallCount shouldBe 0
    hardwareCursor.hideCallCount shouldBe 1
  }
