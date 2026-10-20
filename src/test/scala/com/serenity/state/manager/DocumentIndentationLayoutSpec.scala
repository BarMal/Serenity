package com.serenity.state.manager

import com.serenity.frontend.FrontendCapabilities
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{LayoutEngine, ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A tab wraps by the width of the buffer it is in, not only by the global setting (#1996). */
class DocumentIndentationLayoutSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private val global = IndentationSettings(tabSize = 4)

  private def tuiStateWith(buffer: Buffer): AppState =
    val base = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = AppState.initial.persisted.config.withIndentation(global)
      )
    )
    base.copy(runtime =
      base.runtime.copy(capabilities = FrontendCapabilities.tui(), viewportSize = Some(ViewportSize(80, 24)))
    )

  private def panelWidth: Int =
    LayoutEngine
      .calculateLayout(tuiStateWith(Buffer.fromString(bufferId, "probe")), ViewportSize(80, 24))
      .editorPanelRect
      .width

  // Fits the panel at the global tab size of 4, and wraps once a tab is 8 wide.
  private def tabbedLine: String = "\t" * (panelWidth / 5) + "end"

  private def visualRows(indentation: DocumentIndentation): Int =
    val fresh  = Buffer.fromString(bufferId, tabbedLine)
    val buffer = fresh.copy(document = fresh.document.copy(indentation = indentation))
    EditorGeometryProducer
      .forPane(tuiStateWith(buffer), paneId)
      .getOrElse(fail("expected geometry for pane"))
      .navigation
      .visualLines
      .size

  "Wrapping a tab-indented line" should "use the global tab size for a buffer with no indentation of its own" in {
    visualRows(DocumentIndentation()) shouldBe 1
  }

  it should "use the width the user chose for the buffer" in {
    visualRows(DocumentIndentation().withChosen(Some(IndentationSettings(tabSize = 8, insertSpaces = false)))) should
      be > 1
  }

  it should "use the width detected for the buffer" in {
    visualRows(DocumentIndentation(detected = IndentationHint(tabSize = Some(8)))) should be > 1
  }

  it should "fit again when the buffer's own width is narrower than the global one" in {
    visualRows(DocumentIndentation(detected = IndentationHint(tabSize = Some(2)))) shouldBe 1
  }
