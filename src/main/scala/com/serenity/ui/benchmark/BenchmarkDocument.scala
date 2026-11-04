package com.serenity.ui.benchmark

import com.serenity.config.{AppConfig, TextAreaInsets}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import com.serenity.ui.theme.Theme

/** The one document both renderers are timed on: blocks of prose and of code, long enough to scroll through, so it is
  * the same text, wrapped the same way, whichever backend draws it.
  */
object BenchmarkDocument:

  val bufferId: BufferId = BufferId(1)
  val paneId: PaneId     = PaneId(0)

  /** The line the typing scenario edits, and where that line starts. */
  val typedLine: Int = 4

  private val ProseBlock = 12
  private val CodeBlock  = 24
  private val LineCount  = 1200

  private def proseLine(index: Int): String =
    s"Paragraph $index " + "lorem ipsum dolor sit amet " * (4 + index % 5 * 4)

  private def codeLine(index: Int): String =
    s"  def step$index(value: Int): Int = value * ${index % 7 + 2} + ${index % 13}"

  val lines: Vector[String] =
    Vector.tabulate(LineCount) { index =>
      if index % (ProseBlock + CodeBlock) < ProseBlock then proseLine(index) else codeLine(index)
    }

  /** Where line `typedLine` starts in the text. */
  val typedOffset: Int = lines.take(typedLine).map(_.length + 1).sum

  def initial(using Balance): AppState =
    val base = Buffer.fromString(bufferId, lines.mkString("\n"))
    val buffer = base.copy(
      document = base.document.copy(isDirty = true, language = None),
      viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 40, visibleColumns = 120),
      editing = EditingState(List(CursorPosition(typedLine, 0)))
    )
    val state = AppState.initial(AppConfig.conventional)
    state.copy(persisted =
      state.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = state.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        theme = Theme.light,
        config = AppConfig.conventional.withoutStatusLine.copy(surfaceConfig =
          AppConfig.conventional.surfaceConfig.copy(textAreaInsets = TextAreaInsets(0.04, 0.04, 0.04, 0.04))
        )
      )
    )
