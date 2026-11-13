package com.serenity.ui.layout

import com.serenity.config.{AppConfig, Gutter, StatusLinePlacement, TextAreaInsets}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Every pane carries its own text area: `[left spacer][left rail][text][right rail][right spacer]` inside its pane
  * rect, sized from that pane's width and buffer (#2570). One pane keeps the geometry the workspace used to give it.
  */
class PaneTextAreaLayoutSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport = ViewportSize(160, 40)

  private def numbered(lines: Int, id: Int): Buffer =
    Buffer.fromString(BufferId(id), (1 to lines).map(n => s"line $n").mkString("\n"))

  private def leaf(pane: PaneId): WorkspaceNode =
    WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${pane.value}"), pane)

  private def tree(axis: SplitAxis, panes: List[PaneId]): WorkspaceTree =
    def build(rest: List[PaneId]): WorkspaceNode = rest match
      case only :: Nil => leaf(only)
      case first :: tail =>
        WorkspaceNode.Split(
          WorkspaceNodeId(s"split-${first.value}"),
          axis,
          1.0 / rest.size,
          leaf(first),
          build(tail)
        )
      case Nil => leaf(PaneId(0))
    WorkspaceTree(build(panes))

  private def stateWith(
    axis: SplitAxis,
    lineCounts: List[Int],
    lineNumbers: Boolean,
    insets: TextAreaInsets
  ): AppState =
    val panes   = lineCounts.indices.map(PaneId(_)).toList
    val buffers = lineCounts.zipWithIndex.map((lines, id) => numbered(lines, id))
    val editorPanes =
      panes.zip(buffers).map((pane, buffer) => pane -> EditorPane.withBuffer(pane, buffer.id)).toMap
    val layout = Layout(
      editorPanes = editorPanes,
      activeEditorPaneId = panes.headOption,
      workspaceTree = Some(tree(axis, panes))
    )
    val config = AppConfig.default
      .withLineNumbers(lineNumbers)
      .withStatusLinePlacement(StatusLinePlacement.Pinned)
      .withTextAreaInsets(insets)
    AppState(persisted =
      Persisted(
        layout = layout,
        buffers = buffers.map(buffer => buffer.id -> buffer).toMap,
        focus = Focus.EditorPane(panes.head),
        config = config
      )
    )

  private def layoutsOf(state: AppState): Map[PaneId, EditorPaneLayout] =
    val calculated = LayoutEngine.calculateLayout(state, viewport)
    LayoutEngine.calculateEditorWorkspaceLayout(state, calculated).paneLayouts

  private val insetCases = List(
    "no insets"             -> TextAreaInsets.percent(0.0, 0.0),
    "10% each side"         -> TextAreaInsets.percent(0.10, 0.10),
    "an auto left, 6 cells" -> TextAreaInsets(Gutter.Auto, Gutter.Cells(6), Gutter.Percent(0.0), Gutter.Percent(0.0))
  )

  private val axes = List(SplitAxis.Horizontal, SplitAxis.Vertical)

  for
    (name, insets) <- insetCases
    lineNumbers    <- List(false, true)
    axis           <- axes
    paneCount      <- List(1, 2, 3)
  do
    s"$paneCount pane(s) split $axis with line numbers=$lineNumbers and $name" should
      "order each pane as spacer, rail, text, spacer inside its pane rect" in {
        val state   = stateWith(axis, List.fill(paneCount)(30), lineNumbers, insets)
        val layouts = layoutsOf(state)
        layouts should have size paneCount.toLong
        layouts.values.foreach { pane =>
          pane.leftSpacerRect.x shouldBe pane.paneRect.x
          pane.rightSpacerRect.right shouldBe pane.paneRect.right
          pane.contentRect.x should be >= pane.leftSpacerRect.right
          pane.rightSpacerRect.x should be >= pane.contentRect.right
          pane.lineNumberRect.foreach { rail =>
            rail.x should be >= pane.leftSpacerRect.right
            rail.right should be <= pane.contentRect.x
          }
          pane.leftSpacerRect.y shouldBe pane.paneRect.y
          pane.leftSpacerRect.height shouldBe pane.paneRect.height
          pane.rightSpacerRect.height shouldBe pane.paneRect.height
          pane.lineNumberRect.isDefined shouldBe lineNumbers
          pane.rightLineNumberRect.isDefined shouldBe false
        }
      }

  "A pane's gutters" should "be sized from that pane's own width, not the workspace's" in {
    val state   = stateWith(SplitAxis.Horizontal, List(30, 30), lineNumbers = false, TextAreaInsets.percent(0.10, 0.10))
    val layouts = layoutsOf(state)
    layouts.values.foreach { pane =>
      pane.leftSpacerRect.width shouldBe (pane.paneRect.width * 0.10).toInt
      pane.rightSpacerRect.width shouldBe (pane.paneRect.width * 0.10).toInt
    }
  }

  "A pane's rail" should "be as wide as that pane's own buffer needs, not the widest buffer's" in {
    val state = stateWith(SplitAxis.Horizontal, List(9, 120_000), lineNumbers = true, TextAreaInsets.percent(0.0, 0.0))
    val short = layoutsOf(state)(PaneId(0))
    val long  = layoutsOf(state)(PaneId(1))
    short.lineNumberRect.map(_.width) shouldBe Some(3)
    long.lineNumberRect.map(_.width) shouldBe Some(7)
  }

  "The editor panel" should "be the union of the pane rects" in {
    val state = stateWith(SplitAxis.Horizontal, List(30, 30, 30), lineNumbers = true, TextAreaInsets.percent(0.1, 0.1))
    val calculated = LayoutEngine.calculateLayout(state, viewport)
    val layouts    = LayoutEngine.calculateEditorWorkspaceLayout(state, calculated).paneLayouts.values.map(_.paneRect)
    calculated.editorPanelRect.x shouldBe layouts.map(_.x).min
    calculated.editorPanelRect.right shouldBe layouts.map(_.right).max
  }

  "The split minimum" should "count each pane's gutters and rails, so the text keeps its minimum width" in {
    val insets  = TextAreaInsets.percent(0.20, 0.20)
    val state   = stateWith(SplitAxis.Horizontal, List.fill(6)(30), lineNumbers = true, insets)
    val layouts = layoutsOf(state)
    layouts.values.foreach { pane =>
      pane.contentRect.width should be >= 1
      pane.paneRect.width should be >= (pane.leftSpacerRect.width + pane.rightSpacerRect.width + 1)
    }
  }
end PaneTextAreaLayoutSpec
