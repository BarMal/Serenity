package com.serenity.ui.layout

import com.serenity.DockedPanelFixtures
import com.serenity.config.{AppConfig, Gutter, StatusLinePlacement, TextAreaInsets}
import com.serenity.frontend.FrontendCapabilities
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A dock may grow only until the panes beside it are down to their own minimum width: the text `minimum_pane_width`
  * asks for, plus that pane's gutters and its own line-number rails (#2570, PR 4).
  */
class MinimumPaneWidthSpec extends AnyFlatSpec with Matchers:

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
        WorkspaceNode.Split(WorkspaceNodeId(s"split-${first.value}"), axis, 1.0 / rest.size, leaf(first), build(tail))
      case Nil => leaf(PaneId(0))
    WorkspaceTree(build(panes))

  /** A terminal workspace of panes showing documents of `lineCounts` lines, with a right dock asking for far more than
    * there is room for, so that what it ends up with is decided by the panes' minimums.
    */
  private def dockedState(axis: SplitAxis, lineCounts: List[Int], insets: TextAreaInsets): AppState =
    val panes   = lineCounts.indices.map(PaneId(_)).toList
    val buffers = lineCounts.zipWithIndex.map((lines, id) => numbered(lines, id))
    val layout = Layout(
      editorPanes = panes.zip(buffers).map((pane, buffer) => pane -> EditorPane.withBuffer(pane, buffer.id)).toMap,
      activeEditorPaneId = panes.headOption,
      workspaceTree = Some(tree(axis, panes))
    )
    val config = AppConfig.default
      .withLineNumbers(true)
      .withStatusLinePlacement(StatusLinePlacement.Pinned)
      .withTextAreaInsets(insets)
      .withMinimumPaneWidth(30)
    val base = AppState(persisted =
      Persisted(
        layout = layout,
        buffers = buffers.map(buffer => buffer.id -> buffer).toMap,
        focus = Focus.EditorPane(panes.head),
        config = config
      )
    )
    val sized =
      base.copy(runtime = base.runtime.copy(viewportSize = Some(viewport), capabilities = FrontendCapabilities.tui()))
    DockedPanelFixtures.dockAllContent(
      sized,
      List((SurfaceId("right-dock"), SurfaceContent.Diagnostics(Nil), PanelPosition.Right, 150))
    )

  private def minimumOf(state: AppState, pane: PaneId): Int =
    PaneTextArea.minimumWidth(
      state,
      pane,
      state.persisted.config.surfaceConfig.textAreaInsets.normalized,
      state.persisted.config.editorConfig.minimumPaneWidth
    )

  private def workspaceWidth(state: AppState): Int = LayoutEngine.calculateLayout(state, viewport).editorPanelRect.width

  private val noInsets = TextAreaInsets.percent(0.0, 0.0)
  private val tenth    = TextAreaInsets.percent(0.10, 0.10)
  private val mixed    = TextAreaInsets(Gutter.Cells(4), Gutter.Percent(0.05), Gutter.Zero, Gutter.Zero)

  "A dock asking for the whole window" should "leave one pane exactly its own minimum width" in
    List(noInsets, tenth, mixed).foreach { insets =>
      val state = dockedState(SplitAxis.Horizontal, List(30), insets)
      withClue(s"$insets: ")(workspaceWidth(state) shouldBe minimumOf(state, PaneId(0)))
    }

  it should "leave side-by-side panes the sum of their own minimums, not the largest one for each" in
    List(noInsets, tenth, mixed).foreach { insets =>
      val state = dockedState(SplitAxis.Horizontal, List(9, 120_000), insets)
      val own   = List(PaneId(0), PaneId(1)).map(minimumOf(state, _))
      own.distinct.size shouldBe 2
      withClue(s"$insets: ")(workspaceWidth(state) shouldBe own.sum)
    }

  it should "leave three side-by-side panes the sum of their own minimums" in {
    val state = dockedState(SplitAxis.Horizontal, List(9, 120_000, 1500), mixed)
    workspaceWidth(state) shouldBe List(PaneId(0), PaneId(1), PaneId(2)).map(minimumOf(state, _)).sum
  }

  it should "leave stacked panes the larger of their minimums, as they share the width" in {
    val state = dockedState(SplitAxis.Vertical, List(9, 120_000), tenth)
    workspaceWidth(state) shouldBe List(PaneId(0), PaneId(1)).map(minimumOf(state, _)).max
  }
