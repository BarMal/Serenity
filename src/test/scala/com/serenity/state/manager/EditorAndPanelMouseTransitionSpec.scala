package com.serenity.state.manager

import java.nio.file.Paths

import com.serenity.DockedPanelFixtures
import com.serenity.config.TextAreaInsets
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.components.ComponentResult
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The editor-targeting and pinned-panel mouse handlers as pure functions of a constructed `AppState` (and, for editor
  * targeting, the render-derived `MouseTargetCache` contents) -- no `Ref`, no `IO`.
  */
class EditorAndPanelMouseTransitionSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport = ViewportSize(100, 32)

  private def editorState(text: String): AppState =
    val base = AppState.initial
    base.copy(
      persisted = base.persisted.copy(buffers = Map(BufferId(0) -> Buffer.fromString(BufferId(0), text))),
      runtime = base.runtime.copy(viewportSize = Some(viewport))
    )

  private def run[A](state: AppState)(transition: Transition[A]): (ReducerResult, A) =
    MouseTransition.run(state)(transition)

  private def buffer(state: AppState): Buffer = state.persisted.buffers(BufferId(0))

  private def contentRect(state: AppState): LayoutRect =
    MouseTargetCache.fromState(state, viewport).scene.paneLayouts(PaneId(0)).contentRect

  "EditorMouseTargeting.targetAt" should "resolve a point inside the pane to that pane, its buffer, and a cursor" in {
    val state = editorState("alpha beta\ngamma")
    val rect  = contentRect(state)

    val target = EditorMouseTargeting.targetAt(MouseClick(rect.x, rect.y), state, MouseTargetCache.fromState(state, viewport))

    target.map(_.map((paneId, targetBuffer, _) => (paneId, targetBuffer.id))) shouldBe Right(Some((PaneId(0), BufferId(0))))
  }

  it should "resolve a point outside every pane to no target" in {
    val state = editorState("alpha")

    EditorMouseTargeting.targetAt(MouseClick(500, 500), state, MouseTargetCache.fromState(state, viewport)) shouldBe
      Right(None)
  }

  "EditorMouseTargeting.hover" should "record the hovered editor position" in {
    val state = editorState("alpha")

    val hovered = run(state)(EditorMouseTargeting.hover(Some((PaneId(0), buffer(state), CursorPosition(0, 2)))))._1

    hovered.state.runtime.hoveredEditorTarget shouldBe
      Some(HoveredEditorTarget(PaneId(0), BufferId(0), CursorPosition(0, 2)))
    hovered.effects shouldBe Nil
  }

  it should "leave the state untouched when that position is already the hovered one, or nothing is hovered" in {
    val state   = editorState("alpha")
    val target  = Some((PaneId(0), buffer(state), CursorPosition(0, 2)))
    val hovered = run(state)(EditorMouseTargeting.hover(target))._1.state

    run(hovered)(EditorMouseTargeting.hover(target))._1.state shouldBe theSameInstanceAs(hovered)
    run(state)(EditorMouseTargeting.hover(None))._1.state shouldBe theSameInstanceAs(state)
    run(hovered)(EditorMouseTargeting.hover(None))._1.state.runtime.hoveredEditorTarget shouldBe None
  }

  "MouseHitTesting.editorClick" should "move the cursor to the clicked position and focus the pane" in {
    val state = editorState("alpha beta")

    val result = run(state)(
      MouseHitTesting.editorClick(MouseClick(0, 0), Some((PaneId(0), buffer(state), CursorPosition(0, 3))))
    )._1

    buffer(result.state).editing.cursorPositions shouldBe List(CursorPosition(0, 3))
    buffer(result.state).primarySelection shouldBe None
    result.state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    result.effects shouldBe Nil
  }

  it should "select the clicked word on a double click" in {
    val state = editorState("alpha beta")

    val result = run(state)(
      MouseHitTesting.editorClick(MouseClick(0, 0, clickCount = 2), Some((PaneId(0), buffer(state), CursorPosition(0, 7))))
    )._1

    buffer(result.state).primarySelection shouldBe Some(Selection(CursorPosition(0, 6), CursorPosition(0, 10)))
  }

  it should "only dismiss an open context menu when it lands on no editor target" in {
    val state  = editorState("alpha")
    val opened = run(state)(EditorContextMenuHitTesting.open(Some((PaneId(0), buffer(state), CursorPosition(0, 1)))))._1.state

    val result = run(opened)(MouseHitTesting.editorClick(MouseClick(0, 0), None))._1

    result.state.contextMenuSurface shouldBe None
    buffer(result.state).editing shouldBe buffer(opened).editing
  }

  "MouseHitTesting.editorPress and editorDrag" should "extend a selection from the pressed position to the dragged one" in {
    val state   = editorState("alpha beta")
    val pressed = run(state)(MouseHitTesting.editorPress(MousePress(0, 0), Some((PaneId(0), buffer(state), CursorPosition(0, 1)))))._1.state

    val dragged = run(pressed)(MouseHitTesting.editorDrag(Some((PaneId(0), buffer(pressed), CursorPosition(0, 4)))))._1

    buffer(dragged.state).primarySelection shouldBe Some(Selection(CursorPosition(0, 1), CursorPosition(0, 4)))
    dragged.effects shouldBe Nil
  }

  private val root     = Paths.get("/repo")
  private val src      = root.resolve("src")
  private val readme   = root.resolve("README.md")
  private val explorer = SurfaceId("explorer")

  private def withExplorer: AppState =
    val tree = DirectoryTreeData(
      root,
      entries = Map(root -> List(DirEntry(src, "src", isDirectory = true), DirEntry(readme, "README.md", isDirectory = false)))
    )
    DockedPanelFixtures.dock(
      editorState("text"),
      explorer,
      SurfaceContent.DirectoryTree(tree, Some(root)),
      PanelPosition.Left,
      28
    )

  private def explorerRowPoint(state: AppState, displayedItemRow: Int): (Int, Int) =
    val contract = EditorLayoutContract.from(state, viewport, LayoutEngine.calculateLayoutWithUI(state, viewport))
    val content  = contract.panelContentRect(explorer).getOrElse(fail("Expected the explorer's content rect"))
    val rowY = contract
      .panelRowSlots(explorer)
      .collectFirst { case SurfaceContentRowSlot(SurfaceContentRowKind.Item(`displayedItemRow`), y) => y }
      .getOrElse(fail(s"Expected explorer row $displayedItemRow"))
    (content.x + 1, rowY)

  private def selectedPath(state: AppState): Option[java.nio.file.Path] =
    state.surfaceById(explorer).map(_.content) match
      case Some(SurfaceContent.DirectoryTree(_, selected)) => selected
      case other                                           => fail(s"Expected the explorer tree, got $other")

  "PinnedPanelMouseHitTesting.select" should "select and focus the clicked directory row" in {
    val state    = withExplorer
    val (x, y)   = explorerRowPoint(state, 2)

    val (result, claimed) = run(state)(PinnedPanelMouseHitTesting.select(MouseClick(x, y), state, focusPanel = true))

    claimed shouldBe true
    selectedPath(result.state) shouldBe Some(readme)
    result.state.persisted.focus shouldBe Focus.Surface(explorer)
  }

  "PinnedPanelMouseHitTesting.hover" should "highlight the hovered row without taking focus" in {
    val state  = withExplorer
    val (x, y) = explorerRowPoint(state, 2)

    val (result, claimed) = run(state)(PinnedPanelMouseHitTesting.hover(MouseMove(x, y), state))

    claimed shouldBe true
    selectedPath(result.state) shouldBe Some(readme)
    result.state.persisted.focus shouldBe state.persisted.focus
    run(result.state)(PinnedPanelMouseHitTesting.hover(MouseMove(x, y), result.state))._1.state shouldBe
      theSameInstanceAs(result.state)
  }

  "PinnedPanelMouseHitTesting.activation" should "open a double-clicked file row, and do nothing for a single click" in {
    val state    = withExplorer
    val (x, y)   = explorerRowPoint(state, 2)
    val selected = run(state)(PinnedPanelMouseHitTesting.select(MouseClick(x, y), state, focusPanel = true))._1.state

    PinnedPanelMouseHitTesting.activation(MouseClick(x, y, clickCount = 2), selected) should matchPattern {
      case Some(ComponentResult.ReducerUpdate(ReducerResult(_, List(AppEffect.File(FileEffect.DirectLoadFile(`readme`)))))) =>
    }
    PinnedPanelMouseHitTesting.activation(MouseClick(x, y), selected) shouldBe None
  }

  "PinnedPanelMouseHitTesting.textAreaInsetFromDrag" should "resolve a drag in the top spacer to a top inset" in {
    val base = editorState("text")
    val state = base.copy(persisted =
      base.persisted.copy(config =
        base.persisted.config.withTextAreaInsets(TextAreaInsets(left = 0.0, right = 0.0, top = 0.20))
      )
    )
    val layout        = LayoutEngine.calculateLayout(state, viewport)
    val contentHeight = layout.editorPanelRect.bottom - layout.topSpacerRect.y
    val drag          = MouseDrag(layout.topSpacerRect.x + 2, layout.topSpacerRect.y + 3)

    PinnedPanelMouseHitTesting.textAreaInsetFromDrag(drag, state) shouldBe
      Some(PinnedPanelMouseHitTesting.TextAreaInsetDrag.Top(3.0 / contentHeight.toDouble))
  }
end EditorAndPanelMouseTransitionSpec
