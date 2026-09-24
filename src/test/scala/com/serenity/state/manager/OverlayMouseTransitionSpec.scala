package com.serenity.state.manager

import com.serenity.ContextualToolbarTestSupport
import com.serenity.app.AppStartup
import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.document.CommentRendering
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The overlay mouse handlers as pure transitions: each is run straight against a constructed `AppState`, with no
  * `Ref` and no `IO`, and asserted on the state and the effects it hands back. Commands a click triggers come back as
  * `AppEffect.ExecuteCommand` values rather than being run.
  */
class OverlayMouseTransitionSpec extends AnyFlatSpec with Matchers with ContextualToolbarTestSupport:

  private val viewport = ViewportSize(80, 24)
  private val registry = CommandRegistry.withToggleUI

  private def editorState(text: String, size: ViewportSize = viewport): AppState =
    val base = AppState.initial
    base.copy(
      persisted = base.persisted.copy(buffers = Map(BufferId(0) -> Buffer.fromString(BufferId(0), text))),
      runtime = base.runtime.copy(viewportSize = Some(size))
    )

  private def run[A](state: AppState)(transition: Transition[A]): (ReducerResult, A) =
    MouseTransition.run(state)(transition)

  private def surfaceFrame(state: AppState, surfaceId: SurfaceId): LayoutRect =
    UiSceneSnapshot
      .from(state, state.runtime.viewportSize.getOrElse(fail("Expected a viewport")))
      .nodesInPaintOrder
      .find(_.id == SceneNodeId.Surface(surfaceId))
      .map(_.frameRect)
      .getOrElse(fail(s"Expected a scene node for ${surfaceId.value}"))

  private def clickAt(region: SurfaceHitRegion): MouseClick =
    MouseClick(region.rect.x.toInt, region.rect.y.toInt)

  "StartupPageMouseHitTesting.click" should "emit the clicked launch action's command, leaving state untouched" in {
    val page        = AppStartup.createStartPage(sessionExists = false, recentFiles = Nil)
    val codeMetrics = CellMetrics(charWidth = 8, lineHeight = 12, ascent = 9)
    val uiMetrics   = CellMetrics(charWidth = 11, lineHeight = 24, ascent = 18)
    val base        = AppState.empty
    val state = base.copy(runtime =
      base.runtime.copy(
        viewportSize = Some(viewport),
        uiSurfaces = List(
          UiSurface(
            SurfaceId("startup"),
            SurfaceContent.StartPage(page),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val bounds = page.actionBounds(viewport, codeMetrics, uiMetrics).headOption.getOrElse(fail("Expected bounds"))
    val click = MouseClick(
      col = codeMetrics.toCol(bounds.xPx + 1),
      row = codeMetrics.toRow(bounds.yPx + 1),
      pixelX = Some(bounds.xPx + 1),
      pixelY = Some(bounds.yPx + 1),
      renderMetrics = Some(MouseRenderMetrics(codeMetrics, uiMetrics))
    )
    val expected = page.launchActions.headOption.getOrElse(fail("Expected a launch action")).command

    val (result, claimed) = run(state)(StartupPageMouseHitTesting.click(click, state))

    claimed shouldBe true
    result.state shouldBe theSameInstanceAs(state)
    result.effects shouldBe List(AppEffect.ExecuteCommand(expected))
  }

  it should "decline a click with no startup page" in {
    val state             = editorState("text")
    val (result, claimed) = run(state)(StartupPageMouseHitTesting.click(MouseClick(1, 1), state))

    claimed shouldBe false
    result shouldBe ReducerResult(state, Nil)
  }

  "CommentLensMouseHitTesting.click" should "turn a read-only lens editable on a click inside its body" in {
    val comment = DocumentComment(CursorPosition(0, 0), CursorPosition(0, 5), "A note about hello")
    val base    = editorState("hello world")
    val buffer  = base.persisted.buffers(BufferId(0))
    val withComment = base.copy(persisted =
      base.persisted.copy(buffers =
        Map(
          BufferId(0) -> buffer.copy(
            annotations = buffer.annotations.copy(documentComments = List(comment)),
            editing = EditingState(List(CursorPosition(0, 2)))
          )
        )
      )
    )
    val opened = CommentRendering.openLensAtCursor(withComment, CommentLensMode.ReadOnly)
    val lensId = opened.commentLensSurface.map(_.id).getOrElse(fail("Expected a comment lens"))
    val frame = EditorLayoutContract
      .from(opened, viewport, LayoutEngine.calculateLayoutWithUI(opened, viewport))
      .overlayRect(lensId)
      .getOrElse(fail("Expected the lens overlay rect"))

    val (result, claimed) = run(opened)(CommentLensMouseHitTesting.click(MouseClick(frame.x, frame.y), opened))

    claimed shouldBe true
    result.effects shouldBe Nil
    result.state.commentLensSurface.map(_.content) should matchPattern {
      case Some(SurfaceContent.CommentLens(lens)) if lens.mode == CommentLensMode.Editable =>
    }
  }

  it should "decline a click when no lens is open" in {
    val state = editorState("hello world")

    run(state)(CommentLensMouseHitTesting.click(MouseClick(1, 1), state))._2 shouldBe false
  }

  private def withOpenContextMenu: (AppState, ContextMenu, LayoutRect) =
    val state  = editorState("alpha beta")
    val buffer = state.persisted.buffers(BufferId(0))
    val opened =
      run(state)(EditorContextMenuHitTesting.open(Some((PaneId(0), buffer, CursorPosition(0, 1)))))._1.state
    val menu = opened.contextMenuSurface.map(_.content) match
      case Some(SurfaceContent.ContextMenu(menu)) => menu
      case other                                  => fail(s"Expected an open context menu, got $other")
    (opened, menu, surfaceFrame(opened, SurfaceId("context-menu")))

  private def contextMenuItem(state: AppState, menu: ContextMenu, frame: LayoutRect, index: Int): SurfaceHitRegion =
    ContextMenuSurfaceComposition
      .forMenu(
        menu,
        frame,
        state.persisted.config.effectiveCommandRunnerItemGapRows,
        SurfaceFrameLayout
          .itemTargetRowsFor(SurfaceContent.ContextMenu(menu), state.persisted.config.interfaceDensity)
      )
      .hitRegions
      .find(_.focusId.value == s"context-menu-item-$index")
      .getOrElse(fail(s"Expected context menu item $index"))

  "EditorContextMenuHitTesting.open" should "open the menu on the target pane and focus it" in {
    val (opened, menu, _) = withOpenContextMenu

    menu.targetFocus shouldBe Focus.EditorPane(PaneId(0))
    opened.persisted.focus shouldBe Focus.Surface(SurfaceId("context-menu"))
  }

  "EditorContextMenuHitTesting.click" should "dismiss the menu, restore focus, and emit the clicked item's command" in {
    val (opened, menu, frame) = withOpenContextMenu
    val item                  = contextMenuItem(opened, menu, frame, 0)

    val (result, claimed) = run(opened)(EditorContextMenuHitTesting.click(clickAt(item), opened))

    claimed shouldBe true
    result.state.contextMenuSurface shouldBe None
    result.state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    result.effects shouldBe List(AppEffect.ExecuteCommand(menu.items.head.command))
  }

  it should "dismiss an open menu on a click outside it, emitting nothing" in {
    val (opened, _, frame) = withOpenContextMenu
    val outside            = MouseClick(frame.right + 2, frame.bottom + 2)

    val (result, claimed) = run(opened)(EditorContextMenuHitTesting.click(outside, opened))

    claimed shouldBe true
    result.state.contextMenuSurface shouldBe None
    result.effects shouldBe Nil
  }

  "EditorContextMenuHitTesting.hover" should "move the menu's selection to the hovered item" in {
    val (opened, menu, frame) = withOpenContextMenu
    val item                  = contextMenuItem(opened, menu, frame, 1)

    val (result, claimed) = run(opened)(EditorContextMenuHitTesting.hover(MouseMove(item.rect.x.toInt, item.rect.y.toInt), opened))

    claimed shouldBe true
    result.effects shouldBe Nil
    result.state.contextMenuSurface.map(_.content) should matchPattern {
      case Some(SurfaceContent.ContextMenu(hovered)) if hovered.selectedIndex == 1 =>
    }
  }

  "ContextualToolbarHitTesting.click" should "emit a button's command and hand focus back to the editor" in {
    val base   = editorState("alpha beta", ViewportSize(160, 40))
    val buffer = base.persisted.buffers(BufferId(0))
    val selection = Selection(CursorPosition(0, 6), CursorPosition(0, 10))
    val selected = base.copy(persisted =
      base.persisted.copy(buffers =
        Map(BufferId(0) -> buffer.copy(editing = EditingState.fromCursors(List(Cursor(selection.focus, Some(selection.anchor))))))
      )
    )
    val state = AppEventReducer.reduce(ToggleContextualToolbar, selected, registry).state
    val point = toolbarItemPoint(state, itemId = "italic")

    val (result, claimed) = run(state)(ContextualToolbarHitTesting.click(MouseClick(point.x, point.y), state))

    claimed shouldBe true
    result.state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    result.effects should matchPattern { case List(AppEffect.ExecuteCommand(command)) if command.name == "italic" => }
  }

  it should "decline a click with no toolbar open" in {
    val state = editorState("alpha beta")

    run(state)(ContextualToolbarHitTesting.click(MouseClick(1, 1), state)) shouldBe ((ReducerResult(state, Nil), false))
  }

  private def openCommandRunner: AppState =
    AppEventReducer.reduce(ToggleCommandRunner, editorState("text", ViewportSize(100, 30)), registry).state

  private def runnerFrom(state: AppState): CommandRunner =
    state.commandRunnerSurface.map(_.content) match
      case Some(SurfaceContent.CommandPalette(runner)) => runner
      case other                                       => fail(s"Expected an open command runner, got $other")

  private def commandRunnerRow(state: AppState, index: Int): SurfaceHitRegion =
    val surface = state.commandRunnerSurface.getOrElse(fail("Expected a command runner surface"))
    CommandRunnerSurfaceComposition
      .forRunner(
        runnerFrom(state),
        surfaceFrame(state, surface.id),
        state.persisted.config.effectiveCommandRunnerItemGapRows,
        SurfaceFrameLayout.itemTargetRowsFor(surface.content, state.persisted.config.interfaceDensity),
        showKeyHints = state.persisted.config.surfaceConfig.commandRunnerShowKeyHints
      )
      .hitRegions
      .find(region => CommandRunnerSurfaceComposition.absoluteIndexOf(region.focusId).contains(index))
      .getOrElse(fail(s"Expected command runner row $index"))

  "CommandRunnerMouseHitTesting.hover" should "select the hovered row without emitting anything" in {
    val state = openCommandRunner
    val row   = commandRunnerRow(state, 2)

    val (result, claimed) =
      run(state)(CommandRunnerMouseHitTesting.hover(MouseMove(row.rect.x.toInt, row.rect.y.toInt), state))

    claimed shouldBe true
    result.effects shouldBe Nil
    runnerFrom(result.state).selectedIndex shouldBe 2
  }

  "CommandRunnerMouseHitTesting.click" should "select the clicked row and emit its command, as Enter would" in {
    val state    = openCommandRunner
    val row      = commandRunnerRow(state, 2)
    val hovered  = run(state)(CommandRunnerMouseHitTesting.hover(MouseMove(row.rect.x.toInt, row.rect.y.toInt), state))
    val expected = runnerFrom(hovered._1.state).selectedCommand.getOrElse(fail("Expected a selected command"))

    val (result, claimed) = run(state)(CommandRunnerMouseHitTesting.click(clickAt(row), state))

    claimed shouldBe true
    result.effects shouldBe List(AppEffect.ExecuteCommand(expected))
  }

  "ModalMouseHitTesting.input" should "pick the clicked close-prompt choice and submit it in one transition" in {
    val base     = editorState("alpha")
    val dialogId = SurfaceId("close-confirmation")
    val workflow = CloseWorkflowState(CloseScope.Current, BufferId(0), "notes.scala")
    val state = base.copy(
      persisted = base.persisted.copy(focus = Focus.Modal),
      runtime = base.runtime.copy(modalStack =
        base.runtime.modalStack :+ ModalDialog(dialogId, Modal.CloseWorkflow(workflow), ModalPlacement.Centered)
      )
    )
    val cancel = ModalSurfaceComposition
      .forModal(
        Modal.CloseWorkflow(workflow),
        surfaceFrame(state, dialogId),
        SurfaceFrameLayout.minimumTargetRows(state.persisted.config.interfaceDensity)
      )
      .getOrElse(fail("Expected the close prompt composition"))
      .hitRegions
      .find(_.actionId.contains(SurfaceActionId("close-cancel")))
      .getOrElse(fail("Expected a cancel action"))

    val (result, _) = run(state)(ModalMouseHitTesting.input(clickAt(cancel), state))

    result.state.topModal.map(_.modal) shouldBe Some(
      Modal.CloseWorkflow(workflow.copy(selectedChoice = CloseWorkflowChoice.Cancel))
    )
    result.effects shouldBe List(AppEffect.Workflow(WorkflowEffect.SubmitCloseWorkflow(dialogId)))
  }

  it should "ignore anything but a primary click" in {
    val state = editorState("alpha")

    run(state)(ModalMouseHitTesting.input(MouseMove(1, 1), state))._1 shouldBe ReducerResult(state, Nil)
  }
end OverlayMouseTransitionSpec
