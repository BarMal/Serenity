package com.serenity

import com.serenity.command.*
import com.serenity.config.AppConfig
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.document.RenderedComment
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.OverlayViewModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OverlayViewModelSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def stateWithQuickInfo(text: String): AppState =
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(editing = EditingState(List(CursorPosition(1, 2))))
    val pane = EditorPane.withBuffer(paneId, bufferId)

    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.Surface(SurfaceId("peek"))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("peek"),
            SurfaceContent.QuickInfo(text),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.AboveCursor),
            dismissOnMove = true
          )
        )
      )
    )

  "OverlayViewModel.fromState" should "derive an above-cursor quick-info overlay view from peek state" in {
    val state  = stateWithQuickInfo("List.map(f)")
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

    val overlays = OverlayViewModel.fromState(state, layout)

    overlays.aboveCursor shouldBe defined
    overlays.belowCursor shouldBe None

    val overlay = overlays.aboveCursor.get
    overlay.rows.map(_.plainText) shouldBe List("List.map(f)")
    overlay.rect shouldBe layout.aboveCursorOverlayRect.get
    overlay.contentRect shouldBe Some(
      SurfaceFrameLayout.forContent(overlay.rect, state.runtime.uiSurfaces.head.content).contentRect
    )
  }

  it should "derive a below-cursor modal overlay view from unified floating surfaces" in {
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(editing = EditingState(List(CursorPosition(1, 2))))
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.Surface(SurfaceId("modal"))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("modal"),
            SurfaceContent.ModalWorkflow(Modal.Custom("replace", "needle")),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

    val overlays = OverlayViewModel.fromState(state, layout)

    overlays.belowCursor shouldBe defined

    val overlay = overlays.belowCursor.get
    // Content is painted entirely from `ModalSurfaceComposition` (issue #819); `ModalSurfaceCompositionSpec` covers
    // `Modal.Custom`'s actual paint content in detail.
    overlay.composition shouldBe defined
    overlay.composition.get.paintBoxes.flatMap(_.text) should contain("replace needle")
    overlay.rect shouldBe layout.belowCursorOverlayRect.get
  }

  it should "space command palette item slots without spacing other overlay content" in {
    val buffer =
      Buffer.fromString(bufferId, "one\ntwo\nthree").copy(editing = EditingState(List(CursorPosition(1, 2))))
    val pane   = EditorPane.withBuffer(paneId, bufferId)
    val runner = CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withCommandRunnerItemGapRows(Some(1)),
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.Surface(SurfaceId("command-runner"))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))
    val overlay = OverlayViewModel.fromState(state, layout).belowCursor.getOrElse(fail("Expected command overlay"))

    // Content is painted entirely from `CommandRunnerSurfaceComposition` (issue #819, slice 2); item row spacing is
    // read from its paint boxes, not the plain-rows `contentRowSlots` path this content no longer populates.
    overlay.itemGapRows shouldBe 1
    overlay.composition.toList
      .flatMap(_.paintBoxes)
      .filter(_.focusId.isDefined)
      .map(_.rect.y)
      .sliding(2)
      .foreach {
        case List(first, second) => second - first shouldBe 3
        case _                   => ()
      }
  }

  it should "derive a focused find overlay view beneath the active cursor" in {
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(editing = EditingState(List(CursorPosition(1, 2))))
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.Surface(SurfaceId("find"))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("find"),
            SurfaceContent.ModalWorkflow(Modal.Find("two", List(FindResult(1, 0)), 0)),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

    val overlays = OverlayViewModel.fromState(state, layout)
    val overlay  = overlays.belowCursor.getOrElse(fail("Expected find overlay"))

    // Content is painted entirely from `ModalSurfaceComposition` (issue #819); `ModalSurfaceCompositionSpec` covers
    // `Modal.Find`'s actual paint content in detail.
    overlay.rect shouldBe layout.belowCursorOverlayRect.get
    overlay.composition shouldBe defined
    overlay.composition.toList.flatMap(_.hitRegions).map(_.semanticLabel) shouldBe List("Find", "1. 2:1")
  }

  it should "allocate spaced framed rows for a context menu at compact density" in {
    val save = Command.typed("save", "Save file", CommandIntent.File(FileIntent.SaveCurrentFile), label = "Save")
    val find = Command.typed("find", "Find text", CommandIntent.Edit(EditIntent.FindInCurrentFile), label = "Find")
    val menu = ContextMenu(
      title = "editor",
      targetFocus = Focus.EditorPane(paneId),
      items = List(
        ContextMenuItem(save.name, save.label, save),
        ContextMenuItem(find.name, find.label, find)
      ),
      selectedIndex = 0
    )
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(editing = EditingState(List(CursorPosition(1, 2))))
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default
          .withInterfaceDensity(com.serenity.config.InterfaceDensity.Compact)
          .withCommandRunnerItemGapRows(Some(1)),
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.Surface(SurfaceId("context-menu"))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("context-menu"),
            SurfaceContent.ContextMenu(menu),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

    val overlays = OverlayViewModel.fromState(state, layout)
    val overlay  = overlays.belowCursor.getOrElse(fail("Expected context menu overlay"))

    overlay.rect.height shouldBe SurfaceFrameLayout.frameHeightForItemRows(
      itemRows = 2,
      hasHeader = true,
      hasFooter = true,
      itemGapRows = 1
    )
    // Content is painted entirely from `ContextMenuSurfaceComposition` (issue #819, slice 2); row layout is read from
    // its paint boxes, not the plain-rows `contentRowSlots` path this content no longer populates.
    val composition = overlay.composition.getOrElse(fail("Expected a context menu composition"))
    composition.paintBoxes.map(_.kind) shouldBe List(
      SurfacePaintKind.Text,
      SurfacePaintKind.ActionItem,
      SurfacePaintKind.ActionItem,
      SurfacePaintKind.Text
    )
  }

  it should "derive an interactive command palette view with cursor and selected row metadata" in {
    val commands = List(
      Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile)),
      Command.typed("close", "Close current file", CommandIntent.File(FileIntent.CloseCurrentFile))
    )
    val registry = CommandRegistry(commands)
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("op")(using registry)
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(editing = EditingState(List(CursorPosition(1, 2))))
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.Surface(SurfaceId("command-runner"))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

    val overlays = OverlayViewModel.fromState(state, layout)
    val overlay  = overlays.belowCursor.getOrElse(fail("Expected command runner overlay"))

    // Content is painted entirely from `CommandRunnerSurfaceComposition` (issue #819, slice 2); header/row text and
    // selection are read from its paint boxes, not the plain-rows `header`/`rows` fields this content no longer
    // populates.
    val composition = overlay.composition.getOrElse(fail("Expected a command palette composition"))
    val header      = composition.paintBoxes.headOption.getOrElse(fail("Expected a header box"))
    header.text shouldBe Some("search: op")
    header.cursorOffset shouldBe Some("search: op".length)

    val itemBoxes = composition.paintBoxes.filter(_.focusId.isDefined)
    itemBoxes.exists(_.selected) shouldBe true
    itemBoxes.headOption.flatMap(_.text).exists(_.contains("Open file")) shouldBe true
  }

  // Renamed from "should skip inactive command palettes so closed overlays do not linger" (issue #819, slice 2):
  // `CommandPalette` is now unconditionally composed the same way `ModalWorkflow`/`ContextMenu`/`TabBar` already are
  // (`OverlayViewModel.isComposedContent`), so a *present* palette surface always yields an overlay view regardless of
  // `CommandRunner.isActive` -- matching every other composed surface kind, none of which gate on an inner "is this
  // really active" flag either. "Closed overlays do not linger" is still true in production: dismissing the command
  // runner removes its `UiSurface` from `state.runtime.uiSurfaces` entirely (`AppEventReducer`'s `ToggleCommandRunner`
  // handling), so an inactive-but-present palette surface, as constructed here, is a shape the reducers never actually
  // produce. What this test still pins down: even in that shape, an inactive runner's composition paints only its
  // (empty) header shell -- no selectable items or actions -- so nothing interactive leaks through.
  it should "paint only a header shell, no items, for a command palette whose runner is inactive" in {
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(editing = EditingState(List(CursorPosition(1, 2))))
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId)
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(CommandRunner.empty),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

    val overlays = OverlayViewModel.fromState(state, layout)
    val overlay  = overlays.belowCursor.getOrElse(fail("Expected a command palette overlay"))

    overlay.composition.toList.flatMap(_.paintBoxes).flatMap(_.focusId) shouldBe Nil
    overlay.composition.toList.flatMap(_.hitRegions) shouldBe Nil
  }

  it should "prefer the focused modal surface over earlier below-cursor floating surfaces" in {
    val commands = List(
      Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile))
    )
    val registry = CommandRegistry(commands)
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("op")(using registry)
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(editing = EditingState(List(CursorPosition(1, 2))))
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.Surface(SurfaceId("file-modal"))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          ),
          UiSurface(
            SurfaceId("file-modal"),
            SurfaceContent.ModalWorkflow(
              Modal.FileWorkflow(
                FileWorkflowState(
                  mode = FileWorkflowMode.Open,
                  filename = "notes.scala",
                  path = "/tmp/project"
                )
              )
            ),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

    val overlays = OverlayViewModel.fromState(state, layout)
    val overlay  = overlays.belowCursor.getOrElse(fail("Expected focused modal overlay"))

    // Content is painted entirely from `ModalSurfaceComposition` (issue #819); its presence for the `filename`/`path`
    // focus targets confirms the file-workflow surface -- not the command runner's search -- was picked.
    val focusIds = overlay.composition.toList.flatMap(_.paintBoxes).flatMap(_.focusId)
    focusIds should contain(SurfaceFocusId("filename"))
    focusIds should contain(SurfaceFocusId("path"))
  }

  it should "attach the shared close workflow composition to a modal overlay" in {
    val surfaceId = SurfaceId("close-modal")
    val workflow  = CloseWorkflowState(CloseScope.Current, BufferId(0), "notes.scala")
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        focus = Focus.Modal
      ),
      runtime = AppState.initial.runtime.copy(
        modalStack = List(ModalDialog(surfaceId, Modal.CloseWorkflow(workflow), ModalPlacement.Centered))
      )
    )
    val scene   = UiSceneSnapshot.from(state, ViewportSize(80, 24))
    val overlay = OverlayViewModel.fromState(state, scene).modal.lastOption.getOrElse(fail("Expected modal overlay"))

    overlay.composition.map(_.focusOrder) shouldBe Some(
      List(
        SurfaceFocusId("close-save"),
        SurfaceFocusId("close-discard"),
        SurfaceFocusId("close-cancel")
      )
    )
  }

  it should "stack the contextual toolbar above the command runner beneath the cursor" in {
    val commands = List(
      Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile))
    )
    val registry = CommandRegistry(commands)
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("op")(using registry)
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(editing = EditingState(List(CursorPosition(1, 2))))
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.Surface(SurfaceId("command-runner")),
        config = AppConfig.default.withUiElementGap(0.25)
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("contextual-toolbar"),
            SurfaceContent.ContextualToolbar(ContextualToolbarState()),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          ),
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

    val overlays = OverlayViewModel.fromState(state, layout)
    val stack    = overlays.belowCursorStack

    stack should have size 2
    // Content is painted entirely from `ContextualToolbarSurfaceComposition` (issue #819, slice 3); row segments are
    // read from its paint boxes, not the plain-rows `rows` field this content no longer populates.
    stack.head.composition.toList
      .flatMap(_.paintBoxes)
      .flatMap(_.segments)
      .exists(_.text.contains("Bold")) shouldBe true
    stack.head.itemGapRows shouldBe 0.25
    // `stack(1)` is the command palette, painted entirely from `CommandRunnerSurfaceComposition` (issue #819, slice
    // 2) -- its header text is read from the composition's paint boxes, not the plain-rows `header` field.
    stack(1).composition.toList.flatMap(_.paintBoxes).headOption.flatMap(_.text) shouldBe Some("search: op")
    stack.head.rect.y should be < stack(1).rect.y
  }

  it should "resolve a TabBar surface's composition through the same compositionFor dispatch as every other composed surface (issues #1075/#1076)" in {
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(editing = EditingState(List(CursorPosition(1, 2))))
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val entries = List(
      TabListEntry(BufferId(1), "one.txt", isDirty = false),
      TabListEntry(BufferId(2), "two.txt", isDirty = true)
    )
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.Surface(SurfaceId("tab-bar"))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("tab-bar"),
            SurfaceContent.TabBar(entries, activeBufferId = Some(BufferId(2))),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

    val overlay = OverlayViewModel.fromState(state, layout).belowCursor.getOrElse(fail("Expected a tab bar overlay"))

    overlay.composition shouldBe defined
    val composition = overlay.composition.get
    composition.paintBoxes.head.layout shouldBe SurfacePaintLayout.Distributed
    composition.hitRegions.map(_.focusId) shouldBe entries.map(e => TabBarSurfaceComposition.focusId(e.bufferId))
  }

  it should "paint the always-visible tab strip end to end once 2+ buffers are open, using the reserved layout row (issue #1074/#1075/#1076/#1077)" in {
    val firstBuffer  = Buffer.fromString(BufferId(0), "one")
    val secondBuffer = Buffer.fromString(BufferId(1), "two")
    val pane         = EditorPane.withBuffer(paneId, BufferId(0))
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(firstBuffer.id -> firstBuffer, secondBuffer.id -> secondBuffer),
        bufferOrder = List(firstBuffer.id, secondBuffer.id),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId)
      )
    )
    val viewportSize = ViewportSize(100, 24)
    val layout       = LayoutEngine.calculateLayout(state, viewportSize)

    val overlays = OverlayViewModel.fromState(state, layout)
    val tabBar   = overlays.tabBar.getOrElse(fail("Expected a tab bar overlay once 2+ buffers are open"))

    tabBar.rect shouldBe layout.tabBarRect.getOrElse(fail("expected a reserved tab bar rect"))
    tabBar.surfaceId shouldBe Some(UiSurface.TabBarSurfaceId)
    tabBar.composition shouldBe defined
    val composition = tabBar.composition.get
    composition.paintBoxes.head.layout shouldBe SurfacePaintLayout.Distributed
    composition.hitRegions.map(_.semanticLabel) shouldBe List(firstBuffer.id, secondBuffer.id).map(id =>
      TabListContent.build(state).entries.find(_.bufferId == id).map(_.title).getOrElse(fail("missing tab entry"))
    )
  }

  it should "carry no tab bar overlay with only a single buffer open" in {
    val overlays = OverlayViewModel.fromState(
      AppState.initial,
      LayoutEngine.calculateLayout(AppState.initial, ViewportSize(100, 24))
    )

    overlays.tabBar shouldBe None
  }

  it should "resolve a CommentLens surface's composition through the same compositionFor dispatch as every other composed surface (issue #819, slice 3)" in {
    val lens = CommentLensState(
      comment = RenderedComment(sourceLine = 4, raw = "Review this value", inlineMarkdown = "Review this value"),
      draft = "Review this value",
      cursor = "Review this value".length,
      target = None
    )
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(editing = EditingState(List(CursorPosition(1, 2))))
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.Surface(SurfaceId("comment-lens"))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("comment-lens"),
            SurfaceContent.CommentLens(lens),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.AboveCursor)
          )
        )
      )
    )
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

    val overlay = OverlayViewModel.fromState(state, layout).aboveCursor.getOrElse(fail("Expected a comment lens overlay"))

    // Content is painted entirely from `CommentLensSurfaceComposition` (issue #819, slice 3); draft text and cursor
    // are read from its paint boxes, not the plain-rows `rows`/`header` fields this content no longer populates.
    overlay.header shouldBe None
    overlay.rows shouldBe Nil
    val composition = overlay.composition.getOrElse(fail("Expected a comment lens composition"))
    composition.paintBoxes.map(_.text) shouldBe List(Some("comment"), Some("Review this value"))
    composition.paintBoxes.lastOption.flatMap(_.cursorOffset) shouldBe Some("Review this value".length)
  }
