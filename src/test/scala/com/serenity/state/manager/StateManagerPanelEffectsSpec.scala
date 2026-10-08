package com.serenity.state.manager

import java.nio.file.Paths

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.ViewIntent
import com.serenity.frontend.{FrontendCapabilities, MarkdownPreviewWindowAvailability}
import com.serenity.keystroke.events.{Direction, Event}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.undo.{HistoryEntry, UndoState}
import com.serenity.ui.layout.{DirectoryTreeData, PanelPosition, PanelTarget, PeekContent}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** Exercises [[StateManagerPanelEffects]] on its own. Every surface-capability collaborator it delegates to is a plain
  * closure, so each [[ViewIntent]] can be asserted on as "which collaborator, with which arguments" rather than through
  * a fully composed `StateManager`.
  */
class StateManagerPanelEffectsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  final private class Harness(
      val modelRef: Ref[IO, Model],
      val stateRef: Ref[IO, AppState],
      val committedStates: Ref[IO, List[AppState]],
      val events: Ref[IO, List[Event]],
      val peeks: Ref[IO, List[PeekContent]],
      val calls: Ref[IO, List[String]],
      val panels: StateManagerPanelEffects
  ):
    def currentSurfaces: List[UiSurface] = stateRef.get.unsafeRunSync().runtime.uiSurfaces

  private def harness(
    initialState: AppState = AppState.initial,
    markdownPreviewWindow: MarkdownPreviewWindowAvailability = MarkdownPreviewWindowAvailability.Unavailable
  ): Harness =
    val modelRef  = Ref.of[IO, Model](Model(initialState, UndoState())).unsafeRunSync()
    val stateRef  = ModelViews.appRef(modelRef)
    val committed = Ref.of[IO, List[AppState]](Nil).unsafeRunSync()
    val events    = Ref.of[IO, List[Event]](Nil).unsafeRunSync()
    val peeks     = Ref.of[IO, List[PeekContent]](Nil).unsafeRunSync()
    val calls     = Ref.of[IO, List[String]](Nil).unsafeRunSync()

    new Harness(
      modelRef,
      stateRef,
      committed,
      events,
      peeks,
      calls,
      new StateManagerPanelEffects(
        stateRef.get,
        NoOpLogger.impl[IO],
        markdownPreviewWindow,
        transition =>
          modelRef.get.flatMap(model =>
            transition(model).fold(IO.unit)(next => committed.update(_ :+ next.app) >> modelRef.set(next))
          ),
        event => events.update(_ :+ event),
        (content, _) => peeks.update(_ :+ content),
        update =>
          stateRef.modify { state =>
            val config = update(state.persisted.config)
            (state.copy(persisted = state.persisted.copy(config = config)), config)
          },
        target => calls.update(_ :+ s"unpin:$target"),
        target => calls.update(_ :+ s"expand:$target"),
        () => calls.update(_ :+ "collapse"),
        target => calls.update(_ :+ s"switch:$target"),
        (target, size) => calls.update(_ :+ s"resize:$target:$size"),
        _ => IO.unit
      )
    )

  private def pinnedState(id: SurfaceId, content: SurfaceContent, position: PanelPosition, size: Int): AppState =
    com.serenity.DockedPanelFixtures.dock(AppState.initial, id, content, position, size)

  "StateManagerPanelEffects" should "resize a pinned panel by delta through the shared resize path" in {
    val id      = SurfaceId("explorer")
    val fixture = harness(pinnedState(id, SurfaceContent.Diagnostics(Nil), PanelPosition.Left, 30))

    fixture.panels.interpret(ViewIntent.SetPanelSize(id, 5), fixture.stateRef.get.unsafeRunSync()).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(s"resize:${PanelTarget.ById(id)}:35")
  }

  it should "clamp a shrinking panel at the minimum size rather than letting it collapse" in {
    val id      = SurfaceId("explorer")
    val fixture = harness(pinnedState(id, SurfaceContent.Diagnostics(Nil), PanelPosition.Left, 6))

    fixture.panels.interpret(ViewIntent.SetPanelSize(id, -20), fixture.stateRef.get.unsafeRunSync()).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(s"resize:${PanelTarget.ById(id)}:4")
  }

  it should "ignore a resize for a surface that is not pinned" in {
    val fixture = harness()

    fixture.panels.interpret(ViewIntent.SetPanelSize(SurfaceId("absent"), 5), AppState.initial).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe Nil
  }

  it should "pin the diagnostics panel at its default edge and size" in {
    val fixture = harness()

    fixture.panels.interpret(ViewIntent.TogglePanelShown(PanelId.Diagnostics), AppState.initial).unsafeRunSync()

    val currentState = fixture.stateRef.get.unsafeRunSync()
    val pinned       = fixture.currentSurfaces.filter(_.content == SurfaceContent.Diagnostics(Nil))
    pinned.map(_.presentation) shouldBe List(SurfacePresentation.Docked)
    pinned.map(surface =>
      currentState.persisted.layout.workspaceTree.flatMap(_.positionForSurface(surface.id))
    ) shouldBe
      List(Some(PanelPosition.Bottom))
    pinned.map(surface =>
      com.serenity.state.reducers.PanelStateReducer.currentSize(surface.id, currentState)
    ) shouldBe List(Some(10))
  }

  it should "replace rather than duplicate a panel kind that is already pinned" in {
    val fixture = harness()

    fixture.panels.interpret(ViewIntent.TogglePanelShown(PanelId.Diagnostics), AppState.initial).unsafeRunSync()
    fixture.panels
      .interpret(
        ViewIntent.SetPanelPin(PanelId.Diagnostics, Some(PanelPosition.Right)),
        fixture.stateRef.get.unsafeRunSync()
      )
      .unsafeRunSync()

    val currentState = fixture.stateRef.get.unsafeRunSync()
    val pinned       = fixture.currentSurfaces.filter(_.content == SurfaceContent.Diagnostics(Nil))
    pinned.map(_.presentation) shouldBe List(SurfacePresentation.Docked)
    pinned.map(surface =>
      currentState.persisted.layout.workspaceTree.flatMap(_.positionForSurface(surface.id))
    ) shouldBe
      List(Some(PanelPosition.Right))
    pinned.map(surface =>
      com.serenity.state.reducers.PanelStateReducer.currentSize(surface.id, currentState)
    ) shouldBe List(Some(30))
  }

  it should "declare a pin as an undo boundary in the same commit as the pinned panel" in {
    val fixture = harness()

    fixture.panels.interpret(ViewIntent.TogglePanelShown(PanelId.Diagnostics), AppState.initial).unsafeRunSync()

    fixture.committedStates.get.unsafeRunSync() should have size 1
    fixture.modelRef.get.unsafeRunSync().undo.undoStack shouldBe Vector(
      HistoryEntry.PanelChange.capture(AppState.initial)
    )
  }

  it should "remove a pinned panel kind when its position is cleared" in {
    val fixture = harness()

    fixture.panels.interpret(ViewIntent.TogglePanelShown(PanelId.Diagnostics), AppState.initial).unsafeRunSync()
    fixture.panels
      .interpret(ViewIntent.SetPanelPin(PanelId.Diagnostics, None), fixture.stateRef.get.unsafeRunSync())
      .unsafeRunSync()

    fixture.currentSurfaces.filter(_.content == SurfaceContent.Diagnostics(Nil)) shouldBe Nil
  }

  it should "keep a running project task going when its output panel is closed" in {
    val state =
      pinnedState(PanelId.ProjectOutput.surfaceId, SurfaceContent.Terminal("building", 0), PanelPosition.Bottom, 10)
    val fixture = harness(state)

    fixture.panels.interpret(ViewIntent.TogglePanelShown(PanelId.ProjectOutput), state).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(s"unpin:${PanelTarget.ById(PanelId.ProjectOutput.surfaceId)}")
    fixture.stateRef.get.unsafeRunSync().runtime.projectTasks shouldBe state.runtime.projectTasks
  }

  it should "show a hidden panel at its default edge when it is toggled" in {
    val fixture = harness()

    fixture.panels.interpret(ViewIntent.TogglePanelShown(PanelId.Outline), AppState.initial).unsafeRunSync()

    fixture.stateRef.get
      .unsafeRunSync()
      .persisted
      .layout
      .workspaceTree
      .flatMap(
        _.positionForSurface(PanelId.Outline.surfaceId)
      ) shouldBe Some(PanelRegistry.registrationFor(PanelId.Outline).defaultPosition)
  }

  it should "hide a shown panel, wherever it is docked, when it is toggled" in {
    val state   = pinnedState(PanelId.Outline.surfaceId, SurfaceContent.Outline(Nil), PanelPosition.Top, 10)
    val fixture = harness(state)

    fixture.panels.interpret(ViewIntent.TogglePanelShown(PanelId.Outline), state).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(s"unpin:${PanelTarget.ById(PanelId.Outline.surfaceId)}")
  }

  it should "focus a shown panel by its own id, whichever edge it is on" in {
    val state   = pinnedState(PanelId.Outline.surfaceId, SurfaceContent.Outline(Nil), PanelPosition.Top, 10)
    val fixture = harness(state)

    fixture.panels.interpret(ViewIntent.FocusPanel(PanelId.Outline), state).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(s"switch:${PanelTarget.ById(PanelId.Outline.surfaceId)}")
  }

  it should "move focus from the editor to the panel docked beside it" in {
    val state   = pinnedState(PanelId.Outline.surfaceId, SurfaceContent.Outline(Nil), PanelPosition.Left, 20)
    val fixture = harness(state)

    fixture.panels.interpret(ViewIntent.FocusInDirection(Direction.Left), state).unsafeRunSync()

    fixture.stateRef.get.unsafeRunSync().persisted.focus shouldBe Focus.Surface(PanelId.Outline.surfaceId)
  }

  private def rightEdge: AppState =
    com.serenity.DockedPanelFixtures.dockAllContent(
      AppState.initial,
      List(
        (PanelId.Outline.surfaceId, SurfaceContent.Outline(Nil), PanelPosition.Right, 20),
        (PanelId.Comments.surfaceId, SurfaceContent.Comments(Nil), PanelPosition.Right, 10)
      )
    )

  private def edgeOrder(state: AppState, position: PanelPosition): List[SurfaceId] =
    state.persisted.layout.workspaceTree.toList.flatMap(tree =>
      tree.dockedSurfaceIds.filter(tree.positionForSurface(_).contains(position))
    )

  it should "place a panel at a given index on its own edge, keeping each panel's size" in {
    val state   = rightEdge
    val fixture = harness(state)
    def size(id: PanelId, in: AppState) =
      com.serenity.state.reducers.PanelStateReducer.currentSize(id.surfaceId, in)

    fixture.panels
      .interpret(ViewIntent.PlacePanel(PanelId.Comments, Some(PanelPosition.Right), 0), state)
      .unsafeRunSync()

    val placed = fixture.stateRef.get.unsafeRunSync()
    edgeOrder(placed, PanelPosition.Right) shouldBe List(PanelId.Comments.surfaceId, PanelId.Outline.surfaceId)
    size(PanelId.Outline, placed) shouldBe size(PanelId.Outline, state)
    size(PanelId.Comments, placed) shouldBe size(PanelId.Comments, state)
  }

  it should "place a panel at the start of another edge" in {
    val state = com.serenity.DockedPanelFixtures.dock(
      rightEdge,
      PanelId.Diagnostics.surfaceId,
      SurfaceContent.Diagnostics(Nil),
      PanelPosition.Bottom,
      10
    )
    val fixture = harness(state)

    fixture.panels
      .interpret(ViewIntent.PlacePanel(PanelId.Comments, Some(PanelPosition.Bottom), 0), state)
      .unsafeRunSync()

    val placed = fixture.stateRef.get.unsafeRunSync()
    edgeOrder(placed, PanelPosition.Bottom) shouldBe List(PanelId.Comments.surfaceId, PanelId.Diagnostics.surfaceId)
    edgeOrder(placed, PanelPosition.Right) shouldBe List(PanelId.Outline.surfaceId)
  }

  it should "hide a panel placed nowhere" in {
    val state   = rightEdge
    val fixture = harness(state)

    fixture.panels.interpret(ViewIntent.PlacePanel(PanelId.Outline, None, 0), state).unsafeRunSync()

    edgeOrder(fixture.stateRef.get.unsafeRunSync(), PanelPosition.Right) shouldBe List(PanelId.Comments.surfaceId)
  }

  it should "show a hidden panel before focusing it" in {
    val fixture = harness()

    fixture.panels.interpret(ViewIntent.FocusPanel(PanelId.Outline), AppState.initial).unsafeRunSync()

    fixture.currentSurfaces.map(_.id) should contain(PanelId.Outline.surfaceId)
    fixture.calls.get.unsafeRunSync() shouldBe List(s"switch:${PanelTarget.ById(PanelId.Outline.surfaceId)}")
  }

  it should "maximise the focused panel, and restore it when one is maximised" in {
    val docked  = pinnedState(PanelId.Outline.surfaceId, SurfaceContent.Outline(Nil), PanelPosition.Right, 30)
    val focused = docked.copy(persisted = docked.persisted.copy(focus = Focus.Surface(PanelId.Outline.surfaceId)))
    val fixture = harness(focused)

    fixture.panels.interpret(ViewIntent.ToggleMaximisePanel, focused).unsafeRunSync()
    val maximised = com.serenity.DockedPanelFixtures.expand(focused, PanelId.Outline.surfaceId)
    fixture.panels.interpret(ViewIntent.ToggleMaximisePanel, maximised).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(
      s"expand:${PanelTarget.ById(PanelId.Outline.surfaceId)}",
      "collapse"
    )
  }

  it should "explain that maximising needs a focused panel when an editor has focus" in {
    val fixture = harness()

    fixture.panels.interpret(ViewIntent.ToggleMaximisePanel, AppState.initial).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe Nil
    fixture.peeks.get.unsafeRunSync() shouldBe List(PeekContent.QuickInfo("Focus a panel to maximise it."))
  }

  it should "hide panels whose family doesn't fit the mode being switched to" in {
    val withPanels =
      pinnedState(PanelId.Diagnostics.surfaceId, SurfaceContent.Diagnostics(Nil), PanelPosition.Bottom, 10)
    val state =
      com.serenity.DockedPanelFixtures.dock(
        withPanels,
        PanelId.Outline.surfaceId,
        SurfaceContent.Outline(Nil),
        PanelPosition.Right,
        30
      )
    val fixture = harness(state)

    fixture.panels.interpret(ViewIntent.SetAppMode(com.serenity.config.AppMode.Prose), state).unsafeRunSync()

    fixture.currentSurfaces.map(_.id) shouldBe List(PanelId.Outline.surfaceId)
  }

  it should "raise the tab list as an event rather than mutating state directly" in {
    val fixture = harness()

    fixture.panels.interpret(ViewIntent.ToggleTabList, AppState.initial).unsafeRunSync()

    fixture.events.get.unsafeRunSync() shouldBe List(com.serenity.keystroke.events.ToggleTabList)
    fixture.currentSurfaces shouldBe AppState.initial.runtime.uiSurfaces
  }

  it should "report an unavailable markdown preview window instead of silently doing nothing in the TUI" in {
    val tuiState =
      AppState.initial.copy(runtime = AppState.initial.runtime.copy(capabilities = FrontendCapabilities.tui()))
    val fixture = harness(tuiState)

    fixture.panels.interpret(ViewIntent.OpenMarkdownPreview, tuiState).unsafeRunSync()

    fixture.peeks.get.unsafeRunSync() shouldBe List(
      PeekContent.QuickInfo("Markdown preview window needs a graphical display.")
    )
  }

  it should "pin the comments panel when comment display is not turned off" in {
    val fixture = harness()

    fixture.panels.interpret(ViewIntent.TogglePanelShown(PanelId.Comments), AppState.initial).unsafeRunSync()

    fixture.currentSurfaces.map(_.content) shouldBe List(SurfaceContent.Comments(Nil, None))
    fixture.peeks.get.unsafeRunSync() shouldBe Nil
  }

  it should "report that comments are hidden instead of pinning a panel when comment display is off" in {
    val offState = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(config =
        AppState.initial.persisted.config.withCommentDisplayMode(com.serenity.config.CommentDisplayMode.Off)
      )
    )
    val fixture = harness(offState)

    fixture.panels.interpret(ViewIntent.TogglePanelShown(PanelId.Comments), offState).unsafeRunSync()

    fixture.currentSurfaces shouldBe Nil
    fixture.peeks.get.unsafeRunSync() shouldBe List(
      PeekContent.QuickInfo("Comments are hidden -- comment display is turned off in Settings.")
    )
  }

  it should "report that comments are hidden instead of replacing an already-pinned panel when comment display is off" in {
    val alreadyPinned =
      pinnedState(SurfaceId("comments"), SurfaceContent.Comments(Nil, None), PanelPosition.Right, 30)
    val offState = alreadyPinned.copy(persisted =
      alreadyPinned.persisted.copy(config =
        alreadyPinned.persisted.config.withCommentDisplayMode(com.serenity.config.CommentDisplayMode.Off)
      )
    )
    val fixture = harness(offState)

    fixture.panels
      .interpret(ViewIntent.SetPanelPin(PanelId.Comments, Some(PanelPosition.Left)), offState)
      .unsafeRunSync()

    fixture.currentSurfaces.map(_.content) shouldBe List(SurfaceContent.Comments(Nil, None))
    fixture.peeks.get.unsafeRunSync() shouldBe List(
      PeekContent.QuickInfo("Comments are hidden -- comment display is turned off in Settings.")
    )
  }

  it should "pin the explorer root as an undo step, leaving its listing to the commit boundary" in {
    val directory = Paths.get("/repo")
    val fixture   = harness()

    fixture.panels.pinExplorerPanelEffect(PanelPosition.Left, directory, 30).unsafeRunSync()

    val model = fixture.modelRef.get.unsafeRunSync()
    model.app.pinnedSurfaces.map(_.content) shouldBe List(SurfaceContent.DirectoryTree(DirectoryTreeData(directory)))
    model.app.persisted.layout.workspaceTree.flatMap(
      _.positionForSurface(model.app.pinnedSurfaces.head.id)
    ) shouldBe Some(PanelPosition.Left)
    model.undo.undoStack shouldBe Vector(HistoryEntry.PanelChange.capture(AppState.initial))
    fixture.events.get.unsafeRunSync() shouldBe Nil
  }

  it should "persist the markdown view mode and unpin the preview panel when returning to source" in {
    val fixture = harness(
      pinnedState(
        SurfaceId("preview"),
        SurfaceContent.MarkdownPreview(BufferId(1), "notes.md"),
        PanelPosition.Right,
        40
      )
    )

    fixture.panels
      .interpret(
        ViewIntent.SetMarkdownViewMode(com.serenity.config.MarkdownViewMode.Source),
        fixture.stateRef.get.unsafeRunSync()
      )
      .unsafeRunSync()

    val after = fixture.stateRef.get.unsafeRunSync()
    after.persisted.config.markdownViewMode shouldBe com.serenity.config.MarkdownViewMode.Source
    after.runtime.uiSurfaces shouldBe Nil
  }

  it should "apply a general appearance setting through the shared config update path" in {
    val fixture = harness()

    val target = com.serenity.config.AppMode.values
      .find(_ != AppState.initial.persisted.config.appMode)
      .getOrElse(fail("Expected more than one app mode"))

    fixture.panels.interpret(ViewIntent.SetAppMode(target), AppState.initial).unsafeRunSync()

    fixture.stateRef.get.unsafeRunSync().persisted.config.appMode shouldBe target
  }

  it should "open the current chapter's note in a pane beside the manuscript" in {
    val initial = AppState.initial
    val buffer  = initial.persisted.buffers(BufferId(0))
    val manuscript = buffer.copy(
      document = buffer.document.copy(
        content = com.serenity.rope.Rope("# Chapter 1: Storm\nthe sea"),
        language = Some(com.serenity.lsp.config.LanguageId.Markdown)
      ),
      editing = EditingState(List(CursorPosition(1, 0)))
    )
    val state   = initial.copy(persisted = initial.persisted.copy(buffers = Map(BufferId(0) -> manuscript)))
    val fixture = harness(state)

    fixture.panels.interpret(ViewIntent.OpenChapterNote, state).unsafeRunSync()

    val after = fixture.stateRef.get.unsafeRunSync()
    after.persisted.layout.editorPanes should have size 2
    after.persisted.buffers.values.count(_.hidden) shouldBe 1
    after.persisted.bufferOrder shouldBe List(BufferId(0))
  }
