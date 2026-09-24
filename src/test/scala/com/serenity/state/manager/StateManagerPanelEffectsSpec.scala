package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.{PanelKind, ViewIntent}
import com.serenity.io.FileManager
import com.serenity.keystroke.events.Event
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.undo.{HistoryEntry, UndoState}
import com.serenity.ui.layout.{PanelPosition, PanelTarget, PeekContent}
import com.serenity.ui.tui.MarkdownPreviewWindowAvailability
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
    val modelRef  = Ref.of[IO, Model](Model(initialState, UndoState(), Map.empty)).unsafeRunSync()
    val stateRef  = Model.appRef(modelRef)
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
        stateRef,
        NoOpLogger.impl[IO],
        new FileManager(),
        EffectLanePortFixtures.immediate(stateRef),
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
        calls.update(_ :+ "cancel-project-task")
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

    fixture.panels.interpret(ViewIntent.PinDiagnosticsPanel, AppState.initial).unsafeRunSync()

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

    fixture.panels.interpret(ViewIntent.PinDiagnosticsPanel, AppState.initial).unsafeRunSync()
    fixture.panels
      .interpret(
        ViewIntent.SetPanelPin(PanelKind.Diagnostics, Some(PanelPosition.Right)),
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

    fixture.panels.interpret(ViewIntent.PinDiagnosticsPanel, AppState.initial).unsafeRunSync()

    fixture.committedStates.get.unsafeRunSync() should have size 1
    fixture.modelRef.get.unsafeRunSync().undo.undoStack shouldBe Vector(
      HistoryEntry.PanelChange.capture(AppState.initial)
    )
  }

  it should "remove a pinned panel kind when its position is cleared" in {
    val fixture = harness()

    fixture.panels.interpret(ViewIntent.PinDiagnosticsPanel, AppState.initial).unsafeRunSync()
    fixture.panels
      .interpret(ViewIntent.SetPanelPin(PanelKind.Diagnostics, None), fixture.stateRef.get.unsafeRunSync())
      .unsafeRunSync()

    fixture.currentSurfaces.filter(_.content == SurfaceContent.Diagnostics(Nil)) shouldBe Nil
  }

  it should "stop a running project task when its output panel is closed" in {
    val id      = SurfaceId("terminal")
    val state   = pinnedState(id, SurfaceContent.Terminal("building", 0), PanelPosition.Bottom, 10)
    val fixture = harness(state)

    fixture.panels.interpret(ViewIntent.UnpinPanel(PanelPosition.Bottom), state).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(
      s"unpin:${PanelTarget.ByPosition(PanelPosition.Bottom)}",
      "cancel-project-task"
    )
  }

  it should "leave the project task alone when a non-terminal panel is closed" in {
    val id      = SurfaceId("diagnostics")
    val state   = pinnedState(id, SurfaceContent.Diagnostics(Nil), PanelPosition.Bottom, 10)
    val fixture = harness(state)

    fixture.panels.interpret(ViewIntent.UnpinPanel(PanelPosition.Bottom), state).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(s"unpin:${PanelTarget.ByPosition(PanelPosition.Bottom)}")
  }

  it should "route focus, expand, and collapse intents to their owning capabilities" in {
    val fixture = harness()

    fixture.panels.interpret(ViewIntent.FocusPanel(PanelPosition.Left), AppState.initial).unsafeRunSync()
    fixture.panels.interpret(ViewIntent.ExpandPanel(PanelPosition.Right), AppState.initial).unsafeRunSync()
    fixture.panels.interpret(ViewIntent.CollapseExpandedPanel, AppState.initial).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(
      s"switch:${PanelTarget.ByPosition(PanelPosition.Left)}",
      s"expand:${PanelTarget.ByPosition(PanelPosition.Right)}",
      "collapse"
    )
  }

  it should "raise the tab list as an event rather than mutating state directly" in {
    val fixture = harness()

    fixture.panels.interpret(ViewIntent.ToggleTabList, AppState.initial).unsafeRunSync()

    fixture.events.get.unsafeRunSync() shouldBe List(com.serenity.keystroke.events.ToggleTabList)
    fixture.currentSurfaces shouldBe AppState.initial.runtime.uiSurfaces
  }

  it should "report an unavailable markdown preview window instead of silently doing nothing in the TUI" in {
    val tuiState = AppState.initial.copy(runtime = AppState.initial.runtime.copy(isTuiMode = true))
    val fixture  = harness(tuiState)

    fixture.panels.interpret(ViewIntent.OpenMarkdownPreview, tuiState).unsafeRunSync()

    fixture.peeks.get.unsafeRunSync() shouldBe List(
      PeekContent.QuickInfo("Markdown preview window needs a graphical display.")
    )
  }

  it should "pin the comments panel when comment display is not turned off" in {
    val fixture = harness()

    fixture.panels.interpret(ViewIntent.PinCommentsPanel, AppState.initial).unsafeRunSync()

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

    fixture.panels.interpret(ViewIntent.PinCommentsPanel, offState).unsafeRunSync()

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
      .interpret(ViewIntent.SetPanelPin(PanelKind.Comments, Some(PanelPosition.Left)), offState)
      .unsafeRunSync()

    fixture.currentSurfaces.map(_.content) shouldBe List(SurfaceContent.Comments(Nil, None))
    fixture.peeks.get.unsafeRunSync() shouldBe List(
      PeekContent.QuickInfo("Comments are hidden -- comment display is turned off in Settings.")
    )
  }

  it should "pin the explorer root as an undo step and fill in its listing, selecting the first entry" in {
    val directory = Files.createTempDirectory("panel-effects-explorer")
    val child     = Files.createDirectory(directory.resolve("src"))
    try
      val fixture = harness()

      fixture.panels.pinExplorerPanelEffect(PanelPosition.Left, directory, 30).unsafeRunSync()

      val model = fixture.modelRef.get.unsafeRunSync()
      model.app.pinnedSurfaces.map(_.content) match
        case List(SurfaceContent.DirectoryTree(tree, selectedPath)) =>
          tree.rootPath shouldBe directory
          tree.entries.get(directory).map(_.map(_.path)) shouldBe Some(List(child))
          selectedPath shouldBe Some(child)
        case other =>
          fail(s"Expected a single pinned explorer, got $other")
      model.app.persisted.layout.workspaceTree.flatMap(
        _.positionForSurface(model.app.pinnedSurfaces.head.id)
      ) shouldBe Some(PanelPosition.Left)
      model.undo.undoStack shouldBe Vector(HistoryEntry.PanelChange.capture(AppState.initial))
      fixture.events.get.unsafeRunSync() shouldBe Nil
    finally
      Files.deleteIfExists(child)
      Files.deleteIfExists(directory)
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
