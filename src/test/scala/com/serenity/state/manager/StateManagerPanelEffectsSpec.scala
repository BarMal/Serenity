package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.{PanelKind, ViewIntent}
import com.serenity.io.FileManager
import com.serenity.keystroke.events.{Event, ExplorerEvent}
import com.serenity.rope.Balance
import com.serenity.state.models.*
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
    val stateRef  = Ref.of[IO, AppState](initialState).unsafeRunSync()
    val committed = Ref.of[IO, List[AppState]](Nil).unsafeRunSync()
    val events    = Ref.of[IO, List[Event]](Nil).unsafeRunSync()
    val peeks     = Ref.of[IO, List[PeekContent]](Nil).unsafeRunSync()
    val calls     = Ref.of[IO, List[String]](Nil).unsafeRunSync()

    new Harness(
      stateRef,
      committed,
      events,
      peeks,
      calls,
      new StateManagerPanelEffects(
        stateRef,
        NoOpLogger.impl[IO],
        new FileManager(),
        markdownPreviewWindow,
        (newState, _) => committed.update(_ :+ newState) >> stateRef.set(newState),
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
        calls.update(_ :+ "cancel-project-task"),
        (_, groupable) => calls.update(_ :+ s"record-undo:$groupable")
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

  it should "load the explorer root directory and announce it as an explorer event" in {
    val directory = Files.createTempDirectory("panel-effects-explorer")
    val child     = Files.createDirectory(directory.resolve("src"))
    try
      val fixture = harness()

      fixture.panels.pinExplorerPanelEffect(PanelPosition.Left, directory, 30).unsafeRunSync()

      fixture.events.get.unsafeRunSync() match
        case List(ExplorerEvent.RootDirectoryLoaded(position, rootPath, size, entries, selectedPath)) =>
          position shouldBe PanelPosition.Left
          rootPath shouldBe directory
          size shouldBe 30
          entries.map(_.path) shouldBe List(child)
          selectedPath shouldBe Some(child)
        case other =>
          fail(s"Expected a single RootDirectoryLoaded event, got $other")
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
