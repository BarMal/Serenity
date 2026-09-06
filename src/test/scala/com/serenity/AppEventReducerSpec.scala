package com.serenity

import com.serenity.command.{CommandRegistry, CommandSurfaceItem}
import com.serenity.keystroke.Modifier
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, AppEventReducer, ModalStateReducer}
import com.serenity.ui.layout.{PanelPosition, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AppEventReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val registry = CommandRegistry.withToggleUI

  private def descendants(group: CommandSurfaceItem.GroupItem): List[CommandSurfaceItem] =
    group.children.flatMap {
      case child: CommandSurfaceItem.GroupItem => child :: descendants(child)
      case child                               => List(child)
    }

  "AppEventReducer" should "emit a quit effect without mutating state" in {
    val initialState = AppState.initial

    val result = AppEventReducer.reduce(Quit, initialState, registry)

    result.state shouldBe initialState
    result.effects shouldBe List(AppEffect.CompleteQuit)
  }

  it should "toggle the command runner and restore focus via history" in {
    val initialState =
      AppState.initial.copy(persisted = AppState.initial.persisted.copy(focus = Focus.EditorPane(PaneId(0))))

    val opened = AppEventReducer.reduce(ToggleCommandRunner, initialState, registry)

    opened.state.commandRunnerSurface shouldBe defined
    opened.state.persisted.focus shouldBe Focus.Surface(opened.state.commandRunnerSurface.get.id)
    opened.state.runtime.focusHistory should contain(Focus.EditorPane(PaneId(0)))

    val closed = AppEventReducer.reduce(ToggleCommandRunner, opened.state, registry)

    closed.state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    closed.state.commandRunnerSurface shouldBe None
    closed.effects shouldBe Nil
  }

  it should "not open the contextual toolbar when the setting is disabled" in {
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppState.initial.persisted.config.withContextualToolbarEnabled(false),
        focus = Focus.EditorPane(PaneId(0))
      )
    )

    val result = AppEventReducer.reduce(ToggleContextualToolbar, initialState, registry)

    result.state.contextualToolbarSurface shouldBe None
    result.state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    result.effects shouldBe Nil
  }

  it should "open the contextual toolbar below the cursor without stealing editor focus" in {
    val initialState =
      AppState.initial.copy(persisted = AppState.initial.persisted.copy(focus = Focus.EditorPane(PaneId(0))))

    val result  = AppEventReducer.reduce(ToggleContextualToolbar, initialState, registry)
    val surface = result.state.contextualToolbarSurface.getOrElse(fail("Expected contextual toolbar"))

    surface.presentation shouldBe SurfacePresentation.Floating(
      initialState.activeCursorPosition,
      SurfacePlacement.BelowCursor
    )
    result.state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    result.effects shouldBe Nil
  }

  it should "restore editor focus when runner replaces a modal that held focus" in {
    val base = AppState.initial.copy(persisted = AppState.initial.persisted.copy(focus = Focus.EditorPane(PaneId(0))))

    val withModal = ModalStateReducer
      .show(
        Modal.FileWorkflow(FileWorkflowState(FileWorkflowMode.Open, "", "")),
        base
      )
      .state
    val modalSurfaceId = withModal.modalSurface.get.id
    withModal.persisted.focus shouldBe Focus.Surface(modalSurfaceId)

    val withRunner = AppEventReducer.reduce(ToggleCommandRunner, withModal, registry)
    withRunner.state.modalSurface shouldBe None
    withRunner.state.commandRunnerSurface shouldBe defined

    val closed = AppEventReducer.reduce(ToggleCommandRunner, withRunner.state, registry)

    closed.state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    closed.state.commandRunnerSurface shouldBe None
    closed.state.modalSurface shouldBe None
  }

  it should "not accumulate duplicate entries in focusHistory" in {
    val base = AppState.initial

    val opened1 = AppEventReducer.reduce(ToggleCommandRunner, base, registry)
    val closed1 = AppEventReducer.reduce(ToggleCommandRunner, opened1.state, registry)
    val opened2 = AppEventReducer.reduce(ToggleCommandRunner, closed1.state, registry)

    val editorEntries = opened2.state.runtime.focusHistory.count(_ == Focus.EditorPane(PaneId(0)))
    editorEntries shouldBe 1
  }

  it should "hydrate workspace panel pin options from current pinned surfaces" in {
    val base = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.EditorPane(PaneId(0))),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("outline-panel"),
            SurfaceContent.Outline(Nil),
            SurfacePresentation.Pinned(PanelPosition.Right, 30)
          )
        )
      )
    )

    val opened = AppEventReducer.reduce(ToggleCommandRunner, base, registry).state
    val runner = opened.commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(runner) => Some(runner)
          case _                                     => None
      }
      .getOrElse(fail("Expected command runner"))

    val workspace = runner.settingsGroups
      .find(_.id == "settings-workspace-layout")
      .getOrElse(fail("Expected workspace layout group"))
    val outlineOption = descendants(workspace)
      .collectFirst { case option: CommandSurfaceItem.OptionItem if option.id == "panel-outline-pin" => option }
      .getOrElse(fail("Expected outline pin option"))

    outlineOption.selectedOption shouldBe "Right"
  }

  it should "create a new buffer and focus it on new tab" in {
    val initialState = AppState.initial

    val result = AppEventReducer.reduce(NewTab, initialState, registry)
    val state  = result.state

    state.persisted.buffers should have size 2
    state.persisted.bufferOrder shouldBe List(BufferId(0), BufferId(1))
    state.focusedBufferId shouldBe Some(BufferId(1))
    state.persisted.buffers(BufferId(1)).document.isNewEmpty shouldBe true
    result.effects shouldBe Nil
  }

  it should "navigate to the next and previous buffer according to buffer order" in {
    val newTabState = AppEventReducer.reduce(NewTab, AppState.initial, registry).state
    val stateWithBuffers =
      newTabState.copy(runtime = newTabState.runtime.copy(viewportSize = Some(ViewportSize(200, 24))))

    val movedBack = AppEventReducer.reduce(PreviousTab, stateWithBuffers, registry).state
    movedBack.focusedBufferId shouldBe Some(BufferId(0))

    val movedForward = AppEventReducer.reduce(NextTab, movedBack, registry).state
    movedForward.focusedBufferId shouldBe Some(BufferId(1))
  }

  private def enabledState: AppState =
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppState.initial.persisted.config.withCommandRunnerCursorPeekEnabled(true),
        focus = Focus.EditorPane(PaneId(0))
      )
    )

  "cursor-peek prototype (default off)" should "leave state entirely untouched when the flag is disabled" in {
    val disabledState =
      AppState.initial.copy(persisted = AppState.initial.persisted.copy(focus = Focus.EditorPane(PaneId(0))))

    val result = AppEventReducer.reduce(CursorPeekModifierPressed(Modifier.Meta, 0L), disabledState, registry)

    result.state shouldBe disabledState
    result.effects shouldBe Nil
  }

  it should "begin a peek (freezing the cursor anchor and showing a peek surface) on the first bare press when enabled" in {
    val state = enabledState

    val result = AppEventReducer.reduce(CursorPeekModifierPressed(Modifier.Meta, 0L), state, registry)

    result.state.runtime.cursorPeekAnchor shouldBe state.activeCursorPosition
    result.state.commandRunnerSurface shouldBe None
    val peekSurface = result.state.runtime.uiSurfaces.find(_.id == SurfaceId.CursorPeek)
    peekSurface shouldBe defined
    peekSurface.map(_.content) should matchPattern { case Some(_: SurfaceContent.CommandRunnerPeek) => }
  }

  it should "not steal editor focus while a peek is showing" in {
    val state = enabledState

    val result = AppEventReducer.reduce(CursorPeekModifierPressed(Modifier.Meta, 0L), state, registry)

    result.state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "clear the peek anchor and remove the peek surface when the modifier is released before a second tap" in {
    val state = enabledState

    val pressed  = AppEventReducer.reduce(CursorPeekModifierPressed(Modifier.Meta, 0L), state, registry).state
    val released = AppEventReducer.reduce(CursorPeekModifierReleased(Modifier.Meta, 5L), pressed, registry).state

    released.runtime.cursorPeekAnchor shouldBe None
    released.runtime.cursorPeekResolvedAnchor shouldBe None
    released.runtime.uiSurfaces.find(_.id == SurfaceId.CursorPeek) shouldBe None
    released.commandRunnerSurface shouldBe None
  }

  it should "open the command runner fully -- reusing today's open mechanics -- on a double-tap within the window" in {
    val state = enabledState

    val pressed1 = AppEventReducer.reduce(CursorPeekModifierPressed(Modifier.Meta, 0L), state, registry).state
    val released = AppEventReducer.reduce(CursorPeekModifierReleased(Modifier.Meta, 5L), pressed1, registry).state
    val opened   = AppEventReducer.reduce(CursorPeekModifierPressed(Modifier.Meta, 200L), released, registry).state

    opened.commandRunnerSurface shouldBe defined
    opened.persisted.focus shouldBe Focus.Surface(opened.commandRunnerSurface.get.id)
    opened.runtime.cursorPeekAnchor shouldBe None
    opened.runtime.uiSurfaces.find(_.id == SurfaceId.CursorPeek) shouldBe None
  }

  it should "ignore bare presses of a modifier other than the configured one" in {
    val state = enabledState

    val result = AppEventReducer.reduce(CursorPeekModifierPressed(Modifier.Ctrl, 0L), state, registry)

    result.state.runtime.cursorPeekAnchor shouldBe None
    result.state.commandRunnerSurface shouldBe None
  }

  it should "cancel a pending peek when a non-modifier key is pressed" in {
    val state = enabledState

    val pressed   = AppEventReducer.reduce(CursorPeekModifierPressed(Modifier.Meta, 0L), state, registry).state
    val cancelled = AppEventReducer.reduce(CursorPeekOtherKeyPressed, pressed, registry).state

    cancelled.runtime.cursorPeekAnchor shouldBe None
    cancelled.commandRunnerSurface shouldBe None
  }

  it should "not open the command runner from a modifier release event" in {
    val state = enabledState

    val result = AppEventReducer.reduce(CursorPeekModifierReleased(Modifier.Meta, 0L), state, registry)

    result.state.commandRunnerSurface shouldBe None
  }

  it should "open the shortcuts-help reference below the cursor without stealing editor focus" in {
    val initialState =
      AppState.initial.copy(persisted = AppState.initial.persisted.copy(focus = Focus.EditorPane(PaneId(0))))

    val result  = AppEventReducer.reduce(ToggleShortcutsHelp, initialState, registry)
    val surface = result.state.shortcutsHelpSurface.getOrElse(fail("Expected a shortcuts-help surface"))

    surface.id shouldBe SurfaceId.ShortcutsHelp
    surface.presentation shouldBe SurfacePresentation.Floating(
      initialState.activeCursorPosition,
      SurfacePlacement.BelowCursor
    )
    result.state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    result.effects shouldBe Nil
  }

  it should "close the shortcuts-help reference on a second toggle" in {
    val initialState =
      AppState.initial.copy(persisted = AppState.initial.persisted.copy(focus = Focus.EditorPane(PaneId(0))))

    val opened = AppEventReducer.reduce(ToggleShortcutsHelp, initialState, registry).state
    opened.shortcutsHelpSurface shouldBe defined

    val closed = AppEventReducer.reduce(ToggleShortcutsHelp, opened, registry).state

    closed.shortcutsHelpSurface shouldBe None
    closed.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "populate the shortcuts-help reference from the app's real configured bindings" in {
    val initialState = AppState.initial

    val result  = AppEventReducer.reduce(ToggleShortcutsHelp, initialState, registry)
    val surface = result.state.shortcutsHelpSurface.getOrElse(fail("Expected a shortcuts-help surface"))

    surface.content shouldBe SurfaceContent.ShortcutsHelp(
      com.serenity.state.models.ShortcutsHelpContent.build(initialState.persisted.config)
    )
  }

  it should "not open the shortcuts-help reference while the start page is showing" in {
    val base                  = AppState.initial
    val (withId, startPageId) = base.allocateSurfaceId
    val startPageSurface = UiSurface(
      id = startPageId,
      content = SurfaceContent.StartPage(StartupPage("Serenity")),
      presentation = SurfacePresentation.Modal
    )
    val initialState = withId.copy(runtime = withId.runtime.copy(uiSurfaces = List(startPageSurface)))

    val result = AppEventReducer.reduce(ToggleShortcutsHelp, initialState, registry)

    result.state.shortcutsHelpSurface shouldBe None
  }

  private def paletteRegistration(id: String): PanelRegistration =
    PanelRegistration(
      id = PanelId(id),
      label = id,
      description = s"$id panel",
      buildContent = _ => SurfaceContent.QuickInfo(id),
      supportedModes = Set(PanelDisplayMode.Palette)
    )

  it should "open a registered panel below the cursor without stealing editor focus" in {
    val initialState =
      AppState.initial.copy(persisted = AppState.initial.persisted.copy(focus = Focus.EditorPane(PaneId(0))))
    val panelRegistry = PanelRegistry(List(paletteRegistration("outline")))

    val result = AppEventReducer.reduce(TogglePanel(PanelId("outline")), initialState, registry, panelRegistry)

    val surface = result.state.surfaceById(SurfaceId("panel-outline")).getOrElse(fail("Expected a panel surface"))
    surface.content shouldBe SurfaceContent.QuickInfo("outline")
    surface.presentation shouldBe SurfacePresentation.Floating(
      initialState.activeCursorPosition,
      SurfacePlacement.BelowCursor
    )
    result.state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "close a registered panel on a second toggle" in {
    val initialState  = AppState.initial
    val panelRegistry = PanelRegistry(List(paletteRegistration("outline")))

    val opened = AppEventReducer.reduce(TogglePanel(PanelId("outline")), initialState, registry, panelRegistry).state
    opened.surfaceById(SurfaceId("panel-outline")) shouldBe defined

    val closed = AppEventReducer.reduce(TogglePanel(PanelId("outline")), opened, registry, panelRegistry).state

    closed.surfaceById(SurfaceId("panel-outline")) shouldBe None
  }

  it should "no-op for a panel id with no registration" in {
    val initialState = AppState.initial

    val result = AppEventReducer.reduce(TogglePanel(PanelId("missing")), initialState, registry)

    result.state shouldBe initialState
  }

  it should "no-op for a registration that does not declare palette support" in {
    val initialState = AppState.initial
    val panelRegistry = PanelRegistry(
      List(
        PanelRegistration(
          id = PanelId("corner-only"),
          label = "corner-only",
          description = "corner-only panel",
          buildContent = _ => SurfaceContent.QuickInfo("corner-only"),
          supportedModes = Set(PanelDisplayMode.Corner)
        )
      )
    )

    val result = AppEventReducer.reduce(TogglePanel(PanelId("corner-only")), initialState, registry, panelRegistry)

    result.state shouldBe initialState
  }

  it should "not open a registered panel while the start page is showing" in {
    val base                  = AppState.initial
    val (withId, startPageId) = base.allocateSurfaceId
    val startPageSurface = UiSurface(
      id = startPageId,
      content = SurfaceContent.StartPage(StartupPage("Serenity")),
      presentation = SurfacePresentation.Modal
    )
    val initialState  = withId.copy(runtime = withId.runtime.copy(uiSurfaces = List(startPageSurface)))
    val panelRegistry = PanelRegistry(List(paletteRegistration("outline")))

    val result = AppEventReducer.reduce(TogglePanel(PanelId("outline")), initialState, registry, panelRegistry)

    result.state.surfaceById(SurfaceId("panel-outline")) shouldBe None
  }
