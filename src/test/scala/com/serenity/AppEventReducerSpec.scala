package com.serenity

import com.serenity.command.{CommandRegistry, CommandSurfaceItem}
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
    result.effects shouldBe List(AppEffect.CompleteQuit())
  }

  it should "toggle the command runner and restore focus via history" in {
    val initialState = AppState.initial.copy(focus = Focus.EditorPane(PaneId(0)))

    val opened = AppEventReducer.reduce(ToggleCommandRunner, initialState, registry)

    opened.state.commandRunnerSurface shouldBe defined
    opened.state.focus shouldBe Focus.Surface(opened.state.commandRunnerSurface.get.id)
    opened.state.focusHistory should contain(Focus.EditorPane(PaneId(0)))

    val closed = AppEventReducer.reduce(ToggleCommandRunner, opened.state, registry)

    closed.state.focus shouldBe Focus.EditorPane(PaneId(0))
    closed.state.commandRunnerSurface shouldBe None
    closed.effects shouldBe Nil
  }

  it should "not open the contextual toolbar when the setting is disabled" in {
    val initialState = AppState.initial.copy(
      config = AppState.initial.config.withContextualToolbarEnabled(false),
      focus = Focus.EditorPane(PaneId(0))
    )

    val result = AppEventReducer.reduce(ToggleContextualToolbar, initialState, registry)

    result.state.contextualToolbarSurface shouldBe None
    result.state.focus shouldBe Focus.EditorPane(PaneId(0))
    result.effects shouldBe Nil
  }

  it should "open the contextual toolbar above the cursor and focus it" in {
    val initialState = AppState.initial.copy(focus = Focus.EditorPane(PaneId(0)))

    val result  = AppEventReducer.reduce(ToggleContextualToolbar, initialState, registry)
    val surface = result.state.contextualToolbarSurface.getOrElse(fail("Expected contextual toolbar"))

    surface.presentation shouldBe SurfacePresentation.Floating(
      initialState.activeCursorPosition,
      SurfacePlacement.AboveCursor
    )
    result.state.focus shouldBe Focus.Surface(surface.id)
    result.effects shouldBe Nil
  }

  it should "restore editor focus when runner replaces a modal that held focus" in {
    val base = AppState.initial.copy(focus = Focus.EditorPane(PaneId(0)))

    val withModal = ModalStateReducer
      .show(
        Modal.FileWorkflow(FileWorkflowState(FileWorkflowMode.Open, "", "")),
        base
      )
      .state
    val modalSurfaceId = withModal.modalSurface.get.id
    withModal.focus shouldBe Focus.Surface(modalSurfaceId)

    val withRunner = AppEventReducer.reduce(ToggleCommandRunner, withModal, registry)
    withRunner.state.modalSurface shouldBe None
    withRunner.state.commandRunnerSurface shouldBe defined

    val closed = AppEventReducer.reduce(ToggleCommandRunner, withRunner.state, registry)

    closed.state.focus shouldBe Focus.EditorPane(PaneId(0))
    closed.state.commandRunnerSurface shouldBe None
    closed.state.modalSurface shouldBe None
  }

  it should "not accumulate duplicate entries in focusHistory" in {
    val base = AppState.initial

    val opened1 = AppEventReducer.reduce(ToggleCommandRunner, base, registry)
    val closed1 = AppEventReducer.reduce(ToggleCommandRunner, opened1.state, registry)
    val opened2 = AppEventReducer.reduce(ToggleCommandRunner, closed1.state, registry)

    val editorEntries = opened2.state.focusHistory.count(_ == Focus.EditorPane(PaneId(0)))
    editorEntries shouldBe 1
  }

  it should "hydrate workspace panel pin options from current pinned surfaces" in {
    val base = AppState.initial.copy(
      focus = Focus.EditorPane(PaneId(0)),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("outline-panel"),
          SurfaceContent.Outline(Nil),
          SurfacePresentation.Pinned(PanelPosition.Right, 30)
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

    state.buffers should have size 2
    state.bufferOrder shouldBe List(BufferId(0), BufferId(1))
    state.focusedBufferId shouldBe Some(BufferId(1))
    state.buffers(BufferId(1)).isNewEmpty shouldBe true
    result.effects shouldBe Nil
  }

  it should "navigate to the next and previous buffer according to buffer order" in {
    val stateWithBuffers = AppEventReducer
      .reduce(NewTab, AppState.initial, registry)
      .state
      .copy(viewportSize = Some(ViewportSize(200, 24)))

    val movedBack = AppEventReducer.reduce(PreviousTab, stateWithBuffers, registry).state
    movedBack.focusedBufferId shouldBe Some(BufferId(0))

    val movedForward = AppEventReducer.reduce(NextTab, movedBack, registry).state
    movedForward.focusedBufferId shouldBe Some(BufferId(1))
  }
