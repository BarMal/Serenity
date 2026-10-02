package com.serenity

import com.serenity.config.{AppConfig, AppMode, PanelEscapeTarget}
import com.serenity.keystroke.events.{Direction, PanelInputEvent}
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, PinnedPanelComponent}
import com.serenity.state.models.*
import com.serenity.state.reducers.{DirectionalFocus, PanelStateReducer}
import com.serenity.ui.layout.{Layout, PanelPosition, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Escape in a focused panel goes back to the editor's active pane, or -- with `previous` for the current app mode --
  * to whatever had focus before the panel. The other keys that leave a panel (typing, Delete, Tab) always go to the
  * editor, since what they mean is "I want to type there".
  */
class PanelEscapeFocusSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val firstPane  = PaneId(0)
  private val secondPane = PaneId(1)
  private val panelA     = SurfaceId("panel-a")
  private val panelB     = SurfaceId("panel-b")

  private def baseState: AppState =
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, "hello")
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(
            firstPane  -> EditorPane.withBuffer(firstPane, bufferId),
            secondPane -> EditorPane.withBuffer(secondPane, bufferId)
          ),
          activeEditorPaneId = Some(secondPane),
          workspaceTree = Some(TestWorkspaceTrees.linear(firstPane, secondPane))
        ),
        focus = Focus.EditorPane(secondPane)
      ),
      runtime = AppState.initial.runtime.copy(
        viewportSize = Some(ViewportSize(120, 40)),
        nextBufferId = BufferId(2),
        nextPaneId = PaneId(2)
      )
    )

  private def withPanels: AppState =
    val withA = DockedPanelFixtures.dock(baseState, panelA, SurfaceContent.Outline(Nil), PanelPosition.Left, 20)
    DockedPanelFixtures.dock(withA, panelB, SurfaceContent.Diagnostics(Nil), PanelPosition.Bottom, 8)

  private def configured(state: AppState, update: AppConfig => AppConfig): AppState =
    state.copy(persisted = state.persisted.copy(config = update(state.persisted.config)))

  private def escapingToPrevious(state: AppState, mode: AppMode = AppMode.Code): AppState =
    configured(state, _.withAppMode(mode).withPanelEscapeTarget(mode, PanelEscapeTarget.Previous))

  private def focusPanel(state: AppState, surfaceId: SurfaceId): AppState =
    PanelStateReducer.focus(surfaceId, state).state

  /** Moves focus from `state`'s focused panel onto `target` with the directional focus keys, whichever way it lies. */
  private def moveFocusOnto(state: AppState, target: SurfaceId): AppState =
    Direction.values
      .map(DirectionalFocus.moved(state, _))
      .find(_.persisted.focus == Focus.Surface(target))
      .getOrElse(fail(s"no direction moves focus onto $target"))

  private def press(event: PanelInputEvent, state: AppState): AppState =
    val position = state.persisted.focus match
      case Focus.Surface(surfaceId) => state.persisted.layout.workspaceTree.flatMap(_.positionForSurface(surfaceId))
      case _                        => None
    val component = PinnedPanelComponent(position.getOrElse(fail("focus is not on a docked panel")))
    component.processEvent(event, state) match
      case ComponentResult.FocusTransfer(focus) => state.copy(persisted = state.persisted.copy(focus = focus))
      case ComponentResult.StateChange(update)  => update(state)
      case other                                => fail(s"Expected focus to move, got $other")

  private def valid(state: AppState): AppState =
    AppStateValidation.validationErrors(state) shouldBe Nil
    state

  "Escape from a focused panel" should "return to the editor's active pane by default" in {
    val inB = moveFocusOnto(focusPanel(withPanels, panelA), panelB)

    press(PanelInputEvent.Dismiss, inB).persisted.focus shouldBe Focus.EditorPane(secondPane)
  }

  it should "return to the panel focus came from when set to previous" in {
    val inB = moveFocusOnto(focusPanel(escapingToPrevious(withPanels), panelA), panelB)

    val backInA = valid(press(PanelInputEvent.Dismiss, inB))
    backInA.persisted.focus shouldBe Focus.Surface(panelA)

    valid(press(PanelInputEvent.Dismiss, backInA)).persisted.focus shouldBe Focus.EditorPane(secondPane)
  }

  it should "return to the pane focus came from, making it the active pane again" in {
    val start = escapingToPrevious(withPanels)
    val fromFirstPane = start.copy(persisted =
      start.persisted.copy(
        focus = Focus.EditorPane(firstPane),
        layout = start.persisted.layout.copy(activeEditorPaneId = Some(firstPane))
      )
    )
    val inA = focusPanel(fromFirstPane, panelA)
    val activeMoved =
      inA.copy(persisted =
        inA.persisted.copy(layout = inA.persisted.layout.copy(activeEditorPaneId = Some(secondPane)))
      )

    val returned = valid(press(PanelInputEvent.Dismiss, activeMoved))

    returned.persisted.focus shouldBe Focus.EditorPane(firstPane)
    returned.persisted.layout.activeEditorPaneId shouldBe Some(firstPane)
  }

  it should "fall back to the active pane when what had focus before is gone" in {
    val inB = moveFocusOnto(focusPanel(escapingToPrevious(withPanels), panelA), panelB)
    val withoutPanelA =
      inB.copy(runtime = inB.runtime.copy(uiSurfaces = inB.runtime.uiSurfaces.filterNot(_.id == panelA)))

    press(PanelInputEvent.Dismiss, withoutPanelA).persisted.focus shouldBe Focus.EditorPane(secondPane)
  }

  it should "follow the setting for the current app mode" in {
    val codeReturnsToPrevious = configured(
      withPanels,
      _.withPanelEscapeTarget(AppMode.Code, PanelEscapeTarget.Previous)
        .withPanelEscapeTarget(AppMode.Prose, PanelEscapeTarget.Editor)
    )
    def inBUnder(mode: AppMode) =
      moveFocusOnto(focusPanel(configured(codeReturnsToPrevious, _.withAppMode(mode)), panelA), panelB)

    press(PanelInputEvent.Dismiss, inBUnder(AppMode.Code)).persisted.focus shouldBe Focus.Surface(panelA)
    press(PanelInputEvent.Dismiss, inBUnder(AppMode.Prose)).persisted.focus shouldBe Focus.EditorPane(secondPane)
  }

  "Typing in a focused panel" should "go to the editor's active pane whichever Escape target is set" in
    List(PanelEscapeTarget.Editor, PanelEscapeTarget.Previous).foreach { target =>
      val state = configured(withPanels, _.withPanelEscapeTarget(AppMode.Code, target))
      val inB   = moveFocusOnto(focusPanel(state, panelA), panelB)

      press(PanelInputEvent.ReturnFocus, inB).persisted.focus shouldBe Focus.EditorPane(secondPane)
    }

  "Focusing a panel" should "remember what had focus before it" in {
    focusPanel(withPanels, panelA).runtime.focusHistory.headOption shouldBe Some(Focus.EditorPane(secondPane))
    PanelStateReducer.expand(panelA, withPanels).state.runtime.focusHistory.headOption shouldBe
      Some(Focus.EditorPane(secondPane))
  }

  it should "not remember the panel itself when it already has focus" in {
    val inA = focusPanel(withPanels, panelA)

    focusPanel(inA, panelA).runtime.focusHistory shouldBe inA.runtime.focusHistory
    PanelStateReducer.expand(panelA, inA).state.runtime.focusHistory shouldBe inA.runtime.focusHistory
  }
