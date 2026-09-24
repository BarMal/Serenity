package com.serenity.state.manager

import com.serenity.animation.SweepDirection
import com.serenity.command.CommandRegistry
import com.serenity.config.AppConfig
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.keystroke.events.{NextTab, ResizeEvent, ToggleCommandRunner}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, AppEventReducer, ReducerResult, SurfaceEffect}
import com.serenity.state.undo.UndoState
import com.serenity.ui.layout.{SplitAxis, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** [[EventPipelineTransitions]] as plain values: each step the pipeline used to write on its own after the event's
  * commit (#1183) is now part of the event's transition, so every result here must pass `AppStateValidation`.
  */
class EventPipelineTransitionsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def valid(state: AppState): AppState =
    AppStateValidation.validationErrors(state) shouldBe Nil
    state

  /** Two panes side by side: pane 0 shows buffer 0 ("alpha"), pane 1 shows buffer 1 ("beta"), pane 0 focused. */
  private def twoPanes(config: AppConfig = AppConfig.default): AppState =
    val base  = AppState.initial(config)
    val split = com.serenity.state.core.EditorState.splitFocusedPane(base, SplitAxis.Horizontal)
    val alpha = Buffer.fromString(BufferId(0), "alpha")
    val beta  = Buffer.fromString(BufferId(1), "beta")
    split.copy(
      persisted = split.persisted.copy(
        buffers = Map(alpha.id -> alpha, beta.id -> beta),
        bufferOrder = List(alpha.id, beta.id),
        layout = split.persisted.layout.copy(
          editorPanes = Map(
            PaneId(0) -> EditorPane.withBuffer(PaneId(0), alpha.id),
            PaneId(1) -> EditorPane.withBuffer(PaneId(1), beta.id)
          ),
          activeEditorPaneId = Some(PaneId(0))
        ),
        focus = Focus.EditorPane(PaneId(0))
      ),
      runtime = split.runtime.copy(nextBufferId = BufferId(2))
    )

  "resized" should "record the new viewport and rebalance panes onto the focused buffer in one transition" in {
    val focusedOnSecondPane =
      val state = twoPanes()
      state.copy(persisted = state.persisted.copy(focus = Focus.EditorPane(PaneId(1))))

    val result = EventPipelineTransitions.resized(ResizeEvent(ViewportSize(120, 40)), valid(focusedOnSecondPane))

    val resized = valid(result.state)
    resized.runtime.viewportSize shouldBe Some(ViewportSize(120, 40))
    resized.persisted.layout.activeEditorPaneId shouldBe Some(PaneId(1))
    result.effects shouldBe Nil
  }

  "withCursorPeekAnchorResolved" should "resolve a freshly set peek anchor in the result it is handed, keeping its effects" in {
    val state = twoPanes()
    val peeking = state.copy(runtime =
      state.runtime.copy(
        viewportSize = Some(ViewportSize(100, 30)),
        cursorPeekAnchor = Some(CursorPosition(0, 2))
      )
    )
    val effect = AppEffect.Surface(SurfaceEffect.OpenFileSearch)

    val result = EventPipelineTransitions.withCursorPeekAnchorResolved(ReducerResult(valid(peeking), List(effect)))

    valid(result.state).runtime.cursorPeekResolvedAnchor shouldBe defined
    result.effects shouldBe List(effect)
  }

  it should "leave a result without a pending anchor untouched" in {
    val result = ReducerResult.noEffects(twoPanes())

    EventPipelineTransitions.withCursorPeekAnchorResolved(result) shouldBe result
  }

  "committed" should "fold a result's state and its animation effects into the model" in {
    val model  = Model(twoPanes(), UndoState(), Map.empty)
    val result = AppEventReducer.reduce(NextTab, model.app, CommandRegistry.withToggleUI)

    val next = EventPipelineTransitions.committed(model, result)

    next.app shouldBe result.state
    next.undo shouldBe model.undo
  }

  "withPaneFlow" should "sweep the active pane's buffer so the tab switch and its animation commit together" in {
    val state     = twoPanes(AppConfig.withTestAnimations)
    val switched  = AppEventReducer.reduce(NextTab, state, CommandRegistry.withToggleUI)
    val committed = EventPipelineTransitions.committed(Model(state, UndoState(), Map.empty), switched)

    val swept = EventPipelineTransitions.withPaneFlow(committed, SweepDirection.Backward)

    val app = valid(swept.app)
    app shouldBe switched.state
    val sweptBuffer = app.focusedBufferId.getOrElse(fail("expected a focused buffer"))
    swept.bufferAnimations.get(sweptBuffer).exists(_.hasActiveAnimations) shouldBe true
  }

  it should "leave buffer animations alone when UI transitions are off" in {
    val model = Model(twoPanes(AppConfig.default.withUiAnimation(None)), UndoState(), Map.empty)

    EventPipelineTransitions.withPaneFlow(model, SweepDirection.Forward) shouldBe model
  }

  "commandRunnerFocusNormalized" should "hand focus back to an open command runner that lost it" in {
    val opened =
      AppEventReducer.reduce(ToggleCommandRunner, twoPanes(), CommandRegistry.withToggleUI).state
    val runnerFocus = opened.persisted.focus
    val unfocused   = opened.copy(persisted = opened.persisted.copy(focus = Focus.EditorPane(PaneId(0))))

    valid(EventPipelineTransitions.commandRunnerFocusNormalized(unfocused)).persisted.focus shouldBe runnerFocus
  }

  it should "leave a state without a command runner untouched" in {
    val state = twoPanes()

    EventPipelineTransitions.commandRunnerFocusNormalized(state) shouldBe state
  }
