package com.serenity.state.manager

import com.serenity.animation.SweepDirection
import com.serenity.keystroke.events.ResizeEvent
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, BufferId, Focus, PaneId, SurfaceContent, replacedWhere}
import com.serenity.state.reducers.{AppEventReducer, ReducerResult, SystemEventReducer}
import com.serenity.ui.layout.SplitAxis
import com.serenity.ui.presets.UiPreset

/** The event pipeline's own steps around a reducer, as pure functions, so each lands in the event's single validated
  * commit instead of a write of its own after it (#1183, #1697).
  */
private[manager] object EventPipelineTransitions:

  def resized(event: ResizeEvent, state: AppState): ReducerResult =
    val reduced = SystemEventReducer.reduce(event, state)
    reduced.copy(state = AppEventReducer.rebalancePanes(reduced.state, reduced.state.focusedBufferId))

  // Resolving the frozen cursor anchor to a screen position needs LayoutEngine, which reducers may not touch
  // (ArchitectureChecks.ForbiddenImports), so the pipeline does it right after the reduce that may have set it.
  def withCursorPeekAnchorResolved(result: ReducerResult): ReducerResult =
    result.copy(state = CursorPeekAnchorResolution.resolve(result.state))

  /** `result`'s state with its model-only effects (animations, undo bookkeeping) folded in. */
  def committed(model: Model, result: ReducerResult): Model =
    ModelCommit.applyModelEffects(model.copy(app = result.state), result.effects)

  def withPaneFlow(model: Model, sweep: SweepDirection): Model =
    model.copy(bufferAnimations = AnimationChoreography.withPaneFlowAnimation(model.app, sweep)(model.bufferAnimations))

  def commandRunnerFocusNormalized(state: AppState): AppState =
    if state.hasCommandRunnerDomain && !state.isCommandRunnerDomainFocus() then
      state.preferredCommandRunnerFocus.fold(state)(focus =>
        state.copy(persisted = state.persisted.copy(focus = focus))
      )
    else state

  def withCommandRunnerUiPresetPreviews(model: Model, previews: List[UiPreset.Preview]): Model =
    val state = model.app
    state.commandRunnerSurface.fold(model) { surface =>
      surface.content match
        case SurfaceContent.CommandPalette(runner) =>
          val updatedSurfaces = state.runtime.uiSurfaces.replacedWhere(_.id == surface.id)(
            _.copy(content = SurfaceContent.CommandPalette(runner.withUiPresetPreviews(previews)))
          )
          model.copy(app = state.copy(runtime = state.runtime.copy(uiSurfaces = updatedSurfaces)))
        case _ => model
    }

  /** Advances each buffer's `markdownPreviewEditGeneration`, marking an edit burst the preview has not caught up with.
    */
  def withMarkdownPreviewEditsBumped(state: AppState, bufferIds: List[BufferId]): AppState =
    val buffers = bufferIds.foldLeft(state.persisted.buffers)((buffers, bufferId) =>
      buffers.updatedWith(bufferId)(
        _.map(buffer => buffer.copy(markdownPreviewEditGeneration = buffer.markdownPreviewEditGeneration + 1))
      )
    )
    state.copy(persisted = state.persisted.copy(buffers = buffers))

  /** Focus after a dismissed surface: the active editor pane, or -- when none is left -- a fresh empty buffer in a new
    * pane, all in the dismissal's own commit.
    */
  def dismissedToEditor(state: AppState)(using Balance): AppState =
    state.persisted.layout.activeEditorPaneId match
      case Some(paneId) => focused(state, paneId)
      case None =>
        val creation = EditorTransitions.bufferCreated(state, "", None)
        val (withPane, paneId) = EditorTransitions.paneInserted(
          creation.created,
          creation.created.persisted.layout.orderedPaneIds.lastOption,
          Some(creation.bufferId),
          SplitAxis.Horizontal
        )
        focused(withPane, paneId)

  private def focused(state: AppState, paneId: PaneId): AppState =
    state.copy(persisted = state.persisted.copy(focus = Focus.EditorPane(paneId)))
