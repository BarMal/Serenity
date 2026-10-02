package com.serenity.state.manager

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.ui.layout.*

/** State the event pipeline exposes for routing mouse input into a blocking or floating modal workflow, as a capability
  * record rather than a trait -- nothing here breaks a construction-order cycle (#1389), so mockability is the only
  * reason this needs an interface at all, and a record fakes trivially without one (#1017).
  */
final private[manager] case class ModalMouseHitTestingPort(
    currentState: IO[AppState],
    applyReducerResult: (ReducerResult, AppState) => IO[Unit]
)

/** Hit-tests mouse input against the topmost blocking modal (or a focused floating modal workflow) and translates a hit
  * into a `ModalEventReducer` click, independent of every other mouse target -- editor text, panels, and overlays are
  * never reachable while a modal owns the click.
  */
private[manager] object ModalMouseHitTesting:

  /** A click on an action button of a confirm prompt or a list picker also submits it: those have no separate confirm
    * step, so picking a choice is the decision itself.
    */
  def input(event: MouseInputEvent, state: AppState): Transition[Unit] =
    event match
      case click: MouseClick if click.button == MouseButton.Primary =>
        modalHitAt(click, state).fold(Transition.unit) { (modal, hit) =>
          val clickedType = ModalEventReducer.modalType(modal)
          val submits =
            Set(ModalType.Confirm, ModalType.ListPicker).contains(clickedType) &&
              hit.actionId.nonEmpty
          reduce(clickedType, ModalClick(hit.focusId.value, hit.actionId.map(_.value))) *>
            (if submits then reduce(clickedType, ModalSubmit) else Transition.unit)
        }
      case _ =>
        Transition.unit

  private def reduce(modalType: ModalType, event: ModalInputEvent): Transition[Unit] =
    Transition.get.flatMap(current => ModalEventReducer.reduce(modalType, event, current).toTransition)

  def modalHitAt(click: MouseClick, state: AppState): Option[(Modal, SurfaceHitRegion)] =
    for
      viewportSize <- state.runtime.viewportSize
      (id, modal) <- state.topModal
        .map(dialog => (dialog.id, dialog.modal))
        .orElse(
          ModalEventReducer
            .focusedFloatingModalWorkflow(state)
            .flatMap(surface =>
              surface.content match
                case SurfaceContent.ModalWorkflow(modal) => Some((surface.id, modal))
                case _                                   => None
            )
        )
      node <- UiSceneSnapshot
        .from(state, viewportSize)
        .nodesInPaintOrder
        .find(_.id == SceneNodeId.Surface(id))
      _ <- Option.when(node.frameRect.contains(click.col, click.row))(())
      targetRows = SurfaceFrameLayout.minimumTargetRows(state.persisted.config.interfaceDensity)
      hit <- ModalSurfaceComposition
        .forModal(modal, node.frameRect, targetRows)
        .flatMap(_.hitAt(click.col.toDouble, click.row.toDouble))
    yield (modal, hit)

  /** A primary press or click outside the focused floating modal workflow (`inside` says whether it landed inside)
    * dismisses it, so the input can go on to what it was aimed at.
    */
  def dismissedByOutsideInput(
    event: MouseInputEvent,
    state: AppState,
    inside: UiSurface => Boolean
  ): Option[ReducerResult] =
    ModalEventReducer
      .focusedFloatingModalWorkflow(state)
      .filter(surface => event.button == MouseButton.Primary && !inside(surface))
      .flatMap(_ => ModalEventReducer.dismissFocusedFloatingModalWorkflow(state))

final private[manager] class ModalMouseHitTesting(port: ModalMouseHitTestingPort):

  def handleModalMouseInput(event: MouseInputEvent, state: AppState): IO[Unit] =
    MouseTransition.commit(port.currentState, port.applyReducerResult)(ModalMouseHitTesting.input(event, state))

  def modalHitAt(click: MouseClick, state: AppState): Option[(Modal, SurfaceHitRegion)] =
    ModalMouseHitTesting.modalHitAt(click, state)

  def focusedFloatingModalWorkflow(state: AppState): Option[UiSurface] =
    ModalEventReducer.focusedFloatingModalWorkflow(state)
