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

  def modalType(modal: Modal): ModalType =
    modal match
      case Modal.GotoLine(_)                    => ModalType.GotoLine
      case Modal.RenameSymbol(_, _, _, _, _, _) => ModalType.RenameSymbol
      case Modal.Find(_, _, _)                  => ModalType.Find
      case Modal.FileWorkflow(_)                => ModalType.FileWorkflow
      case Modal.ReplaceWorkflow(_)             => ModalType.ReplaceWorkflow
      case Modal.CloseWorkflow(_)               => ModalType.CloseWorkflow
      case Modal.ReloadConflict(_)              => ModalType.ReloadConflict
      case Modal.SessionNamePrompt(_, _)        => ModalType.SessionNamePrompt
      case Modal.SessionList(_, _, _)           => ModalType.SessionList
      case Modal.Custom(name, _)                => ModalType.Custom(name)

  /** A click on an action button of the close or reload-conflict prompt also submits it: those prompts have no separate
    * confirm step, so picking Save/Discard/Cancel (or Reload/Overwrite/Cancel) is the decision itself.
    */
  def input(event: MouseInputEvent, state: AppState): Transition[Unit] =
    event match
      case click: MouseClick if click.button == MouseButton.Primary =>
        modalHitAt(click, state).fold(Transition.unit) { (modal, hit) =>
          val clickedType = modalType(modal)
          val submits =
            (clickedType == ModalType.CloseWorkflow || clickedType == ModalType.ReloadConflict) &&
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
          focusedFloatingModalWorkflow(state).flatMap(surface =>
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

  def focusedFloatingModalWorkflow(state: AppState): Option[UiSurface] =
    for
      surfaceId <- state.persisted.focus match
        case Focus.Surface(id) => Some(id)
        case _                 => None
      surface <- state.runtime.uiSurfaces.find(_.id == surfaceId)
      _ <- surface.presentation match
        case SurfacePresentation.Floating(_, _) => Some(())
        case _                                  => None
      _ <- surface.content match
        case SurfaceContent.ModalWorkflow(_) => Some(())
        case _                               => None
    yield surface

final private[manager] class ModalMouseHitTesting(port: ModalMouseHitTestingPort):

  def handleModalMouseInput(event: MouseInputEvent, state: AppState): IO[Unit] =
    MouseTransition.commit(port.currentState, port.applyReducerResult)(ModalMouseHitTesting.input(event, state))

  def modalHitAt(click: MouseClick, state: AppState): Option[(Modal, SurfaceHitRegion)] =
    ModalMouseHitTesting.modalHitAt(click, state)

  def focusedFloatingModalWorkflow(state: AppState): Option[UiSurface] =
    ModalMouseHitTesting.focusedFloatingModalWorkflow(state)
