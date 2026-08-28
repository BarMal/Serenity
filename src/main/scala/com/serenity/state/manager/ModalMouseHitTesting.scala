package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.ui.layout.*

/** State the event pipeline exposes for routing mouse input into a blocking or floating modal workflow. */
private[manager] trait ModalMouseHitTestingPort:
  def stateRef: Ref[IO, AppState]
  def applyReducerResult(result: ReducerResult, fallbackState: AppState): IO[Unit]

/** Hit-tests mouse input against the topmost blocking modal (or a focused floating modal workflow) and translates a hit
  * into a `ModalEventReducer` click, independent of every other mouse target -- editor text, panels, and overlays are
  * never reachable while a modal owns the click.
  */
private[manager] object ModalMouseHitTesting:

  def modalType(modal: Modal): ModalType =
    modal match
      case Modal.GotoLine(_)        => ModalType.GotoLine
      case Modal.Find(_, _, _)      => ModalType.Find
      case Modal.FileWorkflow(_)    => ModalType.FileWorkflow
      case Modal.ReplaceWorkflow(_) => ModalType.ReplaceWorkflow
      case Modal.CloseWorkflow(_)   => ModalType.CloseWorkflow
      case Modal.Custom(name, _)    => ModalType.Custom(name)

final private[manager] class ModalMouseHitTesting(port: ModalMouseHitTestingPort):
  import port.*

  def handleModalMouseInput(event: MouseInputEvent, state: AppState): IO[Unit] =
    event match
      case click: MouseClick if click.button == MouseButton.Primary =>
        modalHitAt(click, state) match
          case Some((modal, hit)) =>
            val modalType = ModalMouseHitTesting.modalType(modal)
            val clicked = ModalEventReducer.reduce(
              modalType,
              ModalClick(hit.focusId.value, hit.actionId.map(_.value)),
              state
            )
            applyReducerResult(clicked, state) >>
              Option
                .when(modalType == ModalType.CloseWorkflow && hit.actionId.nonEmpty)(())
                .fold(
                  IO.unit
                )(_ =>
                  stateRef.get.flatMap { updatedState =>
                    applyReducerResult(
                      ModalEventReducer.reduce(ModalType.CloseWorkflow, ModalSubmit, updatedState),
                      updatedState
                    )
                  }
                )
          case None => IO.unit
      case _ =>
        IO.unit

  def modalHitAt(click: MouseClick, state: AppState): Option[(Modal, SurfaceHitRegion)] =
    for
      viewportSize <- state.runtime.viewportSize
      surface      <- state.topModalSurface.orElse(focusedFloatingModalWorkflow(state))
      node <- UiSceneSnapshot
        .from(state, viewportSize)
        .nodesInPaintOrder
        .find(_.id == SceneNodeId.Surface(surface.id))
      _ <- Option.when(node.frameRect.contains(click.col, click.row))(())
      modal <- surface.content match
        case SurfaceContent.ModalWorkflow(modal) => Some(modal)
        case _                                   => None
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
