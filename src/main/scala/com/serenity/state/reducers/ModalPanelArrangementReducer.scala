package com.serenity.state.reducers

import com.serenity.command.{Command, CommandIntent, ViewIntent}
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.ui.layout.PanelArrangementComposition

/** Input for a [[Modal.PanelArrangement]]: arrows and Tab move the selection, Alt+arrows move the selected panel, Enter
  * or Space shows or hides it, a click selects the clicked panel, and Escape closes the list. A move asks for the panel
  * to be placed and leaves the list as it is; the commit that moves the panel brings the list up to date.
  */
private[reducers] object ModalPanelArrangementReducer:
  import ModalEventReducer.{currentModal, dismissToPane, updateModal}

  def reduce(event: ModalInputEvent, currentState: AppState): ReducerResult =
    currentModal(currentState) match
      case Some((id, Modal.PanelArrangement(arrangement))) =>
        def selecting(next: PanelArrangement) =
          ReducerResult.noEffects(updateModal(currentState, id, Modal.PanelArrangement(next)))
        event match
          case ModalDismiss                                     => ReducerResult.noEffects(dismissToPane(currentState))
          case ModalNavigate(Direction.Down) | ModalNextField   => selecting(arrangement.selectionMoved(1))
          case ModalNavigate(Direction.Up) | ModalPreviousField => selecting(arrangement.selectionMoved(-1))
          case ModalMove(Direction.Down)                        => placed(arrangement.moved(1), currentState)
          case ModalMove(Direction.Up)                          => placed(arrangement.moved(-1), currentState)
          case ModalSubmit | ModalInsertChar(' ')               => placed(arrangement.toggled, currentState)
          case ModalClick(_, Some(actionId)) =>
            PanelArrangementComposition
              .rowPanel(actionId)
              .fold(ReducerResult.noEffects(currentState))(panel => selecting(arrangement.selecting(panel)))
          case _ => ReducerResult.noEffects(currentState)
      case _ => ReducerResult.noEffects(currentState)

  private def placed(placement: Option[PanelPlacement], state: AppState): ReducerResult =
    placement.fold(ReducerResult.noEffects(state)) {
      case PanelPlacement(id, position, index) =>
        ReducerResult.withEffect(
          state,
          AppEffect.ExecuteCommand(
            Command.typed(
              s"place-${id.key}-panel",
              s"Place the ${PanelRegistry.registrationFor(id).label} panel.",
              CommandIntent.View(ViewIntent.PlacePanel(id, position, index))
            )
          )
        )
    }
