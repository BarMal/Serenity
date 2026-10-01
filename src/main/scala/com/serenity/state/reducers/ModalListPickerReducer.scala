package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.ui.layout.ListPickerComposition
import com.serenity.ui.widget.{Loadable, WidgetInput}

/** Input for a [[Modal.ListPicker]]: arrows and Tab move the highlight, Enter runs the highlighted choice's command,
  * Escape closes it, and a click highlights the clicked choice (the mouse layer then submits it). A choice that waits
  * on slow work keeps the picker open and pending on it; nothing else can be picked meanwhile.
  */
private[reducers] object ModalListPickerReducer:
  import ModalEventReducer.{currentModal, dismissToPane, updateModal}

  def reduce(event: ModalInputEvent, currentState: AppState): ReducerResult =
    currentModal(currentState) match
      case Some((id, Modal.ListPicker(picker))) =>
        event match
          case ModalDismiss                  => ReducerResult.noEffects(dismissToPane(currentState))
          case _ if picker.pending.isDefined => ReducerResult.noEffects(currentState)
          case ModalSubmit                   => picked(id, picker, currentState)
          case ModalClick(_, Some(actionId)) =>
            ListPickerComposition
              .choiceIndex(actionId)
              .fold(ReducerResult.noEffects(currentState))(index =>
                moved(id, picker, WidgetInput.Click(index, 1), currentState)
              )
          case other =>
            navigation(other).fold(ReducerResult.noEffects(currentState))(moved(id, picker, _, currentState))
      case _ => ReducerResult.noEffects(currentState)

  private def navigation(event: ModalInputEvent): Option[WidgetInput] =
    event match
      case ModalNextField | ModalNavigate(Direction.Down) | ModalNavigate(Direction.Right)  => Some(WidgetInput.Down)
      case ModalPreviousField | ModalNavigate(Direction.Up) | ModalNavigate(Direction.Left) => Some(WidgetInput.Up)
      case _                                                                                => None

  private def moved(id: SurfaceId, picker: ListPicker, input: WidgetInput, state: AppState): ReducerResult =
    picker.items match
      case Loadable.Ready(choices) =>
        val (updated, _) = choices.update(input, ListPickerComposition.VisibleRows)
        ReducerResult.noEffects(updateModal(state, id, Modal.ListPicker(picker.copy(items = Loadable.Ready(updated)))))
      case _ => ReducerResult.noEffects(state)

  private def picked(id: SurfaceId, picker: ListPicker, state: AppState): ReducerResult =
    picker.selectedChoice match
      case Some(choice) if choice.waitingLabel.isDefined =>
        val waiting = updateModal(state, id, Modal.ListPicker(picker.copy(pending = Some(choice))))
        ReducerResult.withEffect(waiting, AppEffect.ExecuteCommand(choice.action))
      case Some(choice) => ReducerResult.withEffect(dismissToPane(state), AppEffect.ExecuteCommand(choice.action))
      case None         => ReducerResult.noEffects(state)
