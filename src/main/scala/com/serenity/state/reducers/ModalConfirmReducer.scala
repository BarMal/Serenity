package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.ui.layout.ConfirmComposition
import com.serenity.ui.widget.WidgetInput

/** Input for a [[Modal.Confirm]] prompt: the arrows and Tab move between its choices, Home/End and paging jump to the
  * first or last, Enter takes the highlighted one, Escape takes the prompt's `onDismiss`, and a click highlights the
  * clicked choice (the mouse layer then submits it).
  */
private[reducers] object ModalConfirmReducer:
  import ModalEventReducer.{currentModal, dismissToPane, updateModal}

  def reduce(event: ModalInputEvent, currentState: AppState): ReducerResult =
    currentModal(currentState) match
      case Some((id, Modal.Confirm(prompt))) =>
        event match
          case ModalDismiss => answered(prompt.onDismiss, currentState)
          case ModalSubmit  => answered(prompt.selectedChoice.fold(ConfirmAction.Dismiss)(_.action), currentState)
          case ModalClick(_, actionId) =>
            actionId
              .flatMap(ConfirmComposition.choiceIndex)
              .fold(ReducerResult.noEffects(currentState))(index =>
                moved(id, prompt, WidgetInput.Click(index, 1), currentState)
              )
          case other =>
            navigation(other).fold(ReducerResult.noEffects(currentState))(moved(id, prompt, _, currentState))
      case _ => ReducerResult.noEffects(currentState)

  private def navigation(event: ModalInputEvent): Option[WidgetInput] =
    event match
      case ModalNextField | ModalNavigate(Direction.Down) | ModalNavigate(Direction.Right)  => Some(WidgetInput.Down)
      case ModalPreviousField | ModalNavigate(Direction.Up) | ModalNavigate(Direction.Left) => Some(WidgetInput.Up)
      case ModalLineStart | ModalFirst                                                      => Some(WidgetInput.First)
      case ModalLineEnd | ModalLast                                                         => Some(WidgetInput.Last)
      case ModalPage(pages) => Some(if pages < 0 then WidgetInput.PageUp else WidgetInput.PageDown)
      case _                => None

  private def moved(id: SurfaceId, prompt: ConfirmPrompt, input: WidgetInput, state: AppState): ReducerResult =
    val (choices, _) = prompt.choices.update(input, prompt.choices.items.size)
    ReducerResult.noEffects(updateModal(state, id, Modal.Confirm(prompt.copy(choices = choices))))

  private def answered(action: ConfirmAction, state: AppState): ReducerResult =
    val dismissed = dismissToPane(state)
    action match
      case ConfirmAction.Run(command) => ReducerResult.withEffect(dismissed, AppEffect.ExecuteCommand(command))
      case ConfirmAction.Dismiss      => ReducerResult.noEffects(dismissed)
