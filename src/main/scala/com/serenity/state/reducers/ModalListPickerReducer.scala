package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.ui.layout.ListPickerComposition
import com.serenity.ui.widget.{Loadable, TextField, TextFieldOutcome, WidgetInput}

/** Input for a [[Modal.ListPicker]]: arrows and Tab move the highlight, PageUp/PageDown page it, Ctrl+Home/Ctrl+End
  * jump to the first or last loaded choice, Enter runs the highlighted choice's command, Escape closes it (then runs
  * its `onDismiss`), and a click highlights the clicked choice (the mouse layer then submits it). A picker with a query
  * takes typing, deletes, paste, Left/Right and Home/End as edits to it, and recomputes its items after each change;
  * without one, Home/End jump like Ctrl+Home/Ctrl+End. Each move of the highlight runs the newly highlighted choice's
  * preview. Moving down off, or paging down onto, the last choice of a picker whose source has more loads the next
  * batch first. A choice that waits on slow work keeps the picker open and pending on it; nothing else can be picked or
  * typed meanwhile.
  */
private[reducers] object ModalListPickerReducer:
  import ModalEventReducer.{currentModal, dismissToPane, updateModal}

  def reduce(event: ModalInputEvent, currentState: AppState): ReducerResult =
    currentModal(currentState) match
      case Some((id, Modal.ListPicker(picker))) =>
        event match
          case ModalDismiss =>
            ReducerResult(
              dismissToPane(currentState),
              picker.onDismiss.map(AppEffect.ExecuteCommandUnrecorded(_)).toList
            )
          case _ if picker.pending.isDefined => ReducerResult.noEffects(currentState)
          case ModalSubmit                   => picked(id, picker, currentState)
          case ModalClick(_, Some(actionId)) =>
            ListPickerComposition
              .choiceIndex(actionId)
              .fold(ReducerResult.noEffects(currentState))(index =>
                shown(id, picker, moved(picker, WidgetInput.Click(index, 1), currentState), currentState)
              )
          case other =>
            updated(other, picker, currentState)
              .fold(ReducerResult.noEffects(currentState))(shown(id, picker, _, currentState))
      case _ => ReducerResult.noEffects(currentState)

  private def updated(event: ModalInputEvent, picker: ListPicker, state: AppState): Option[ListPicker] =
    val edit = for
      field <- picker.query
      input <- queryEditing(event, state)
    yield edited(picker, field, input, state)
    edit.orElse(navigation(event).map(moved(picker, _, state)))

  private def queryEditing(event: ModalInputEvent, state: AppState): Option[WidgetInput] =
    event match
      case ModalInsertChar(char)          => Some(WidgetInput.Insert(char))
      case ModalDeleteBackward            => Some(WidgetInput.DeleteBackward)
      case ModalDeleteForward             => Some(WidgetInput.DeleteForward)
      case ModalDeleteWordBackward        => Some(WidgetInput.DeleteWordBackward)
      case ModalDeleteWordForward         => Some(WidgetInput.DeleteWordForward)
      case ModalPaste                     => state.runtime.clipboard.map(WidgetInput.InsertText(_))
      case ModalNavigate(Direction.Left)  => Some(WidgetInput.Left)
      case ModalNavigate(Direction.Right) => Some(WidgetInput.Right)
      case ModalLineStart                 => Some(WidgetInput.First)
      case ModalLineEnd                   => Some(WidgetInput.Last)
      case _                              => None

  private def navigation(event: ModalInputEvent): Option[WidgetInput] =
    event match
      case ModalNextField | ModalNavigate(Direction.Down) | ModalNavigate(Direction.Right)  => Some(WidgetInput.Down)
      case ModalPreviousField | ModalNavigate(Direction.Up) | ModalNavigate(Direction.Left) => Some(WidgetInput.Up)
      case ModalLineStart | ModalFirst                                                      => Some(WidgetInput.First)
      case ModalLineEnd | ModalLast                                                         => Some(WidgetInput.Last)
      case ModalPage(pages) => Some(if pages < 0 then WidgetInput.PageUp else WidgetInput.PageDown)
      case _                => None

  private def edited(picker: ListPicker, field: TextField, input: WidgetInput, state: AppState): ListPicker =
    field.update(input) match
      case (next, Some(TextFieldOutcome.Changed(_))) =>
        ListPickerSearch.refreshed(picker.copy(query = Some(next)), state)
      case (next, _) => picker.copy(query = Some(next))

  private def moved(picker: ListPicker, input: WidgetInput, state: AppState): ListPicker =
    val grown = input match
      case WidgetInput.Down if picker.hasMore && isOnLastChoice(picker) => ListPickerSearch.extended(picker, state)
      case WidgetInput.PageDown if picker.hasMore && isOnLastChoice(movedWithinLoaded(picker, input)) =>
        ListPickerSearch.extended(picker, state)
      case _ => picker
    movedWithinLoaded(grown, input)

  private def movedWithinLoaded(picker: ListPicker, input: WidgetInput): ListPicker =
    picker.items match
      case Loadable.Ready(choices) =>
        val (updated, _) = choices.update(input, ListPickerComposition.VisibleRows)
        picker.copy(items = Loadable.Ready(updated))
      case _ => picker

  private def isOnLastChoice(picker: ListPicker): Boolean =
    picker.items.toOption.exists(choices => choices.selected.contains(choices.items.size - 1))

  /** Shows `after` in place of `before`, previewing its highlighted choice if the highlight changed. */
  private def shown(id: SurfaceId, before: ListPicker, after: ListPicker, state: AppState): ReducerResult =
    val preview =
      if after.selectedChoice == before.selectedChoice then None else after.selectedChoice.flatMap(_.preview)
    ReducerResult(
      updateModal(state, id, Modal.ListPicker(after)),
      preview.map(AppEffect.ExecuteCommandUnrecorded(_)).toList
    )

  private def picked(id: SurfaceId, picker: ListPicker, state: AppState): ReducerResult =
    picker.selectedChoice match
      case Some(choice) if choice.waitingLabel.isDefined =>
        val waiting = updateModal(state, id, Modal.ListPicker(picker.copy(pending = Some(choice))))
        ReducerResult.withEffect(waiting, AppEffect.ExecuteCommand(choice.action))
      case Some(choice) => ReducerResult.withEffect(dismissToPane(state), AppEffect.ExecuteCommand(choice.action))
      case None         => ReducerResult.noEffects(state)
