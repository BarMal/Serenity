package com.serenity.state.models

import com.serenity.command.Command
import com.serenity.ui.widget.{EndBehaviour, Loadable, SelectableList}

/** One entry of a [[ListPicker]]: what it shows, and the command picking it runs. A choice with a `waitingLabel` starts
  * slow work: picking it keeps the picker open, showing that label, until the work lands.
  */
final case class ListChoice(
    label: String,
    detail: Option[String],
    action: Command,
    waitingLabel: Option[String] = None
)

/** A titled list to pick one entry from, as data -- so a new picker needs only a value of this. Its choices may still
  * be loading when it opens. While it is `pending` on a picked choice, that choice's work applies only if the picker is
  * still open and pending on it when the work lands, so Escape abandons it.
  */
final case class ListPicker(
    title: String,
    items: Loadable[SelectableList[ListChoice]],
    pending: Option[ListChoice] = None
):
  def selectedChoice: Option[ListChoice] = items.toOption.flatMap(_.selectedItem)

  def isPendingOn(command: Command): Boolean = pending.exists(_.action == command)

  def withChoices(choices: Seq[ListChoice], emptyMessage: String): ListPicker =
    copy(items = ListPicker.loaded(choices, emptyMessage))

object ListPicker:

  def loading(title: String): ListPicker = ListPicker(title, Loadable.Loading())

  def of(title: String, choices: Seq[ListChoice], emptyMessage: String): ListPicker =
    ListPicker(title, loaded(choices, emptyMessage))

  /** The open picker waiting on `command`, if any. */
  def pendingOn(state: AppState, command: Command): Option[SurfaceId] =
    state.runtime.uiSurfaces.collectFirst {
      case UiSurface(id, SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)), _, _) if picker.isPendingOn(command) =>
        id
    }

  private def loaded(choices: Seq[ListChoice], emptyMessage: String): Loadable[SelectableList[ListChoice]] =
    if choices.isEmpty then Loadable.Empty(emptyMessage)
    else Loadable.Ready(SelectableList.of(choices, EndBehaviour.Wrap))
