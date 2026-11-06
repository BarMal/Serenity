package com.serenity.state.models

import java.nio.file.Path

import com.serenity.command.{Command, CommandRegistry, CommandRunner}
import com.serenity.io.ProjectFileListing
import com.serenity.ui.widget.{EndBehaviour, Loadable, SelectableList, TextField}

/** One entry of a [[ListPicker]]: what it shows, and the command picking it runs. A choice with a `waitingLabel` starts
  * slow work: picking it keeps the picker open, showing that label, until the work lands. A `preview` runs whenever the
  * choice becomes the highlighted one, without counting as a use of that command. A `hint` (such as the key that runs
  * the command) shows on the right; a choice with a `disabledReason` shows greyed with that reason, and picking it
  * refuses rather than runs.
  */
final case class ListChoice(
    label: String,
    detail: Option[String],
    action: Command,
    waitingLabel: Option[String] = None,
    preview: Option[Command] = None,
    hint: Option[String] = None,
    disabledReason: Option[String] = None,
    // Laid out in label/detail/hint columns even without a hint, as every row of the command picker is.
    columns: Boolean = false
)

/** What a picker's choices are recomputed from each time its query changes -- see [[ListPickerSearch.refreshed]]. Data
  * rather than a function, so a picker stays comparable and serialisable like the rest of the state.
  */
enum PickerSource:

  /** These choices, narrowed to the ones whose label or detail contains the query. */
  case Fixed(choices: Vector[ListChoice])

  /** The lines of every open buffer containing the query, `batchSize` at a time -- see [[BufferTextSearch]]. `resumeAt`
    * is where the next batch starts, while one is left.
    */
  case BufferText(batchSize: Int = BufferTextSearch.BatchSize, resumeAt: Option[BufferLine] = None)

  /** The files under `root`, ranked against the query by [[FileFinder]] once their listing lands. `matched` is what the
    * last non-empty query matched, for a longer one to narrow from.
    */
  case ProjectFiles(root: Path, listing: Loadable[ProjectFileListing], matched: Option[FileMatches] = None)

  /** The palette's commands and settings hits (#2577), searched by `runner` -- activated when the picker opened, so its
    * editing context is the one the picker was opened from -- over `registry`. Both compare by reference, which is
    * right: each is built once per open.
    */
  case Commands(runner: CommandRunner, registry: CommandRegistry)

  /** Whether more choices than the picker holds can still be loaded, by [[ListPickerSearch.extended]]. */
  def hasMore: Boolean =
    this match
      case Fixed(_)                => false
      case BufferText(_, resumeAt) => resumeAt.isDefined
      case ProjectFiles(_, _, _)   => false
      case Commands(_, _)          => false

/** A titled list to pick one entry from, as data -- so a new picker needs only a value of this. Its choices may still
  * be loading when it opens. While it is `pending` on a picked choice, that choice's work applies only if the picker is
  * still open and pending on it when the work lands, so Escape abandons it.
  *
  * A picker with a `query` shows it as an editable row; each edit recomputes its items from its `source`. Without a
  * source, its items are only ever the ones it was given. `onDismiss` runs, unrecorded, when Escape closes it. While
  * `searchPending`, its items are those of an earlier query: a background search for the current one is running.
  */
final case class ListPicker(
    title: String,
    items: Loadable[SelectableList[ListChoice]],
    pending: Option[ListChoice] = None,
    query: Option[TextField] = None,
    source: Option[PickerSource] = None,
    onDismiss: Option[Command] = None,
    searchPending: Boolean = false,
    // Why the last pick was refused; cleared by the next move or edit.
    message: Option[String] = None,
    // Key hints shown under the choices; left out when nothing matched, as there is no row to act on.
    footer: Option[String] = None
):
  def selectedChoice: Option[ListChoice] = items.toOption.flatMap(_.selectedItem)

  def isPendingOn(command: Command): Boolean = pending.exists(_.action == command)

  def queryText: String = query.fold("")(_.text)

  def hasMore: Boolean = source.exists(_.hasMore)

  def isCommandPicker: Boolean =
    source match
      case Some(PickerSource.Commands(_, _)) => true
      case _                                 => false

  def withChoices(choices: Seq[ListChoice], emptyMessage: String): ListPicker =
    copy(items = ListPicker.loaded(choices, emptyMessage))

object ListPicker:

  val NoMatches: String = "No matches"

  def loading(title: String): ListPicker = ListPicker(title, Loadable.Loading())

  def of(title: String, choices: Seq[ListChoice], emptyMessage: String): ListPicker =
    ListPicker(title, loaded(choices, emptyMessage))

  /** A picker over `choices` that the user narrows by typing; it opens on all of them, the first highlighted. */
  def filterable(title: String, choices: Seq[ListChoice], onDismiss: Option[Command] = None): ListPicker =
    ListPicker(
      title,
      loaded(choices, NoMatches),
      query = Some(TextField()),
      source = Some(PickerSource.Fixed(choices.toVector)),
      onDismiss = onDismiss
    )

  /** The open picker waiting on `command`, if any. */
  def pendingOn(state: AppState, command: Command): Option[SurfaceId] =
    state.runtime.uiSurfaces.collectFirst {
      case UiSurface(id, SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)), _, _) if picker.isPendingOn(command) =>
        id
    }

  private def loaded(choices: Seq[ListChoice], emptyMessage: String): Loadable[SelectableList[ListChoice]] =
    if choices.isEmpty then Loadable.Empty(emptyMessage)
    else Loadable.Ready(SelectableList.of(choices, EndBehaviour.Wrap))
