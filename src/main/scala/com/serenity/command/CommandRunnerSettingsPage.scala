package com.serenity.command

import com.serenity.config.HotkeyAction
import com.serenity.keystroke.KeyStrokeInfo

/** In-flight keybinding recording for a settings item, and any conflict it surfaced.
  *
  * Scoped to whichever page owns the recording (see `SettingsPage.Editing.recording`) rather than carried as per-item
  * fields on a flat submenu state, since the page stack (issue #1059) already knows which item, if any, is being
  * edited.
  */
final case class RecordingState(
    itemId: String,
    pendingRecordedBinding: Option[(KeyStrokeInfo, Long)] = None,
    pendingGlobalHotkeyConflict: Option[(HotkeyAction, String)] = None,
    pendingFocusedKeymapConflict: Option[(String, String)] = None
)

/** One page of the unified settings navigation stack (issue #1059).
  *
  * `Group` is a browsable list of a group's children (with its own local search); `Editing` is a single input item with
  * an in-progress text edit or keybinding recording. Both carry `groupId` so a page always knows which group it belongs
  * to without a separate ancestor-lookup field.
  */
enum SettingsPage:
  case Group(groupId: String, selectedIndex: Int = 0, searchTerm: String = "")

  case Editing(
      groupId: String,
      itemId: String,
      draftText: String,
      searchTerm: String = "",
      recording: Option[RecordingState] = None
  )

object SettingsPage:

  /** The group a page belongs to, regardless of which case it is, and the local search filtering its item list. Plain
    * `extension`s rather than members of the enum itself, so each case stays a normal case class -- notably keeping its
    * compiler-synthesized `copy` (an inherited abstract `def` overridden by a same-named case parameter suppresses that
    * synthesis under Scala 3, so this sidesteps it). `Editing` carries its own `searchTerm` (rather than reusing the
    * `Group` page's) so drilling into one input's edit doesn't lose the list filter it was reached through.
    */
  extension (page: SettingsPage)

    def groupId: String = page match
      case Group(id, _, _)         => id
      case Editing(id, _, _, _, _) => id

    def searchTerm: String = page match
      case Group(_, _, term)         => term
      case Editing(_, _, _, term, _) => term

    /** The item currently mid-edit, if `page` is an `Editing` page -- `None` for a plain `Group` page. */
    def editingItemId: Option[String] = page match
      case _: Group         => None
      case editing: Editing => Some(editing.itemId)

    /** The in-progress draft text for `page`'s edit -- empty for a plain `Group` page, since nothing is being edited
      * there.
      */
    def draftText: String = page match
      case _: Group         => ""
      case editing: Editing => editing.draftText

    /** The in-flight keybinding recording for `page`'s edit, if any -- `None` for a plain `Group` page. */
    def recording: Option[RecordingState] = page match
      case _: Group         => None
      case editing: Editing => editing.recording

/** The unified settings surface's navigation state: an explicit page stack (issue #1059).
  *
  * `current` is the page on screen; `ancestors` are the pages to return to, nearest first. Splitting the two (rather
  * than one `List[SettingsPage]` checked non-empty at runtime) makes "the stack always has a current page" a structural
  * guarantee instead of an invariant that has to be defended with a partial `head`/`tail` -- closing the settings
  * surface entirely is represented by the absence of a `SettingsSurfaceState`, not by an empty one.
  */
final case class SettingsSurfaceState(current: SettingsPage, ancestors: List[SettingsPage] = Nil):

  /** `current` followed by `ancestors`, nearest first -- for callers that want the whole stack as a list. */
  def stack: List[SettingsPage] = current :: ancestors

  def depth: Int = ancestors.size + 1

  /** Enter a child page, pushing it above the current one. */
  def push(page: SettingsPage): SettingsSurfaceState = SettingsSurfaceState(page, current :: ancestors)

  /** Leave the current page, revealing its parent. `None` at the root, where there is nothing left to pop -- callers
    * (Escape at depth 1) treat that as "close the whole surface" instead.
    */
  def pop: Option[SettingsSurfaceState] = ancestors match
    case parent :: rest => Some(SettingsSurfaceState(parent, rest))
    case Nil            => None

object SettingsSurfaceState:

  /** Reconstruct a state from a full stack (nearest-first, `head` = current). `Nil` is rejected since a
    * `SettingsSurfaceState` always has a current page -- see the class doc.
    */
  def apply(stack: List[SettingsPage]): SettingsSurfaceState =
    require(stack.nonEmpty, "SettingsSurfaceState requires at least one page")
    (stack: @unchecked) match
      case head :: rest => SettingsSurfaceState(head, rest)

  /** Escape always means "up one level, or close if there is no level left" -- never a mix of that and text-clearing,
    * unlike the Backspace overload it replaces (issue #1059).
    */
  enum EscapeOutcome:
    case Popped(state: SettingsSurfaceState)
    case CloseSurface

  def escape(state: SettingsSurfaceState): EscapeOutcome =
    state.pop match
      case Some(popped) => EscapeOutcome.Popped(popped)
      case None         => EscapeOutcome.CloseSurface

  /** Backspace always means "delete one character of the current page's text" -- it never pops or otherwise navigates
    * the stack, and is a no-op when the current page has no text to delete (issue #1059's "Backspace is overloaded
    * across search-term / edit-text / go-up-a-level").
    */
  def deleteBackward(state: SettingsSurfaceState): SettingsSurfaceState =
    state.current match
      case page: SettingsPage.Group if page.searchTerm.nonEmpty =>
        state.copy(current = page.copy(searchTerm = page.searchTerm.dropRight(1)))
      case page: SettingsPage.Editing if page.draftText.nonEmpty =>
        state.copy(current = page.copy(draftText = page.draftText.dropRight(1)))
      case _ =>
        state

  private val MaxPreviewRows = 4

  /** Capped, expand-in-place group preview rows for a group nested under the selected item -- capped to
    * `MaxPreviewRows` with an overflow count -- a pure function of the selected item, carrying no state of its own
    * (issue #1059).
    */
  final case class PreviewRows(rows: List[String], overflowCount: Int):
    def isEmpty: Boolean = rows.isEmpty

  def previewRows(items: List[CommandSurfaceItem], selectedIndex: Int): PreviewRows =
    items.lift(selectedIndex) match
      case Some(group: CommandSurfaceItem.GroupItem) =>
        val labels = group.children.map(previewLabel)
        PreviewRows(labels.take(MaxPreviewRows), math.max(0, labels.size - MaxPreviewRows))
      case _ =>
        PreviewRows(Nil, 0)

  private def previewLabel(item: CommandSurfaceItem): String =
    item match
      case CommandSurfaceItem.CommandItem(command)    => command.label
      case item: CommandSurfaceItem.OptionItem        => item.label
      case item: CommandSurfaceItem.InputItem         => item.label
      case item: CommandSurfaceItem.SettingSearchItem => item.label
      case item: CommandSurfaceItem.GroupItem         => item.label
