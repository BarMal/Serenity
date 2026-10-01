package com.serenity.command

import com.serenity.state.models.Shell

/** Whether a settings row is shown on the running frontend. A row with no visible effect there is hidden, unless Show
  * All Settings asks for everything: then it stays, its hint led by a note that it is inert here, so a config can still
  * be prepared from either frontend. Applying a value is unaffected either way (see [[CommandScope.of]]).
  */
final private[command] case class SettingsFrontendFilter(shell: Shell, showAll: Boolean):

  def row[A](frontend: FrontendSupport, item: A)(using hint: SettingsRowHint[A]): Option[A] =
    if frontend.shells.contains(shell) then Some(item)
    else Option.when(showAll)(hint.lead(item, inertNote))

  def rows[A : SettingsRowHint](frontend: FrontendSupport, items: List[A]): List[A] =
    items.flatMap(row(frontend, _))

  // The note leads rather than trails because the settings surface's hint column is a fixed share of the panel width
  // and elides from the right (`TextOverlayRenderer.fitCellText`): a trailing note is cut off before it can be read.
  private def inertNote: String =
    shell match
      case Shell.Tui => "Inert in TUI mode"
      case Shell.Gui => "Inert in GUI mode"

private[command] trait SettingsRowHint[A]:
  def lead(item: A, note: String): A

private[command] object SettingsRowHint:

  private def led(note: String, hint: String): String = s"$note -- $hint"

  given SettingsRowHint[CommandSurfaceItem.OptionItem] =
    (item, note) => item.copy(hint = Some(led(note, item.hint.getOrElse(item.label))))

  given SettingsRowHint[CommandSurfaceItem.InputItem] =
    (item, note) => item.copy(hint = led(note, item.hint))

  given SettingsRowHint[CommandSurfaceItem.GroupItem] =
    (item, note) => item.copy(hint = Some(led(note, item.hint.getOrElse(item.label))))
