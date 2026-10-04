package com.serenity.command

import com.serenity.config.{HotkeyAction, HotkeyConfig, HotkeyTrigger}

/** The one table from a registry command to the keys that run it (issue #1922): the keys bound to the command by id,
  * plus those of the [[HotkeyAction]] that already performs it. The palette shows a command's key from here, so a
  * command bound in either way shows it.
  */
object CommandKeyBindings:

  def triggers(hotkeys: HotkeyConfig): Map[CommandId, List[HotkeyTrigger]] =
    val byId = hotkeys.commandBindings.toList.map((commandId, keys) => CommandId(commandId) -> keys)
    val byAction =
      HotkeyAction.values.toList.flatMap(action => commandFor(action).map(_ -> hotkeys.bindingsFor(action)))
    (byId ++ byAction).groupMap(_._1)(_._2).view.mapValues(_.flatten).toMap.filter(_._2.nonEmpty)

  /** Each bound command's first key, rendered, keyed by command name -- what a palette row shows. */
  def displayed(hotkeys: HotkeyConfig): Map[String, String] =
    triggers(hotkeys).toList
      .flatMap((commandId, keys) => keys.headOption.map(key => commandId.value -> key.render))
      .toMap

  /** The command a key bound by id runs, when the palette would let it run here: offered in this mode and frontend
    * ([[CommandScope]]) and with its prerequisites met.
    */
  def runnable(registry: CommandRegistry, commandId: String, context: CommandRunnerContext): Option[Command] =
    registry
      .findCommand(commandId)
      .filter(command =>
        CommandRelevance.isAvailable(command, context.editingContext) &&
          CommandPrerequisites.unmetReason(command, context).isEmpty
      )

  /** The registry command a global hotkey action performs, if there is one. Exhaustive, so a new action has to say. */
  def commandFor(action: HotkeyAction): Option[CommandId] =
    action match
      case HotkeyAction.Save                => Some(CommandId("save"))
      case HotkeyAction.SaveAs              => Some(CommandId("save-as"))
      case HotkeyAction.OpenFile            => Some(CommandId("open"))
      case HotkeyAction.FileSearch          => Some(CommandId("file-search"))
      case HotkeyAction.GoToFile            => Some(CommandId("go-to-file"))
      case HotkeyAction.Quit                => Some(CommandId("quit"))
      case HotkeyAction.NewTab              => Some(CommandId("new"))
      case HotkeyAction.NextTab             => Some(CommandId("next-tab"))
      case HotkeyAction.PreviousTab         => Some(CommandId("previous-tab"))
      case HotkeyAction.CloseTab            => Some(CommandId("close"))
      case HotkeyAction.SplitPaneHorizontal => Some(CommandId("split-pane-horizontal"))
      case HotkeyAction.SplitPaneVertical   => Some(CommandId("split-pane-vertical"))
      case HotkeyAction.ToggleChapterGhosts => Some(CommandId("toggle-chapter-ghosts"))
      case HotkeyAction.OpenChapterNote     => Some(CommandId("open-chapter-note"))
      case HotkeyAction.ToggleNotesPin      => Some(CommandId("toggle-notes-pin"))
      case HotkeyAction.ClosePane           => Some(CommandId("close-pane"))
      case HotkeyAction.Find                => Some(CommandId("find"))
      case HotkeyAction.Replace             => Some(CommandId("replace"))
      case HotkeyAction.Copy                => Some(CommandId("copy"))
      case HotkeyAction.Cut                 => Some(CommandId("cut"))
      case HotkeyAction.Paste               => Some(CommandId("paste"))
      case HotkeyAction.SelectAll           => Some(CommandId("select-all"))
      case HotkeyAction.Undo                => Some(CommandId("undo"))
      case HotkeyAction.Redo                => Some(CommandId("redo"))
      case HotkeyAction.GoToLine            => Some(CommandId("goto-line"))
      case HotkeyAction.FocusLeft           => Some(CommandId("focus-left"))
      case HotkeyAction.FocusRight          => Some(CommandId("focus-right"))
      case HotkeyAction.FocusUp             => Some(CommandId("focus-up"))
      case HotkeyAction.FocusDown           => Some(CommandId("focus-down"))
      case HotkeyAction.ToggleSyntaxHighlighting | HotkeyAction.ToggleCommandRunner |
          HotkeyAction.ToggleContextualToolbar | HotkeyAction.MoveTabLeft | HotkeyAction.MoveTabRight |
          HotkeyAction.ToggleShortcutsHelp =>
        None
