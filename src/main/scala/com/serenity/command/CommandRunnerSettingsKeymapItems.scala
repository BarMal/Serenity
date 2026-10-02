package com.serenity.command

import com.serenity.config.*

/** Keybinding input items for every focused keymap and global hotkey. Split out of
  * `CommandRunnerSettingsInputItems.build` to keep both under the architecture size targets -- see that object's doc.
  */
private[command] object CommandRunnerSettingsKeymapItems:

  private[command] def buildKeymapInputItems(config: InputConfig): List[CommandSurfaceItem.InputItem] =
    val globalActions = List(HotkeyAction.ToggleCommandRunner, HotkeyAction.FileSearch) ++
      HotkeyAction.values.toList.filterNot(action =>
        action == HotkeyAction.ToggleCommandRunner || action == HotkeyAction.FileSearch
      )
    val items = globalActions.map(action =>
      bindingInputItem(
        s"keymap-global-${action.configKey}",
        if action == HotkeyAction.OpenFile then "Open Document" else keymapLabel(action.configKey),
        config.hotkeyConfig.bindingsFor(action).map(_.render).reduceOption(_ + ", " + _),
        binding => CommandIntent.Keybindings(KeybindingsIntent.SetGlobalHotkey(action, binding)),
        CommandIntent.Keybindings(KeybindingsIntent.ResetGlobalHotkey(action))
      )
    ) ++ EditorKeyAction.values.toList.map(action =>
      bindingInputItem(
        s"keymap-editor-${action.configKey}",
        keymapLabel(action.configKey),
        config.focusedKeymapConfig.editor.bindingsFor(action).map(_.render).reduceOption(_ + ", " + _),
        binding => CommandIntent.Keybindings(KeybindingsIntent.SetEditorKeyBinding(action, binding)),
        CommandIntent.Keybindings(KeybindingsIntent.ResetEditorKeyBinding(action))
      )
    ) ++ CommandRunnerKeyAction.values.toList.map(action =>
      bindingInputItem(
        s"keymap-command-runner-${action.configKey}",
        keymapLabel(action.configKey),
        config.focusedKeymapConfig.commandRunner.bindingsFor(action).map(_.render).reduceOption(_ + ", " + _),
        binding => CommandIntent.Keybindings(KeybindingsIntent.SetCommandRunnerKeyBinding(action, binding)),
        CommandIntent.Keybindings(KeybindingsIntent.ResetCommandRunnerKeyBinding(action))
      )
    ) ++ ModalKeyAction.values.toList.map(action =>
      bindingInputItem(
        s"keymap-modal-${action.configKey}",
        keymapLabel(action.configKey),
        config.focusedKeymapConfig.modal.bindingsFor(action).map(_.render).reduceOption(_ + ", " + _),
        binding => CommandIntent.Keybindings(KeybindingsIntent.SetModalKeyBinding(action, binding)),
        CommandIntent.Keybindings(KeybindingsIntent.ResetModalKeyBinding(action))
      )
    ) ++ PanelKeyAction.values.toList.map(action =>
      bindingInputItem(
        s"keymap-panel-${action.configKey}",
        keymapLabel(action.configKey),
        config.focusedKeymapConfig.panel.bindingsFor(action).map(_.render).reduceOption(_ + ", " + _),
        binding => CommandIntent.Keybindings(KeybindingsIntent.SetPanelKeyBinding(action, binding)),
        CommandIntent.Keybindings(KeybindingsIntent.ResetPanelKeyBinding(action))
      )
    ) ++ PeekKeyAction.values.toList.map(action =>
      bindingInputItem(
        s"keymap-peek-${action.configKey}",
        keymapLabel(action.configKey),
        config.focusedKeymapConfig.peek.bindingsFor(action).map(_.render).reduceOption(_ + ", " + _),
        binding => CommandIntent.Keybindings(KeybindingsIntent.SetPeekKeyBinding(action, binding)),
        CommandIntent.Keybindings(KeybindingsIntent.ResetPeekKeyBinding(action))
      )
    )
    items

  /** Settings › Keys: one section per place the keys apply, with the global hotkeys split again by what they are for.
    * `items` are [[buildKeymapInputItems]]'s rows, whose ids name their place (`keymap-panel-…`).
    */
  private[command] def keymapGroups(items: List[CommandSurfaceItem.InputItem]): List[CommandSurfaceItem.GroupItem] =
    def section(id: String, label: String, hint: String, children: List[CommandSurfaceItem]) =
      CommandSurfaceItem.GroupItem(s"settings-keymap-$id", label, children, CommandCategory.Settings, Some(hint))
    def rows(prefix: String) = items.filter(_.id.startsWith(s"keymap-$prefix-"))
    val globalRows           = rows("global")
    val global = HotkeyPurpose.values.toList.map { purpose =>
      val actionIds = HotkeyAction.values.toList.filter(_.purpose == purpose).map(a => s"keymap-global-${a.configKey}")
      section(
        s"global-${purpose.label.toLowerCase}",
        s"${purpose.label} Keys",
        s"${purpose.label} keys that work anywhere",
        actionIds.flatMap(id => globalRows.find(_.id == id))
      )
    }
    List(
      section("global", "Global Keys", "Keys that work anywhere in the app", global),
      section("editor", "Editor Keys", "While typing in a document", rows("editor")),
      section(
        "command-runner",
        "Command Runner Keys",
        "Inside the command palette and settings",
        rows("command-runner")
      ),
      section("dialogs", "Dialog Keys", "Inside prompts, pickers and other dialogs", rows("modal")),
      section("panels", "Panel Keys", "While a docked panel has focus", rows("panel")),
      section("peek", "Peek Keys", "While a peek is open", rows("peek"))
    )

  private def keymapLabel(configKey: String): String =
    configKey.split("_").toList.map(_.capitalize).mkString(" ")

  private def bindingInputItem(
    id: String,
    label: String,
    currentValue: Option[String],
    parse: String => CommandIntent,
    reset: CommandIntent
  ): CommandSurfaceItem.InputItem =
    CommandSurfaceItem.InputItem(
      id = id,
      label = label,
      hint = "Binding or default",
      currentValue = currentValue.getOrElse(""),
      kind = CommandSurfaceItem.InputKind.Binding,
      parse = text => parseBindingText(text, parse, reset),
      category = CommandCategory.Settings
    )

  private def parseBindingText(
    text: String,
    parse: String => CommandIntent,
    reset: CommandIntent
  ): Option[CommandIntent] =
    text.trim.toLowerCase match
      case "default" | "reset" => Some(reset)
      case binding             => HotkeyTrigger.parse(binding).map(_ => parse(binding))
