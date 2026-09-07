package com.serenity.command

import com.serenity.config.*

/** Keybinding input items for every focused keymap and global hotkey. Split out of
  * `CommandRunnerSettingsInputItems.build` to keep both under the architecture size targets -- see that
  * object's doc.
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
    val primaryIds = List(
      "keymap-global-command_palette",
      "keymap-global-file_search",
      "keymap-editor-page_down",
      "keymap-command-runner-submit",
      "keymap-modal-dismiss",
      "keymap-panel-activate",
      "keymap-peek-accept"
    )
    primaryIds.flatMap(id => items.find(_.id == id)) ++ items.filterNot(item => primaryIds.contains(item.id))

  private def formatDecimal(value: Double): String =
    if value.isWhole then value.toLong.toString else value.toString

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
      isDecimal = false,
      parse = text => parseBindingText(text, parse, reset),
      category = CommandCategory.Settings,
      acceptsBindingText = true
    )

  private def parseBindingText(
    text: String,
    parse: String => CommandIntent,
    reset: CommandIntent
  ): Option[CommandIntent] =
    text.trim.toLowerCase match
      case "default" | "reset" => Some(reset)
      case binding             => HotkeyTrigger.parse(binding).map(_ => parse(binding))
