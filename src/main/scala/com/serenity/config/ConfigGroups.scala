package com.serenity.config

import com.serenity.lsp.config.LspUserConfig
import com.typesafe.config.ConfigUtil

/** The settings that are not one key to one value.
  *
  * The LSP, hotkey and keymap groups have as many keys as there are languages, actions and bindings. They cannot be
  * [[ConfigField]]s, but they are still settings that have to be written, read and covered -- so they are declared here
  * in the same shape, and the writer, the parser and the coverage tests treat a group exactly as they treat a field.
  */
object ConfigGroups:

  val dynamicPrefixes: List[String] = List("lsp.", "hotkey.", "keymap.")

  private val commandHotkeyPrefix = "hotkey.command."

  def lsp(config: LspUserConfig): List[(String, HoconValue)] =
    config.servers
      .getOrElse(Map.empty)
      .toList
      .sortBy(_._1)
      .flatMap {
        case (languageId, serverOverride) =>
          def key(field: String) = ConfigUtil.joinPath("lsp", languageId, field)
          List(
            serverOverride.enabled.map(enabled => key("enabled") -> HoconValue.boolean(enabled)),
            serverOverride.command.map(command => key("command") -> HoconValue.string(command)),
            serverOverride.args.map(args => key("args") -> HoconValue.list(args))
          ).flatten
      }

  def hotkeys(config: AppConfig): List[(String, HoconValue)] =
    HotkeyAction.values.toList.map { action =>
      ConfigUtil.joinPath("hotkey", action.configKey) ->
        HoconValue.list(config.inputConfig.hotkeyConfig.bindingsFor(action).map(_.render))
    }

  /** Registry command ids are lowercase words joined by hyphens, which HOCON takes unquoted -- so the id read back from
    * a key is the id that was written.
    */
  def commandHotkeys(config: AppConfig): List[(String, HoconValue)] =
    config.inputConfig.hotkeyConfig.commandBindings.toList.sortBy(_._1).map { (commandId, triggers) =>
      ConfigUtil.joinPath("hotkey", "command", commandId) -> HoconValue.list(triggers.map(_.render))
    }

  def commandIdOf(key: String): Option[String] =
    Option.when(key.startsWith(commandHotkeyPrefix))(key.stripPrefix(commandHotkeyPrefix)).filter(_.nonEmpty)

  def keymaps(config: AppConfig): List[(String, HoconValue)] =
    val keymap = config.inputConfig.focusedKeymapConfig

    def binding(bindings: List[HotkeyTrigger], defaults: List[HotkeyTrigger]): HoconValue =
      HoconValue.string(bindings.headOption.orElse(defaults.headOption).fold("")(_.render))

    def editor(action: EditorKeyAction): HoconValue =
      binding(keymap.editor.bindingsFor(action), EditorKeyAction.defaultBindings.getOrElse(action, Nil))

    List(
      "keymap.editor.page_down"              -> editor(EditorKeyAction.PageDown),
      "keymap.editor.extend_selection_left"  -> editor(EditorKeyAction.ExtendSelectionLeft),
      "keymap.editor.extend_selection_right" -> editor(EditorKeyAction.ExtendSelectionRight),
      "keymap.editor.extend_selection_up"    -> editor(EditorKeyAction.ExtendSelectionUp),
      "keymap.editor.extend_selection_down"  -> editor(EditorKeyAction.ExtendSelectionDown),
      "keymap.command_runner.submit" -> binding(
        keymap.commandRunner.bindingsFor(CommandRunnerKeyAction.Submit),
        CommandRunnerKeyAction.defaultBindings.getOrElse(CommandRunnerKeyAction.Submit, Nil)
      ),
      "keymap.modal.dismiss" -> binding(
        keymap.modal.bindingsFor(ModalKeyAction.Dismiss),
        ModalKeyAction.defaultBindings.getOrElse(ModalKeyAction.Dismiss, Nil)
      )
    )

  def handles(key: String): Boolean = dynamicPrefixes.exists(key.startsWith)
