package com.serenity.state.models

import com.serenity.config.{AppConfig, EditorKeyAction, HotkeyAction, HotkeyConfig, HotkeyTrigger}

/** Builds the toggleable keyboard-shortcuts reference (issue #1247) from the app's actual, currently-configured
  * bindings -- never a hardcoded list, so it cannot drift from what a user has customized via `HotkeyConfig` /
  * `FocusedKeymapConfig`.
  *
  * Scoped to the two groups a user reaches for while actually editing: the global hotkeys (save, undo, find, tab
  * management, ...) and the editor's own movement/selection/editing keymap. The command runner, modal, panel and peek
  * keymap groups are deliberately left out of this first version -- each of those surfaces already shows its own
  * current-state hint via the `commandRunnerShowKeyHints` mechanism (issue #931, Stage 3) while it is open, so
  * duplicating them here would mostly repeat what is already on screen at the moment they matter.
  */
object ShortcutsHelpContent:

  def build(config: AppConfig): List[ShortcutHelpGroup] =
    List(
      ShortcutHelpGroup("Global", globalEntries(config.inputConfig.hotkeyConfig)),
      ShortcutHelpGroup("Editor", editorEntries(config.inputConfig.focusedKeymapConfig.editor.bindings))
    ).filter(_.entries.nonEmpty)

  private def globalEntries(hotkeyConfig: HotkeyConfig): List[ShortcutHelpEntry] =
    HotkeyAction.values.toList.flatMap(action => entryFor(action.configKey, hotkeyConfig.bindingsFor(action)))

  private def editorEntries(bindings: Map[EditorKeyAction, List[HotkeyTrigger]]): List[ShortcutHelpEntry] =
    EditorKeyAction.values.toList.flatMap(action => entryFor(action.configKey, bindings.getOrElse(action, Nil)))

  private def entryFor(configKey: String, triggers: List[HotkeyTrigger]): Option[ShortcutHelpEntry] =
    Option.when(triggers.nonEmpty)(ShortcutHelpEntry(displayLabel(configKey), triggers.map(_.render).mkString(" / ")))

  private def displayLabel(configKey: String): String =
    configKey.split("_").filter(_.nonEmpty).map(_.capitalize).mkString(" ")
