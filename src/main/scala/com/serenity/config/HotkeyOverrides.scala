package com.serenity.config

/** What `config.conf` records of the hotkeys: only where they differ from the platform defaults.
  *
  * Writing every binding pinned each one to the defaults of the build that last saved the file, so a default added
  * later (Ctrl+Shift+Z for redo) never reached anyone who had saved once. An empty list is kept, because it is how a
  * default is unbound.
  */
object HotkeyOverrides:

  def runningOs: String = System.getProperty("os.name", "")

  def actions(config: HotkeyConfig, osName: String): List[(HotkeyAction, List[HotkeyTrigger])] =
    val defaults = HotkeyConfig.platformDefaults(osName)
    HotkeyAction.values.toList
      .map(action => action -> config.bindingsFor(action))
      .filter((action, triggers) => triggers != defaults.getOrElse(action, Nil))

  def commands(config: HotkeyConfig, osName: String): List[(String, List[HotkeyTrigger])] =
    val defaults = HotkeyConfig.defaultCommandBindingsFor(osName)
    (defaults.keySet ++ config.commandBindings.keySet).toList.sorted
      .map(commandId => commandId -> config.commandBindingsFor(commandId))
      .filter((commandId, triggers) => triggers != defaults.getOrElse(commandId, Nil))
