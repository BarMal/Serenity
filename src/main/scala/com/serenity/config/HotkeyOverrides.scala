package com.serenity.config

/** What `config.conf` records of the hotkeys: only where they differ from the platform defaults.
  *
  * Writing every binding pinned each one to the defaults of the build that last saved the file, so a default added
  * later (Ctrl+Shift+Z for redo) never reached anyone who had saved once. An empty list is kept, because it is how a
  * default is unbound.
  */
object HotkeyOverrides:

  def runningOs: String = System.getProperty("os.name", "")

  /** The defaults `config` is measured against: the platform's, or what the terminal makes of them when `config` is the
    * one `forTerminalUse` rewrote. A terminal on macOS holds Ctrl where the platform default is Cmd, and a save of that
    * config would otherwise record every such action as the user's own choice and pin it, GUI included.
    */
  def defaultsFor(config: HotkeyConfig, osName: String): HotkeyConfig =
    val platform = HotkeyConfig.forOs(osName)
    if config.terminalAdjusted then platform.forTerminalUse else platform

  /** `loaded` read the way `live` is, so the two are compared on the same footing. */
  def comparableTo(live: AppConfig, loaded: AppConfig): AppConfig =
    if live.inputConfig.hotkeyConfig.terminalAdjusted then
      loaded.withHotkeyConfig(loaded.inputConfig.hotkeyConfig.forTerminalUse)
    else loaded

  def actions(config: HotkeyConfig, osName: String): List[(HotkeyAction, List[HotkeyTrigger])] =
    val defaults = defaultsFor(config, osName).bindings
    HotkeyAction.values.toList
      .map(action => action -> config.bindingsFor(action))
      .filter((action, triggers) => triggers != defaults.getOrElse(action, Nil))

  def commands(config: HotkeyConfig, osName: String): List[(String, List[HotkeyTrigger])] =
    val defaults = defaultsFor(config, osName).commandBindings
    (defaults.keySet ++ config.commandBindings.keySet).toList.sorted
      .map(commandId => commandId -> config.commandBindingsFor(commandId))
      .filter((commandId, triggers) => triggers != defaults.getOrElse(commandId, Nil))
