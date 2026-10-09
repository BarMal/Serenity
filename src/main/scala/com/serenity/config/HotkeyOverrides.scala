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

  /** `other` with its hotkeys read the way `running` holds its own: a terminal-adjusted config keeps the terminal's
    * rewrite of whatever is applied to it (a loaded file, a preset, the shipped defaults), so Cmd bindings a terminal
    * cannot receive never come back and the adjustment is not lost.
    */
  def likeRunning(running: AppConfig, other: AppConfig): AppConfig =
    if running.inputConfig.hotkeyConfig.terminalAdjusted then
      other.withHotkeyConfig(other.inputConfig.hotkeyConfig.forTerminalUse)
    else other

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
