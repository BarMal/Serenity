package com.serenity.config

import scala.concurrent.duration.{DurationLong, FiniteDuration}

/** When a file-backed buffer's unsaved changes are written to the file itself, after VS Code's `files.autoSave`.
  *
  * `Off` is the default: nothing is written until the user saves.
  */
enum AutoSaveMode(val configKey: String):
  case Off            extends AutoSaveMode("off")
  case AfterDelay     extends AutoSaveMode("after-delay")
  case OnFocusChange  extends AutoSaveMode("on-focus-change")
  case OnWindowChange extends AutoSaveMode("on-window-change")

object AutoSaveMode:

  def fromConfigKey(value: String): Option[AutoSaveMode] =
    value.trim.toLowerCase match
      case "off" | "false" | "none" | "disabled"                      => Some(Off)
      case "after-delay" | "after_delay" | "afterdelay" | "delay"     => Some(AfterDelay)
      case "on-focus-change" | "on_focus_change" | "onfocuschange"    => Some(OnFocusChange)
      case "on-window-change" | "on_window_change" | "onwindowchange" => Some(OnWindowChange)
      case _                                                          => None

/** `delayMillis` is the pause after the last edit that [[AutoSaveMode.AfterDelay]] waits out; the other modes ignore
  * it.
  */
final case class AutoSaveConfig(
    mode: AutoSaveMode = AutoSaveMode.Off,
    delayMillis: Long = AutoSaveConfig.DefaultDelayMillis
):

  def delay: FiniteDuration = delayMillis.millis

object AutoSaveConfig:

  val DefaultDelayMillis: Long = 1000L

  /** Below this a save would start while the user is still typing, so the setting is refused rather than clamped. */
  val MinDelayMillis: Long = 100L
