package com.serenity.state.manager

import cats.effect.IO
import com.serenity.command.PanelChromeIntent
import com.serenity.config.AppConfigMotionOps.withWheelScrollLines

/** The window-sitter, companion-sprite, visual-flair, wheel-scroll, and text-area/word-count half of the panel-chrome
  * settings dispatch. Split out of [[StateManagerConfigEffects]] (which keeps the toggles and the contextual-toolbar
  * case that needs the editor port) to keep that file under the architecture size target. Drives the host's shared
  * config-update helpers.
  */
private[manager] object StateManagerPanelWindowEffects:

  def interpret(host: StateManagerConfigEffects, intent: PanelChromeIntent): IO[Unit] =
    intent match
      case PanelChromeIntent.SetWindowSitterEnabled(enabled) =>
        host.updateWindowSitterConfig(_.copy(enabled = enabled))
      case PanelChromeIntent.SetWindowSitterAction(action) =>
        host.updateWindowSitterConfig(_.copy(action = action))
      case PanelChromeIntent.SetWindowSitterFrames(frames) =>
        host.updateWindowSitterConfig(_.copy(frames = frames))
      case PanelChromeIntent.SetWindowSitterActiveTicks(ticks) =>
        host.updateWindowSitterConfig(_.copy(activeTicks = ticks))
      case PanelChromeIntent.SetWindowSitterFastActiveTicks(ticks) =>
        host.updateWindowSitterConfig(_.copy(fastActiveTicks = ticks))
      case PanelChromeIntent.SetWindowSitterFastTypingThresholdMs(ms) =>
        host.updateWindowSitterConfig(_.copy(fastTypingThresholdMs = ms))
      case PanelChromeIntent.SetCompanionSpriteEnabled(enabled) =>
        host.updateCompanionSpriteConfig(_.copy(enabled = enabled))
      case PanelChromeIntent.SetVisualFlairLevel(level) =>
        host.updateVisualFlairLevel(level)
      case PanelChromeIntent.SetWheelScrollLines(lines) =>
        host.updateConfig(_.withWheelScrollLines(lines)).void
      case PanelChromeIntent.SetTextAreaLeftInset(value) =>
        host.updateTextDisplayConfig(_.withTextAreaLeftInset(value)).void
      case PanelChromeIntent.SetTextAreaRightInset(value) =>
        host.updateTextDisplayConfig(_.withTextAreaRightInset(value)).void
      case PanelChromeIntent.SetTextAreaTopInset(value) =>
        host.updateTextDisplayConfig(_.withTextAreaTopInset(value)).void
      case PanelChromeIntent.SetTextAreaBottomInset(value) =>
        host.updateTextDisplayConfig(_.withTextAreaBottomInset(value)).void
      case PanelChromeIntent.SetShowWordCount(enabled) =>
        host.updateTextDisplayConfig(_.withWordCount(enabled)).void
      case _ => IO.unit
