package com.serenity.command.menu

import com.serenity.command.{CommandIntent, SettingsIntent, StatusLineIntent, TextDisplayIntent}
import com.serenity.state.models.AppState

object CommandToggleState:

  /** Whether a toggle command's setting is on, or `None` for a command that does not toggle one. */
  def of(intent: CommandIntent, app: AppState): Option[Boolean] =
    intent match
      case CommandIntent.Settings(SettingsIntent.TextDisplay(display)) => textDisplay(display, app)
      case CommandIntent.Settings(SettingsIntent.StatusLine(StatusLineIntent.ToggleVisibility)) =>
        Some(app.persisted.config.statusLine.isShown)
      case _ => None

  // Exhaustive, so a toggle added to TextDisplayIntent has to say where its state is read from.
  private def textDisplay(intent: TextDisplayIntent, app: AppState): Option[Boolean] =
    val surface = app.persisted.config.surfaceConfig
    intent match
      case TextDisplayIntent.ToggleLineNumbers                => Some(surface.showLineNumbers)
      case TextDisplayIntent.ToggleWordWrap                   => Some(surface.wordWrapEnabled)
      case TextDisplayIntent.ToggleFocusedTextBody            => Some(surface.focusedTextBodyEnabled)
      case TextDisplayIntent.ToggleContextualToolbar          => Some(surface.contextualToolbarEnabled)
      case TextDisplayIntent.TogglePaneHeaders                => Some(surface.showPaneHeaders)
      case TextDisplayIntent.ToggleVisualLineCursorNavigation => Some(surface.visualLineCursorNavigation)
      case TextDisplayIntent.ToggleTypewriterScrolling        => Some(surface.typewriterScrollingEnabled)
      case TextDisplayIntent.ToggleColumnMode                 => Some(surface.columnModeEnabled)
      case TextDisplayIntent.SetLineNumbers(_) | TextDisplayIntent.SetLineNumberSide(_) |
          TextDisplayIntent.SetLineNumberMarginLeft(_) | TextDisplayIntent.SetLineNumberMarginRight(_) |
          TextDisplayIntent.SetLineNumberPadding(_) | TextDisplayIntent.SetWordWrap(_) |
          TextDisplayIntent.SetVisualLineCursorNavigation(_) | TextDisplayIntent.SetTypewriterScrolling(_) |
          TextDisplayIntent.SetColumnMode(_) | TextDisplayIntent.SetColumnTargetWidth(_) |
          TextDisplayIntent.SetColumnGap(_) | TextDisplayIntent.SetColumnCount(_) |
          TextDisplayIntent.SetFocusedTextBody(_) | TextDisplayIntent.SetContextualToolbarEnabled(_) |
          TextDisplayIntent.SetContextualToolbarDisplayMode(_) | TextDisplayIntent.SetTextAreaLeftInset(_) |
          TextDisplayIntent.SetTextAreaRightInset(_) | TextDisplayIntent.SetTextAreaTopInset(_) |
          TextDisplayIntent.SetTextAreaBottomInset(_) | TextDisplayIntent.SetDropCapsEnabled(_) =>
        None
