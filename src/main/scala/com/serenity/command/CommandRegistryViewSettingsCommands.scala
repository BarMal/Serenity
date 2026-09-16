package com.serenity.command

/** Theme and view-toggle commands. Split out of `CommandRegistry.defaultCommands` to keep both under the architecture
  * size targets -- see that method's doc.
  */
private[command] object CommandRegistryViewSettingsCommands:

  private[command] def themeAndViewCommands: List[Command] = List(
    Command.typed(
      "toggle-theme",
      "Switch between the light and dark themes.",
      CommandIntent.Theme(ThemeIntent.ToggleTheme),
      CommandCategory.Settings,
      label = "Toggle Theme"
    ),
    Command.typed(
      "reload-theme",
      "Reload the current theme configuration.",
      CommandIntent.Theme(ThemeIntent.ReloadTheme),
      CommandCategory.Settings,
      label = "Reload Theme"
    ),
    Command.typed(
      "theme-chooser",
      "Choose a theme with live preview.",
      CommandIntent.Theme(ThemeIntent.OpenThemeChooser),
      CommandCategory.Settings,
      label = "Open Theme Chooser"
    ),
    Command.typed(
      "theme-creator",
      "Create and save a custom theme with live colour previews.",
      CommandIntent.Theme(ThemeIntent.OpenThemeCreator),
      CommandCategory.Settings,
      label = "Open Theme Creator"
    ),
    Command.typed(
      "export-theme",
      "Export the current theme to a theme config file.",
      CommandIntent.Theme(ThemeIntent.ExportCurrentTheme),
      CommandCategory.Settings,
      label = "Export Current Theme"
    ),
    Command.typed(
      "reload-themes",
      "Reload available themes from disk.",
      CommandIntent.Theme(ThemeIntent.ReloadThemes),
      CommandCategory.Settings,
      label = "Reload Theme List"
    ),
    Command.typed(
      "format",
      "Format the current file.",
      CommandIntent.Edit(EditIntent.FormatCurrentFile),
      CommandCategory.Edit,
      label = "Format File"
    ),
    Command.typed(
      "pin-explorer",
      "Pin the explorer panel on the left.",
      CommandIntent.View(ViewIntent.PinExplorerPanel),
      CommandCategory.View,
      label = "Pin Explorer Panel"
    ),
    Command.typed(
      "pin-outline",
      "Pin the outline panel on the right.",
      CommandIntent.View(ViewIntent.PinOutlinePanel),
      CommandCategory.View,
      label = "Pin Outline Panel"
    ),
    Command.typed(
      "pin-comments",
      "Pin the comments panel on the right.",
      CommandIntent.View(ViewIntent.PinCommentsPanel),
      CommandCategory.View,
      label = "Pin Comments Panel"
    ),
    Command.typed(
      "pin-diagnostics",
      "Pin the diagnostics panel at the bottom.",
      CommandIntent.View(ViewIntent.PinDiagnosticsPanel),
      CommandCategory.View,
      label = "Pin Diagnostics Panel"
    )
  )

  private[command] def markdownAndModeCommands: List[Command] = List(
    Command.typed(
      "toggle-shortcuts-help",
      "Show or hide a reference of the app's current keyboard shortcuts.",
      CommandIntent.View(ViewIntent.ToggleShortcutsHelp),
      CommandCategory.View,
      label = "Toggle Shortcuts Help"
    ),
    Command.typed(
      "toggle-tab-list",
      "Show or hide the list of open tabs.",
      CommandIntent.View(ViewIntent.ToggleTabList),
      CommandCategory.View,
      label = "Tab List"
    ),
    Command.typed(
      "toggle-recent-in-mode",
      "Show or hide recent files opened in the current app mode.",
      CommandIntent.View(ViewIntent.ToggleRecentFilesInMode),
      CommandCategory.View,
      label = "Recent in This Mode"
    ),
    Command.typed(
      "markdown-preview",
      "Open a rendered Markdown preview for the current buffer.",
      CommandIntent.View(ViewIntent.OpenMarkdownPreview),
      CommandCategory.View,
      label = "Open Markdown Preview"
    )
  )
