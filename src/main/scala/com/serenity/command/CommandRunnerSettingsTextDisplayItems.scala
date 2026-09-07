package com.serenity.command

import com.serenity.config.*

/** Editor text-display settings items (line numbers, gutter, wrap, contextual toolbar). Split out of
  * `CommandRunnerSettingsItems` to keep both under the architecture size targets -- see that object's doc.
  */
private[command] object CommandRunnerSettingsTextDisplayItems:

  private[command] def lineNumbersOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandRunnerSettingsItems.enabledOptionItem(
      id = "line-numbers",
      label = "Line Numbers",
      selectedIndex = optionSelections.getOrElse("line-numbers", 0),
      enabledIntent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetLineNumbers(true))),
      disabledIntent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetLineNumbers(false))),
      hint = "Show or hide line numbers"
    )

  private[command] def wordCountOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandRunnerSettingsItems.enabledOptionItem(
      id = "show-word-count",
      label = "Word Count",
      selectedIndex = optionSelections.getOrElse("show-word-count", 1),
      enabledIntent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetShowWordCount(true))),
      disabledIntent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetShowWordCount(false))),
      hint = "Show word count, character count, and reading time in the status bar"
    )

  private[command] def gutterOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandRunnerSettingsItems.enabledOptionItem(
      id = "gutter",
      label = "Gutter",
      selectedIndex = optionSelections.getOrElse("gutter", 0),
      enabledIntent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetGutter(true))),
      disabledIntent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetGutter(false))),
      hint = "Show or hide the status gutter"
    )

  private[command] def lineWrapOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandRunnerSettingsItems.enabledOptionItem(
      id = "line-wrap",
      label = "Line Wrap",
      selectedIndex = optionSelections.getOrElse("line-wrap", optionSelections.getOrElse("word-wrap", 0)),
      enabledIntent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetWordWrap(true))),
      disabledIntent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetWordWrap(false))),
      hint = "Wrap long logical lines to the editor width"
    )

  private[command] def visualLineNavigationOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandRunnerSettingsItems.enabledOptionItem(
      id = "visual-line-navigation",
      label = "Visual Line Navigation",
      selectedIndex = optionSelections.getOrElse("visual-line-navigation", 0),
      enabledIntent =
        CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetVisualLineCursorNavigation(true))),
      disabledIntent =
        CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetVisualLineCursorNavigation(false))),
      hint = "Move Up/Down by wrapped visual row instead of logical line"
    )

  private[command] def typewriterScrollingOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandRunnerSettingsItems.enabledOptionItem(
      id = "typewriter-scrolling",
      label = "Typewriter Scrolling",
      selectedIndex = optionSelections.getOrElse("typewriter-scrolling", 1),
      enabledIntent =
        CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetTypewriterScrolling(true))),
      disabledIntent =
        CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetTypewriterScrolling(false))),
      hint = "Keep the cursor's line vertically centred as you type, padding past the document's end"
    )

  private[command] def focusedTextBodyOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandRunnerSettingsItems.enabledOptionItem(
      id = "focused-text-body",
      label = "Text Body Focus",
      selectedIndex = optionSelections.getOrElse("focused-text-body", 1),
      enabledIntent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetFocusedTextBody(true))),
      disabledIntent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetFocusedTextBody(false))),
      hint = "Dim text outside the active body"
    )

  private[command] def contextualToolbarOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandRunnerSettingsItems.enabledOptionItem(
      id = "contextual-toolbar",
      label = "Contextual Toolbar",
      selectedIndex = optionSelections.getOrElse("contextual-toolbar", 0),
      enabledIntent =
        CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetContextualToolbarEnabled(true))),
      disabledIntent =
        CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetContextualToolbarEnabled(false))),
      hint = "Show the floating rich-text toolbar near the cursor"
    )

  private[command] def contextualToolbarDisplayModeOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "contextual-toolbar-display",
      label = "Contextual Toolbar Style",
      options = List(
        CommandOption(
          "Icon Only",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(PanelChromeIntent.SetContextualToolbarDisplayMode(ToolbarDisplayMode.IconOnly))
          )
        ),
        CommandOption(
          "Text Only",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(PanelChromeIntent.SetContextualToolbarDisplayMode(ToolbarDisplayMode.TextOnly))
          )
        ),
        CommandOption(
          "Icon + Text",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(
              PanelChromeIntent.SetContextualToolbarDisplayMode(ToolbarDisplayMode.IconAndText)
            )
          )
        )
      ),
      selectedIndex = optionSelections.getOrElse("contextual-toolbar-display", 2),
      category = CommandCategory.Settings,
      hint = Some("Toolbar labels as icons, text, or both")
    )

  private[command] def commandRunnerKeyHintsOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandRunnerSettingsItems.enabledOptionItem(
      id = "command-runner-key-hints",
      label = "Command Runner Key Hints",
      selectedIndex = optionSelections.getOrElse("command-runner-key-hints", 0),
      enabledIntent =
        CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetCommandRunnerShowKeyHints(true))),
      disabledIntent =
        CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetCommandRunnerShowKeyHints(false))),
      hint = "Persistent key-binding footer in the palette and settings surface"
    )
