package com.serenity.command

import com.serenity.command.CommandSurfaceItem.CommandItem
import com.serenity.lsp.config.LanguageId
import com.serenity.ui.presets.UiPreset

class CommandRegistry(private val commands: List[Command]):

  private val searcher = new CommandSearcher(commands)

  private lazy val commandsByCategory: Map[CommandCategory, List[Command]] =
    CommandCategory.values.map { category =>
      val categoryCommands =
        category match
          case CommandCategory.All => commands
          case _                   => commands.filter(_.category == category)
      category -> categoryCommands
    }.toMap

  def getAllCommands: List[Command] = commands

  def searchCommands(term: String, maxResults: Int = 5): List[Command] =
    searcher.search(term, maxResults)

  def commandsForCategory(category: CommandCategory): List[Command] =
    commandsByCategory(category)

  def surfaceItemsForCategory(
    category: CommandCategory,
    optionSelections: Map[String, Int] = Map.empty
  ): List[CommandSurfaceItem] =
    val commandItems =
      commandsForCategory(category)
        .map(CommandItem(_))

    val optionItems =
      if category == CommandCategory.Settings then
        List(
          CommandRunnerSettingsCursorItems.cursorModeOptionItem(optionSelections),
          CommandRunnerSettingsAppearanceItems.backgroundStyleOptionItem(optionSelections),
          CommandRunnerSettingsAppearanceItems.postProcessingOptionItem(optionSelections),
          CommandRunnerSettingsAppearanceItems.uiShadowsOptionItem(optionSelections)
        )
      else Nil

    optionItems ++ commandItems

  def searchSurfaceItems(
    term: String,
    optionSelections: Map[String, Int] = Map.empty,
    maxResults: Int = 50
  ): List[CommandSurfaceItem] =
    val commandItems = searchCommands(term, maxResults).map(CommandItem(_))
    val optionItems = List(
      CommandRunnerSettingsCursorItems.cursorModeOptionItem(optionSelections),
      CommandRunnerSettingsAppearanceItems.backgroundStyleOptionItem(optionSelections),
      CommandRunnerSettingsAppearanceItems.postProcessingOptionItem(optionSelections),
      CommandRunnerSettingsAppearanceItems.uiShadowsOptionItem(optionSelections)
    ).filter { item =>
      val lowerTerm = term.toLowerCase
      lowerTerm.isEmpty || item.searchText.toLowerCase.contains(lowerTerm)
    }

    (optionItems ++ commandItems).take(maxResults)

  /** Find a command by exact name */
  def findCommand(name: String): Option[Command] =
    commands.find(_.name == name)

object CommandRegistry:

  def apply(commands: List[Command]): CommandRegistry = new CommandRegistry(commands)

  /** Registry with default commands. The command list is static, so this is built once and reused. */
  lazy val default: CommandRegistry = new CommandRegistry(defaultCommands)

  /** Registry with default commands plus UI toggle commands. The command list is static, so this is built once and
    * reused.
    */
  lazy val withToggleUI: CommandRegistry = new CommandRegistry(defaultCommands ++ toggleUICommands)

  /** Pure typed UI toggle commands. */
  private def toggleUICommands: List[Command] = List(
    Command.typed(
      "toggle-line-numbers",
      "Show or hide line numbers.",
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleLineNumbers)),
      CommandCategory.View,
      label = "Toggle Line Numbers"
    ),
    Command.typed(
      "toggle-gutter",
      "Show or hide the status gutter.",
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleGutter)),
      CommandCategory.View,
      label = "Toggle Gutter"
    ),
    Command.typed(
      "toggle-pane-headers",
      "Show or hide pane header bars.",
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.TogglePaneHeaders)),
      CommandCategory.View,
      label = "Toggle Pane Headers"
    ),
    Command.typed(
      "toggle-visual-line-navigation",
      "Move Up/Down by visual row instead of jumping straight to the previous/next logical line under word wrap.",
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleVisualLineCursorNavigation)),
      CommandCategory.View,
      label = "Toggle Visual Line Navigation"
    ),
    Command.typed(
      "toggle-typewriter-scrolling",
      "Keep the cursor's line vertically centred as you type, padding past the document's end.",
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleTypewriterScrolling)),
      CommandCategory.View,
      label = "Toggle Typewriter Scrolling"
    ),
    Command.typed(
      "toggle-line-wrap",
      "Soft-wrap long logical lines to the editor width.",
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleWordWrap)),
      CommandCategory.View,
      label = "Toggle Line Wrap"
    ),
    Command.typed(
      "toggle-word-wrap",
      "Wrap long logical lines to the editor width.",
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleWordWrap)),
      CommandCategory.View,
      label = "Toggle Word Wrap"
    ),
    Command.typed(
      "toggle-text-body-focus",
      "Dim text outside the current paragraph or code block.",
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleFocusedTextBody)),
      CommandCategory.View,
      label = "Toggle Text Body Focus"
    ),
    Command.typed(
      "toggle-contextual-toolbar",
      "Show or hide the floating rich-text formatting toolbar near the cursor.",
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleContextualToolbar)),
      CommandCategory.View,
      label = "Toggle Contextual Toolbar"
    )
  )

  /** Default set of editor commands.
    *
    * Split by domain across `CommandRegistry*Commands` objects in this package to keep both this method and each split
    * file under the architecture size targets -- the concatenation order below is the exact order the commands appeared
    * in before the split, so behavior (search ranking, palette default ordering) is unaffected.
    */
  private def defaultCommands: List[Command] =
    CommandRegistryFileCommands.fileCommands ++
      CommandRegistryFileCommands.sessionAndTabCommands ++
      CommandRegistryEditCommands.editCommands ++
      CommandRegistryEditCommands.richTextCommands ++
      CommandRegistryNavigationCommands.commentAndBookmarkCommands ++
      CommandRegistryNavigationCommands.navigationAndLspCommands ++
      CommandRegistryViewSettingsCommands.themeAndViewCommands ++
      CommandRegistryViewSettingsCommands.markdownAndModeCommands ++
      CommandRegistryPanelProjectCommands.panelFocusCommands ++
      CommandRegistryPanelProjectCommands.paneCommands ++
      CommandRegistryPanelProjectCommands.projectCommands ++
      builtInPresetCommands ++ languageCommands

  /** Buffer-language switchers -- previously only reachable as fake "settings" under the "Current Buffer Language"
    * settings group (`CommandRunnerSettingsItems.languageItems`), even though picking one is a one-shot action with no
    * persisted value of its own (issue #1057). Registered here, in the same commit that removes that settings-tree
    * group (`CommandRunnerSettingsGroups.build`), so an exact-match command by this id/name is never simultaneously an
    * exact-match settings-search target too -- that collision (two things named "lang-markdown") is what broke
    * `CommandRunnerFloatingRenderingSpec`/`CommandRunnerReducerSpec` the first time this was tried standalone.
    */
  private def languageCommands: List[Command] =
    Command.typed(
      "lang-plain-text",
      "Use plain text mode for the current buffer.",
      CommandIntent.File(FileIntent.SetBufferLanguage(None)),
      CommandCategory.Settings,
      label = "Plain Text"
    ) :: LanguageId.values.toList.sortBy(_.displayName).map { lang =>
      Command.typed(
        s"lang-${lang.id}",
        s"Use ${lang.displayName} mode for the current buffer.",
        CommandIntent.File(FileIntent.SetBufferLanguage(Some(lang))),
        CommandCategory.Settings,
        label = lang.displayName
      )
    }

  private def builtInPresetCommands: List[Command] =
    UiPreset.builtIns.map { preset =>
      val preview = UiPreset.Preview.fromPreset(preset)
      Command.typed(
        name = s"apply-${preset.name.toLowerCase.replace(' ', '-')}-preset",
        description = s"Apply the ${preset.name} workspace preset: ${preview.hint}.",
        intent = CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset(preset.name)),
        category = CommandCategory.Settings,
        label = s"Apply ${preset.name} Preset"
      )
    }
