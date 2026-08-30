package com.serenity.command

import com.serenity.command.CommandSurfaceItem.CommandItem
import com.serenity.project.ProjectTaskKind
import com.serenity.richtext.{ParagraphAlignment, ParagraphRole}
import com.serenity.ui.layout.PanelPosition
import com.serenity.ui.presets.UiPreset

/** Registry of all available commands */
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

  /** Get all registered commands */
  def getAllCommands: List[Command] = commands

  /** Search commands by term */
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
          CommandRunnerSettingsItems.cursorModeOptionItem(optionSelections),
          CommandRunnerSettingsItems.backgroundStyleOptionItem(optionSelections),
          CommandRunnerSettingsItems.postProcessingOptionItem(optionSelections),
          CommandRunnerSettingsItems.uiShadowsOptionItem(optionSelections)
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
      CommandRunnerSettingsItems.cursorModeOptionItem(optionSelections),
      CommandRunnerSettingsItems.backgroundStyleOptionItem(optionSelections),
      CommandRunnerSettingsItems.postProcessingOptionItem(optionSelections),
      CommandRunnerSettingsItems.uiShadowsOptionItem(optionSelections)
    ).filter { item =>
      val lowerTerm = term.toLowerCase
      lowerTerm.isEmpty || item.searchText.toLowerCase.contains(lowerTerm)
    }

    (optionItems ++ commandItems).take(maxResults)

  /** Find a command by exact name */
  def findCommand(name: String): Option[Command] =
    commands.find(_.name == name)

object CommandRegistry:

  /** Create registry with custom commands */
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

  /** Default set of editor commands */
  private def defaultCommands: List[Command] = List(
    Command.typed(
      "open-settings",
      "Browse, search, inspect, and change application settings.",
      CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.OpenSettings)),
      CommandCategory.Settings,
      label = "Open Settings"
    ),
    Command.typed(
      "save",
      "Save the current file.",
      CommandIntent.File(FileIntent.SaveCurrentFile),
      CommandCategory.File,
      label = "Save"
    ),
    Command.typed(
      "save-as",
      "Save the current file under a new name.",
      CommandIntent.File(FileIntent.SaveCurrentFileAs),
      CommandCategory.File,
      label = "Save As"
    ),
    Command.typed(
      "save-config",
      "Write the current settings using the latest config format.",
      CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SaveConfig)),
      CommandCategory.Settings,
      label = "Save Config"
    ),
    Command.typed(
      "save-session",
      "Save the current editor session.",
      CommandIntent.Session(SessionIntent.SaveSession),
      CommandCategory.File,
      label = "Save Session"
    ),
    Command.typed(
      "restore-session",
      "Restore the last saved editor session.",
      CommandIntent.Session(SessionIntent.RestoreSession),
      CommandCategory.File,
      label = "Restore Session"
    ),
    Command.typed(
      "clear-session",
      "Clear the saved editor session.",
      CommandIntent.Session(SessionIntent.ClearSession),
      CommandCategory.File,
      label = "Clear Session"
    ),
    Command.typed(
      "open",
      "Open a file.",
      CommandIntent.File(FileIntent.OpenFile),
      CommandCategory.File,
      label = "Open File"
    ),
    Command.typed(
      "file-search",
      "Search for a file to open.",
      CommandIntent.File(FileIntent.OpenFileSearch),
      CommandCategory.File,
      label = "File Search"
    ),
    Command.typed(
      "quit",
      "Quit the application.",
      CommandIntent.Lifecycle(LifecycleIntent.QuitApp),
      CommandCategory.File,
      label = "Quit"
    ),
    Command.typed(
      "new",
      "Create a new file.",
      CommandIntent.File(FileIntent.NewFile),
      CommandCategory.File,
      label = "New File"
    ),
    Command.typed(
      "next-tab",
      "Switch to the next open file.",
      CommandIntent.View(ViewIntent.NextTab),
      CommandCategory.File,
      label = "Next Tab"
    ),
    Command.typed(
      "previous-tab",
      "Switch to the previous open file.",
      CommandIntent.View(ViewIntent.PreviousTab),
      CommandCategory.File,
      label = "Previous Tab"
    ),
    Command.typed(
      "close",
      "Close the current file.",
      CommandIntent.File(FileIntent.CloseCurrentFile),
      CommandCategory.File,
      label = "Close File"
    ),
    Command.typed(
      "close-all",
      "Close all files.",
      CommandIntent.File(FileIntent.CloseAll),
      CommandCategory.File,
      label = "Close All Files"
    ),
    Command.typed(
      "close-others",
      "Close every file except the current one.",
      CommandIntent.File(FileIntent.CloseOthers),
      CommandCategory.File,
      label = "Close Other Files"
    ),
    Command.typed(
      "find",
      "Find text in the current file.",
      CommandIntent.Edit(EditIntent.FindInCurrentFile),
      CommandCategory.Edit,
      label = "Find"
    ),
    Command.typed(
      "find-all",
      "Show every match for text in the current file.",
      CommandIntent.Edit(EditIntent.FindAllInCurrentFile),
      CommandCategory.Edit,
      label = "Find All"
    ),
    Command.typed(
      "replace",
      "Find and replace text in the current file.",
      CommandIntent.Edit(EditIntent.ReplaceInCurrentFile),
      CommandCategory.Edit,
      label = "Replace"
    ),
    Command.typed(
      "replace-all",
      "Replace every match in the current file or active selection.",
      CommandIntent.Edit(EditIntent.ReplaceAllInCurrentFile),
      CommandCategory.Edit,
      label = "Replace All"
    ),
    Command.typed(
      "copy",
      "Copy the active selection or current line.",
      CommandIntent.Edit(EditIntent.Copy),
      CommandCategory.Edit,
      label = "Copy"
    ),
    Command.typed(
      "cut",
      "Cut the active selection or current line.",
      CommandIntent.Edit(EditIntent.Cut),
      CommandCategory.Edit,
      label = "Cut"
    ),
    Command.typed(
      "paste",
      "Paste clipboard text at the cursor.",
      CommandIntent.Edit(EditIntent.Paste),
      CommandCategory.Edit,
      label = "Paste"
    ),
    Command.typed(
      "select-all",
      "Select all text in the current file.",
      CommandIntent.Edit(EditIntent.SelectAll),
      CommandCategory.Edit,
      label = "Select All"
    ),
    Command.typed(
      "undo",
      "Undo the most recent edit.",
      CommandIntent.Edit(EditIntent.Undo),
      CommandCategory.Edit,
      label = "Undo"
    ),
    Command.typed(
      "redo",
      "Redo the most recently undone edit.",
      CommandIntent.Edit(EditIntent.Redo),
      CommandCategory.Edit,
      label = "Redo"
    ),
    Command.typed(
      "bold",
      "Toggle bold formatting on the active selection.",
      CommandIntent.RichText(RichTextIntent.ToggleRichTextMark(com.serenity.richtext.InlineMark.Bold)),
      CommandCategory.Edit,
      label = "Bold"
    ),
    Command.typed(
      "italic",
      "Toggle italic formatting on the active selection.",
      CommandIntent.RichText(RichTextIntent.ToggleRichTextMark(com.serenity.richtext.InlineMark.Italic)),
      CommandCategory.Edit,
      label = "Italic"
    ),
    Command.typed(
      "underline",
      "Toggle underline formatting on the active selection.",
      CommandIntent.RichText(RichTextIntent.ToggleRichTextMark(com.serenity.richtext.InlineMark.Underline)),
      CommandCategory.Edit,
      label = "Underline"
    ),
    Command.typed(
      "paragraph-body",
      "Set the active paragraph to body text.",
      CommandIntent.RichText(RichTextIntent.SetRichTextParagraphRole(ParagraphRole.Body)),
      CommandCategory.Edit,
      label = "Body Text"
    ),
    Command.typed(
      "heading-1",
      "Set the active paragraph to heading level 1.",
      CommandIntent.RichText(RichTextIntent.SetRichTextParagraphRole(ParagraphRole.Heading(1))),
      CommandCategory.Edit,
      label = "Heading 1"
    ),
    Command.typed(
      "heading-2",
      "Set the active paragraph to heading level 2.",
      CommandIntent.RichText(RichTextIntent.SetRichTextParagraphRole(ParagraphRole.Heading(2))),
      CommandCategory.Edit,
      label = "Heading 2"
    ),
    Command.typed(
      "heading-3",
      "Set the active paragraph to heading level 3.",
      CommandIntent.RichText(RichTextIntent.SetRichTextParagraphRole(ParagraphRole.Heading(3))),
      CommandCategory.Edit,
      label = "Heading 3"
    ),
    Command.typed(
      "align-left",
      "Align the active paragraph to the left.",
      CommandIntent.RichText(RichTextIntent.SetRichTextParagraphAlignment(ParagraphAlignment.Left)),
      CommandCategory.Edit,
      label = "Align Left"
    ),
    Command.typed(
      "align-center",
      "Center the active paragraph.",
      CommandIntent.RichText(RichTextIntent.SetRichTextParagraphAlignment(ParagraphAlignment.Center)),
      CommandCategory.Edit,
      label = "Align Center"
    ),
    Command.typed(
      "align-right",
      "Align the active paragraph to the right.",
      CommandIntent.RichText(RichTextIntent.SetRichTextParagraphAlignment(ParagraphAlignment.Right)),
      CommandCategory.Edit,
      label = "Align Right"
    ),
    Command.typed(
      "align-justify",
      "Justify the active paragraph.",
      CommandIntent.RichText(RichTextIntent.SetRichTextParagraphAlignment(ParagraphAlignment.Justify)),
      CommandCategory.Edit,
      label = "Justify"
    ),
    Command.typed(
      "comment-lens",
      "Show or hide the rendered comment at the cursor.",
      CommandIntent.Comments(CommentsIntent.ToggleCommentLens),
      CommandCategory.View,
      label = "Comment Lens"
    ),
    Command.typed(
      "add-document-comment",
      "Add a document comment at the current cursor or selection.",
      CommandIntent.Comments(CommentsIntent.AddDocumentComment("Comment")),
      CommandCategory.Edit,
      label = "Add Document Comment"
    ),
    Command.typed(
      "delete-document-comment",
      "Delete the document comment at the current cursor.",
      CommandIntent.Comments(CommentsIntent.DeleteDocumentComment),
      CommandCategory.Edit,
      label = "Delete Document Comment"
    ),
    Command.typed(
      "goto-line",
      "Go to a specific line number.",
      CommandIntent.Navigation(NavigationIntent.OpenGotoLine),
      CommandCategory.Edit,
      label = "Go to Line"
    ),
    Command.typed(
      "toggle-bookmark",
      "Add or remove a bookmark at the current cursor.",
      CommandIntent.Navigation(NavigationIntent.ToggleBookmark),
      CommandCategory.View,
      label = "Toggle Bookmark"
    ),
    Command.typed(
      "next-bookmark",
      "Go to the next bookmark.",
      CommandIntent.Navigation(NavigationIntent.NextBookmark),
      CommandCategory.View,
      label = "Next Bookmark"
    ),
    Command.typed(
      "previous-bookmark",
      "Go to the previous bookmark.",
      CommandIntent.Navigation(NavigationIntent.PreviousBookmark),
      CommandCategory.View,
      label = "Previous Bookmark"
    ),
    Command.typed(
      "next-document-comment",
      "Go to the next document comment.",
      CommandIntent.Comments(CommentsIntent.NextDocumentComment),
      CommandCategory.View,
      label = "Next Document Comment"
    ),
    Command.typed(
      "previous-document-comment",
      "Go to the previous document comment.",
      CommandIntent.Comments(CommentsIntent.PreviousDocumentComment),
      CommandCategory.View,
      label = "Previous Document Comment"
    ),
    Command.typed(
      "navigate-back",
      "Go back to the previous document navigation point.",
      CommandIntent.Navigation(NavigationIntent.NavigateBack),
      CommandCategory.View,
      label = "Navigate Back"
    ),
    Command.typed(
      "navigate-forward",
      "Go forward to the next document navigation point.",
      CommandIntent.Navigation(NavigationIntent.NavigateForward),
      CommandCategory.View,
      label = "Navigate Forward"
    ),
    Command.typed(
      "next-document-symbol",
      "Go to the next document symbol.",
      CommandIntent.Navigation(NavigationIntent.NextDocumentSymbol),
      CommandCategory.View,
      label = "Next Document Symbol"
    ),
    Command.typed(
      "previous-document-symbol",
      "Go to the previous document symbol.",
      CommandIntent.Navigation(NavigationIntent.PreviousDocumentSymbol),
      CommandCategory.View,
      label = "Previous Document Symbol"
    ),
    Command.typed(
      "lsp-hover",
      "Show language-server hover information at the cursor.",
      CommandIntent.Lsp(LspIntent.RequestLspHover),
      CommandCategory.Edit,
      label = "LSP Hover"
    ),
    Command.typed(
      "lsp-completion",
      "Request language-server completion candidates at the cursor.",
      CommandIntent.Lsp(LspIntent.RequestLspCompletion),
      CommandCategory.Edit,
      label = "LSP Completion"
    ),
    Command.typed(
      "lsp-definition",
      "Request the symbol definition from the language server.",
      CommandIntent.Lsp(LspIntent.RequestLspDefinition),
      CommandCategory.Edit,
      label = "LSP Definition"
    ),
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
    ),
    Command.typed(
      "markdown-preview",
      "Open a rendered Markdown preview for the current buffer.",
      CommandIntent.View(ViewIntent.OpenMarkdownPreview),
      CommandCategory.View,
      label = "Open Markdown Preview"
    ),
    Command.typed(
      "markdown-view-source",
      "Show Markdown buffers as editable source.",
      CommandIntent.View(ViewIntent.SetMarkdownViewMode(com.serenity.config.MarkdownViewMode.Source)),
      CommandCategory.Settings,
      label = "Markdown View Source"
    ),
    Command.typed(
      "markdown-view-split",
      "Show Markdown source with a live side-by-side preview.",
      CommandIntent.View(ViewIntent.SetMarkdownViewMode(com.serenity.config.MarkdownViewMode.SplitPreview)),
      CommandCategory.Settings,
      label = "Markdown View Split"
    ),
    Command.typed(
      "markdown-view-inline-lens",
      "Show rendered Markdown with a raw-source editing lens at the cursor.",
      CommandIntent.View(ViewIntent.SetMarkdownViewMode(com.serenity.config.MarkdownViewMode.InlineLens)),
      CommandCategory.Settings,
      label = "Markdown View Inline Lens"
    ),
    Command.typed(
      "spellcheck-on",
      "Enable spell-checking for prose buffers.",
      CommandIntent.Settings(SettingsIntent.SpellCheck(SpellCheckIntent.SetSpellCheckEnabled(true))),
      CommandCategory.Settings,
      label = "Spell Check On"
    ),
    Command.typed(
      "spellcheck-off",
      "Disable spell-checking for prose buffers.",
      CommandIntent.Settings(SettingsIntent.SpellCheck(SpellCheckIntent.SetSpellCheckEnabled(false))),
      CommandCategory.Settings,
      label = "Spell Check Off"
    ),
    Command.typed(
      "focus-left-panel",
      "Focus the left pinned panel.",
      CommandIntent.View(ViewIntent.FocusPanel(PanelPosition.Left)),
      CommandCategory.View,
      label = "Focus Left Panel"
    ),
    Command.typed(
      "focus-right-panel",
      "Focus the right pinned panel.",
      CommandIntent.View(ViewIntent.FocusPanel(PanelPosition.Right)),
      CommandCategory.View,
      label = "Focus Right Panel"
    ),
    Command.typed(
      "focus-bottom-panel",
      "Focus the bottom pinned panel.",
      CommandIntent.View(ViewIntent.FocusPanel(PanelPosition.Bottom)),
      CommandCategory.View,
      label = "Focus Bottom Panel"
    ),
    Command.typed(
      "unpin-left-panel",
      "Unpin the left panel.",
      CommandIntent.View(ViewIntent.UnpinPanel(PanelPosition.Left)),
      CommandCategory.View,
      label = "Unpin Left Panel"
    ),
    Command.typed(
      "unpin-right-panel",
      "Unpin the right panel.",
      CommandIntent.View(ViewIntent.UnpinPanel(PanelPosition.Right)),
      CommandCategory.View,
      label = "Unpin Right Panel"
    ),
    Command.typed(
      "unpin-bottom-panel",
      "Unpin the bottom panel.",
      CommandIntent.View(ViewIntent.UnpinPanel(PanelPosition.Bottom)),
      CommandCategory.View,
      label = "Unpin Bottom Panel"
    ),
    Command.typed(
      "expand-left-panel",
      "Expand the left pinned panel into the editor workspace.",
      CommandIntent.View(ViewIntent.ExpandPanel(PanelPosition.Left)),
      CommandCategory.View,
      label = "Expand Left Panel"
    ),
    Command.typed(
      "expand-right-panel",
      "Expand the right pinned panel into the editor workspace.",
      CommandIntent.View(ViewIntent.ExpandPanel(PanelPosition.Right)),
      CommandCategory.View,
      label = "Expand Right Panel"
    ),
    Command.typed(
      "expand-bottom-panel",
      "Expand the bottom pinned panel into the editor workspace.",
      CommandIntent.View(ViewIntent.ExpandPanel(PanelPosition.Bottom)),
      CommandCategory.View,
      label = "Expand Bottom Panel"
    ),
    Command.typed(
      "collapse-expanded-panel",
      "Collapse the expanded panel back to its pinned position.",
      CommandIntent.View(ViewIntent.CollapseExpandedPanel),
      CommandCategory.View,
      label = "Collapse Expanded Panel"
    ),
    Command.typed(
      "project-build",
      "Build the detected project.",
      CommandIntent.Project(ProjectIntent.RunProjectTask(ProjectTaskKind.Build)),
      CommandCategory.Project,
      label = "Build Project"
    ),
    Command.typed(
      "project-test",
      "Run tests for the detected project.",
      CommandIntent.Project(ProjectIntent.RunProjectTask(ProjectTaskKind.Test)),
      CommandCategory.Project,
      label = "Test Project"
    ),
    Command.typed(
      "project-run",
      "Run the detected project.",
      CommandIntent.Project(ProjectIntent.RunProjectTask(ProjectTaskKind.Run)),
      CommandCategory.Project,
      label = "Run Project"
    ),
    Command.typed(
      "project-debug",
      "Launch the detected project through its debug task.",
      CommandIntent.Project(ProjectIntent.RunProjectTask(ProjectTaskKind.Debug)),
      CommandCategory.Project,
      label = "Run Debug Task"
    ),
    Command.typed(
      "project-dependencies",
      "Show or resolve dependencies for the detected project.",
      CommandIntent.Project(ProjectIntent.RunProjectTask(ProjectTaskKind.Dependencies)),
      CommandCategory.Project,
      label = "Project Dependencies"
    ),
    Command.typed(
      "project-cancel",
      "Cancel the running project task.",
      CommandIntent.Project(ProjectIntent.CancelProjectTask),
      CommandCategory.Project,
      label = "Cancel Project Task"
    )
  ) ++ builtInPresetCommands

  // NOTE (issue #1057, Stage 2 of #931): buffer-language switchers (`lang-plain-text`, `lang-<id>`) still need
  // registering here as ordinary commands -- deferred to the turn that also removes
  // `CommandRunnerSettingsItems.languageItems`/the "Current Buffer Language" settings group. Registering them
  // alongside that still-present settings-tree group gives two search targets sharing one id/name
  // (`CommandRunnerFloatingRenderingSpec`/`CommandRunnerReducerSpec` caught this: an exact-match command now outranks
  // the exact-match settings target it collides with, breaking "search settings, land on the settings leaf").
  // `CommandRunnerOneShotActionsSpec` documents this as an intentionally red spec until that turn.

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
