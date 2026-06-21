package com.serenity.command

import com.serenity.command.CommandSurfaceItem.CommandItem
import com.serenity.project.ProjectTaskKind
import com.serenity.richtext.{ParagraphAlignment, ParagraphRole}
import com.serenity.state.manager.StateManager
import com.serenity.ui.layout.PanelPosition
import com.serenity.ui.presets.UiPreset

/** Registry of all available commands */
class CommandRegistry(private val commands: List[Command]):

  private val searcher = new CommandSearcher(commands)

  /** Get all registered commands */
  def getAllCommands: List[Command] = commands

  /** Search commands by term */
  def searchCommands(term: String, maxResults: Int = 5): List[Command] =
    searcher.search(term, maxResults)

  def commandsForCategory(category: CommandCategory): List[Command] =
    category match
      case CommandCategory.All => commands
      case _                   => commands.filter(_.category == category)

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
          CommandRunner.animationOptionItem(optionSelections),
          CommandRunner.cursorModeOptionItem(optionSelections),
          CommandRunner.backgroundStyleOptionItem(optionSelections)
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
      CommandRunner.animationOptionItem(optionSelections),
      CommandRunner.cursorModeOptionItem(optionSelections),
      CommandRunner.backgroundStyleOptionItem(optionSelections)
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

  /** Create registry with default commands */
  def default: CommandRegistry = new CommandRegistry(defaultCommands)

  /** Create registry with default commands plus UI toggle commands */
  def withToggleUI: CommandRegistry = new CommandRegistry(defaultCommands ++ toggleUICommands)

  /** Compatibility alias during the reducer migration. */
  def withToggleUIStateful(stateManager: StateManager): CommandRegistry =
    withToggleUI

  /** Pure typed UI toggle commands. */
  private def toggleUICommands: List[Command] = List(
    Command.typed(
      "toggle-line-numbers",
      "Show or hide line numbers.",
      CommandIntent.ToggleLineNumbers,
      CommandCategory.View,
      label = "Toggle Line Numbers"
    ),
    Command.typed(
      "toggle-gutter",
      "Show or hide the status gutter.",
      CommandIntent.ToggleGutter,
      CommandCategory.View,
      label = "Toggle Gutter"
    )
  )

  /** Default set of editor commands */
  private def defaultCommands: List[Command] = List(
    Command.typed(
      "save",
      "Save the current file.",
      CommandIntent.SaveCurrentFile,
      CommandCategory.File,
      label = "Save"
    ),
    Command.typed(
      "save-as",
      "Save the current file under a new name.",
      CommandIntent.SaveCurrentFileAs,
      CommandCategory.File,
      label = "Save As"
    ),
    Command.typed(
      "save-config",
      "Write the current settings using the latest config format.",
      CommandIntent.SaveConfig,
      CommandCategory.Settings,
      label = "Save Config"
    ),
    Command.typed(
      "save-session",
      "Save the current editor session.",
      CommandIntent.SaveSession,
      CommandCategory.File,
      label = "Save Session"
    ),
    Command.typed(
      "restore-session",
      "Restore the last saved editor session.",
      CommandIntent.RestoreSession,
      CommandCategory.File,
      label = "Restore Session"
    ),
    Command.typed(
      "clear-session",
      "Clear the saved editor session.",
      CommandIntent.ClearSession,
      CommandCategory.File,
      label = "Clear Session"
    ),
    Command.typed(
      "open",
      "Open a file.",
      CommandIntent.OpenFile,
      CommandCategory.File,
      label = "Open File"
    ),
    Command.typed(
      "quit",
      "Quit the application.",
      CommandIntent.QuitApp,
      CommandCategory.File,
      label = "Quit"
    ),
    Command.typed(
      "new",
      "Create a new file.",
      CommandIntent.NewFile,
      CommandCategory.File,
      label = "New File"
    ),
    Command.typed(
      "close",
      "Close the current file.",
      CommandIntent.CloseCurrentFile,
      CommandCategory.File,
      label = "Close File"
    ),
    Command.typed(
      "close-all",
      "Close all files.",
      CommandIntent.CloseAll,
      CommandCategory.File,
      label = "Close All Files"
    ),
    Command.typed(
      "close-others",
      "Close every file except the current one.",
      CommandIntent.CloseOthers,
      CommandCategory.File,
      label = "Close Other Files"
    ),
    Command.typed(
      "find",
      "Find text in the current file.",
      CommandIntent.FindInCurrentFile,
      CommandCategory.Edit,
      label = "Find"
    ),
    Command.typed(
      "find-all",
      "Show every match for text in the current file.",
      CommandIntent.FindAllInCurrentFile,
      CommandCategory.Edit,
      label = "Find All"
    ),
    Command.typed(
      "replace",
      "Find and replace text in the current file.",
      CommandIntent.ReplaceInCurrentFile,
      CommandCategory.Edit,
      label = "Replace"
    ),
    Command.typed(
      "replace-all",
      "Replace every match in the current file or active selection.",
      CommandIntent.ReplaceAllInCurrentFile,
      CommandCategory.Edit,
      label = "Replace All"
    ),
    Command.typed(
      "copy",
      "Copy the active selection or current line.",
      CommandIntent.Copy,
      CommandCategory.Edit,
      label = "Copy"
    ),
    Command.typed(
      "cut",
      "Cut the active selection or current line.",
      CommandIntent.Cut,
      CommandCategory.Edit,
      label = "Cut"
    ),
    Command.typed(
      "paste",
      "Paste clipboard text at the cursor.",
      CommandIntent.Paste,
      CommandCategory.Edit,
      label = "Paste"
    ),
    Command.typed(
      "select-all",
      "Select all text in the current file.",
      CommandIntent.SelectAll,
      CommandCategory.Edit,
      label = "Select All"
    ),
    Command.typed(
      "bold",
      "Toggle bold formatting on the active selection.",
      CommandIntent.ToggleRichTextMark(com.serenity.richtext.InlineMark.Bold),
      CommandCategory.Edit,
      label = "Bold"
    ),
    Command.typed(
      "italic",
      "Toggle italic formatting on the active selection.",
      CommandIntent.ToggleRichTextMark(com.serenity.richtext.InlineMark.Italic),
      CommandCategory.Edit,
      label = "Italic"
    ),
    Command.typed(
      "underline",
      "Toggle underline formatting on the active selection.",
      CommandIntent.ToggleRichTextMark(com.serenity.richtext.InlineMark.Underline),
      CommandCategory.Edit,
      label = "Underline"
    ),
    Command.typed(
      "paragraph-body",
      "Set the active paragraph to body text.",
      CommandIntent.SetRichTextParagraphRole(ParagraphRole.Body),
      CommandCategory.Edit,
      label = "Body Text"
    ),
    Command.typed(
      "heading-1",
      "Set the active paragraph to heading level 1.",
      CommandIntent.SetRichTextParagraphRole(ParagraphRole.Heading(1)),
      CommandCategory.Edit,
      label = "Heading 1"
    ),
    Command.typed(
      "heading-2",
      "Set the active paragraph to heading level 2.",
      CommandIntent.SetRichTextParagraphRole(ParagraphRole.Heading(2)),
      CommandCategory.Edit,
      label = "Heading 2"
    ),
    Command.typed(
      "heading-3",
      "Set the active paragraph to heading level 3.",
      CommandIntent.SetRichTextParagraphRole(ParagraphRole.Heading(3)),
      CommandCategory.Edit,
      label = "Heading 3"
    ),
    Command.typed(
      "align-left",
      "Align the active paragraph to the left.",
      CommandIntent.SetRichTextParagraphAlignment(ParagraphAlignment.Left),
      CommandCategory.Edit,
      label = "Align Left"
    ),
    Command.typed(
      "align-center",
      "Center the active paragraph.",
      CommandIntent.SetRichTextParagraphAlignment(ParagraphAlignment.Center),
      CommandCategory.Edit,
      label = "Align Center"
    ),
    Command.typed(
      "align-right",
      "Align the active paragraph to the right.",
      CommandIntent.SetRichTextParagraphAlignment(ParagraphAlignment.Right),
      CommandCategory.Edit,
      label = "Align Right"
    ),
    Command.typed(
      "align-justify",
      "Justify the active paragraph.",
      CommandIntent.SetRichTextParagraphAlignment(ParagraphAlignment.Justify),
      CommandCategory.Edit,
      label = "Justify"
    ),
    Command.typed(
      "comment-lens",
      "Show or hide the rendered comment at the cursor.",
      CommandIntent.ToggleCommentLens,
      CommandCategory.View,
      label = "Comment Lens"
    ),
    Command.typed(
      "goto-line",
      "Go to a specific line number.",
      CommandIntent.OpenGotoLine,
      CommandCategory.Edit,
      label = "Go to Line"
    ),
    Command.typed(
      "toggle-bookmark",
      "Add or remove a bookmark at the current cursor.",
      CommandIntent.ToggleBookmark,
      CommandCategory.View,
      label = "Toggle Bookmark"
    ),
    Command.typed(
      "next-bookmark",
      "Go to the next bookmark.",
      CommandIntent.NextBookmark,
      CommandCategory.View,
      label = "Next Bookmark"
    ),
    Command.typed(
      "previous-bookmark",
      "Go to the previous bookmark.",
      CommandIntent.PreviousBookmark,
      CommandCategory.View,
      label = "Previous Bookmark"
    ),
    Command.typed(
      "navigate-back",
      "Go back to the previous document navigation point.",
      CommandIntent.NavigateBack,
      CommandCategory.View,
      label = "Navigate Back"
    ),
    Command.typed(
      "navigate-forward",
      "Go forward to the next document navigation point.",
      CommandIntent.NavigateForward,
      CommandCategory.View,
      label = "Navigate Forward"
    ),
    Command.typed(
      "next-document-symbol",
      "Go to the next document symbol.",
      CommandIntent.NextDocumentSymbol,
      CommandCategory.View,
      label = "Next Document Symbol"
    ),
    Command.typed(
      "previous-document-symbol",
      "Go to the previous document symbol.",
      CommandIntent.PreviousDocumentSymbol,
      CommandCategory.View,
      label = "Previous Document Symbol"
    ),
    Command.typed(
      "lsp-hover",
      "Show language-server hover information at the cursor.",
      CommandIntent.RequestLspHover,
      CommandCategory.Edit,
      label = "LSP Hover"
    ),
    Command.typed(
      "lsp-definition",
      "Request the symbol definition from the language server.",
      CommandIntent.RequestLspDefinition,
      CommandCategory.Edit,
      label = "LSP Definition"
    ),
    Command.typed(
      "toggle-theme",
      "Switch between the light and dark themes.",
      CommandIntent.ToggleTheme,
      CommandCategory.Settings,
      label = "Toggle Theme"
    ),
    Command.typed(
      "reload-theme",
      "Reload the current theme configuration.",
      CommandIntent.ReloadTheme,
      CommandCategory.Settings,
      label = "Reload Theme"
    ),
    Command.typed(
      "theme-chooser",
      "Choose a theme with live preview.",
      CommandIntent.OpenThemeChooser,
      CommandCategory.Settings,
      label = "Open Theme Chooser"
    ),
    Command.typed(
      "reload-themes",
      "Reload available themes from disk.",
      CommandIntent.ReloadThemes,
      CommandCategory.Settings,
      label = "Reload Theme List"
    ),
    Command.typed(
      "format",
      "Format the current file.",
      CommandIntent.FormatCurrentFile,
      CommandCategory.Edit,
      label = "Format File"
    ),
    Command.typed(
      "pin-explorer",
      "Pin the explorer panel on the left.",
      CommandIntent.PinExplorerPanel,
      CommandCategory.View,
      label = "Pin Explorer Panel"
    ),
    Command.typed(
      "pin-outline",
      "Pin the outline panel on the right.",
      CommandIntent.PinOutlinePanel,
      CommandCategory.View,
      label = "Pin Outline Panel"
    ),
    Command.typed(
      "pin-diagnostics",
      "Pin the diagnostics panel at the bottom.",
      CommandIntent.PinDiagnosticsPanel,
      CommandCategory.View,
      label = "Pin Diagnostics Panel"
    ),
    Command.typed(
      "markdown-preview",
      "Open a rendered Markdown preview for the current buffer.",
      CommandIntent.OpenMarkdownPreview,
      CommandCategory.View,
      label = "Open Markdown Preview"
    ),
    Command.typed(
      "markdown-view-source",
      "Show Markdown buffers as editable source.",
      CommandIntent.SetMarkdownViewMode(com.serenity.config.MarkdownViewMode.Source),
      CommandCategory.Settings,
      label = "Markdown View Source"
    ),
    Command.typed(
      "markdown-view-split",
      "Show Markdown source with a live side-by-side preview.",
      CommandIntent.SetMarkdownViewMode(com.serenity.config.MarkdownViewMode.SplitPreview),
      CommandCategory.Settings,
      label = "Markdown View Split"
    ),
    Command.typed(
      "markdown-view-inline-lens",
      "Show rendered Markdown with a raw-source editing lens at the cursor.",
      CommandIntent.SetMarkdownViewMode(com.serenity.config.MarkdownViewMode.InlineLens),
      CommandCategory.Settings,
      label = "Markdown View Inline Lens"
    ),
    Command.typed(
      "spellcheck-on",
      "Enable spell-checking for prose buffers.",
      CommandIntent.SetSpellCheckEnabled(true),
      CommandCategory.Settings,
      label = "Spell Check On"
    ),
    Command.typed(
      "spellcheck-off",
      "Disable spell-checking for prose buffers.",
      CommandIntent.SetSpellCheckEnabled(false),
      CommandCategory.Settings,
      label = "Spell Check Off"
    ),
    Command.typed(
      "focus-left-panel",
      "Focus the left pinned panel.",
      CommandIntent.FocusPanel(PanelPosition.Left),
      CommandCategory.View,
      label = "Focus Left Panel"
    ),
    Command.typed(
      "focus-right-panel",
      "Focus the right pinned panel.",
      CommandIntent.FocusPanel(PanelPosition.Right),
      CommandCategory.View,
      label = "Focus Right Panel"
    ),
    Command.typed(
      "focus-bottom-panel",
      "Focus the bottom pinned panel.",
      CommandIntent.FocusPanel(PanelPosition.Bottom),
      CommandCategory.View,
      label = "Focus Bottom Panel"
    ),
    Command.typed(
      "unpin-left-panel",
      "Unpin the left panel.",
      CommandIntent.UnpinPanel(PanelPosition.Left),
      CommandCategory.View,
      label = "Unpin Left Panel"
    ),
    Command.typed(
      "unpin-right-panel",
      "Unpin the right panel.",
      CommandIntent.UnpinPanel(PanelPosition.Right),
      CommandCategory.View,
      label = "Unpin Right Panel"
    ),
    Command.typed(
      "unpin-bottom-panel",
      "Unpin the bottom panel.",
      CommandIntent.UnpinPanel(PanelPosition.Bottom),
      CommandCategory.View,
      label = "Unpin Bottom Panel"
    ),
    Command.typed(
      "expand-left-panel",
      "Expand the left pinned panel into the editor workspace.",
      CommandIntent.ExpandPanel(PanelPosition.Left),
      CommandCategory.View,
      label = "Expand Left Panel"
    ),
    Command.typed(
      "expand-right-panel",
      "Expand the right pinned panel into the editor workspace.",
      CommandIntent.ExpandPanel(PanelPosition.Right),
      CommandCategory.View,
      label = "Expand Right Panel"
    ),
    Command.typed(
      "expand-bottom-panel",
      "Expand the bottom pinned panel into the editor workspace.",
      CommandIntent.ExpandPanel(PanelPosition.Bottom),
      CommandCategory.View,
      label = "Expand Bottom Panel"
    ),
    Command.typed(
      "collapse-expanded-panel",
      "Collapse the expanded panel back to its pinned position.",
      CommandIntent.CollapseExpandedPanel,
      CommandCategory.View,
      label = "Collapse Expanded Panel"
    ),
    Command.typed(
      "project-build",
      "Build the detected project.",
      CommandIntent.RunProjectTask(ProjectTaskKind.Build),
      CommandCategory.Project,
      label = "Build Project"
    ),
    Command.typed(
      "project-test",
      "Run tests for the detected project.",
      CommandIntent.RunProjectTask(ProjectTaskKind.Test),
      CommandCategory.Project,
      label = "Test Project"
    ),
    Command.typed(
      "project-run",
      "Run the detected project.",
      CommandIntent.RunProjectTask(ProjectTaskKind.Run),
      CommandCategory.Project,
      label = "Run Project"
    ),
    Command.typed(
      "project-debug",
      "Run the detected project through its debug workflow.",
      CommandIntent.RunProjectTask(ProjectTaskKind.Debug),
      CommandCategory.Project,
      label = "Debug Project"
    ),
    Command.typed(
      "project-dependencies",
      "Show or resolve dependencies for the detected project.",
      CommandIntent.RunProjectTask(ProjectTaskKind.Dependencies),
      CommandCategory.Project,
      label = "Project Dependencies"
    )
  ) ++ builtInPresetCommands

  private def builtInPresetCommands: List[Command] =
    UiPreset.builtIns.map { preset =>
      val preview = UiPreset.Preview.fromPreset(preset)
      Command.typed(
        name = s"apply-${preset.name.toLowerCase.replace(' ', '-')}-preset",
        description = s"Apply the ${preset.name} workspace preset: ${preview.hint}.",
        intent = CommandIntent.ApplyUiPreset(preset.name),
        category = CommandCategory.Settings,
        label = s"Apply ${preset.name} Preset"
      )
    }
