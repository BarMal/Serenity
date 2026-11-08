package com.serenity.command

import com.serenity.config.{AppConfig, AppMode}
import com.serenity.state.models.{BufferKind, EditingContext, Focus, PaneId, Shell}
import com.serenity.ui.fonts.FontLoader
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2577 P0: what the palette lists first for a fixed set of queries today, in a code and a prose workspace. Later
  * ranking steps change rows here on purpose; a PR that does must say which rows and why.
  */
class CommandPaletteGoldenRankingSpec extends AnyFlatSpec with Matchers:

  private val registry = CommandRegistry.default

  private val fontFamilies =
    FontLoader.FontFamilyCatalog(monospace = List("Mono A"), text = List("Serif A"), ui = List("Sans A"))

  private def workspace(mode: AppMode, buffer: BufferKind): CommandRunnerContext =
    CommandRunnerContext(
      themeNames = List("Daylight", "Midnight"),
      currentThemeName = Some("Daylight"),
      editingContext = Some(EditingContext(mode, Some(buffer), Shell.Gui, Focus.EditorPane(PaneId(0))))
    )

  private val code  = workspace(AppMode.Code, BufferKind.PlainText)
  private val prose = workspace(AppMode.Prose, BufferKind.RichText)

  /** A row as `kind:id`, so a settings group and a command with the same id stay distinguishable. */
  private def rowKey(item: CommandSurfaceItem): String =
    item match
      case CommandSurfaceItem.CommandItem(command, _) => s"command:${command.name}"
      case setting: CommandSurfaceItem.SettingSearchItem =>
        s"setting:${setting.targetGroupId}/${setting.targetItemId}"
      case group: CommandSurfaceItem.GroupItem => s"group:${group.id}"
      case other                               => s"item:${other.id}"

  private def topFive(context: CommandRunnerContext, query: String): List[String] =
    CommandRunner.empty
      .copy(fontFamilies = fontFamilies)
      .activate(registry, AppConfig.default, context = context)
      .updateSearchTerm(query)(using registry)
      .visibleItems
      .take(5)
      .map(rowKey)

  private val codeRows: List[(String, List[String])] = List(
    "save" -> List(
      "command:save",
      "setting:settings-keymap-global-files/keymap-global-save",
      "command:save-as",
      "command:save-config",
      "command:save-session"
    ),
    "open" -> List(
      "command:open",
      "command:open-settings",
      "command:open-session",
      "command:open-folder",
      "command:open-logs-folder"
    ),
    "go to file" -> List(
      "command:go-to-file",
      "setting:settings-keymap-global-navigation/keymap-global-go_to_file",
      "command:goto-line",
      "command:toggle-explorer-panel",
      "command:clear-recent-files"
    ),
    "file" -> List(
      "command:file-search",
      "command:go-to-file",
      "command:clear-recent-files",
      "command:new",
      "command:open"
    ),
    "line" -> List(
      "command:goto-line",
      "command:change-line-ending",
      "command:toggle-line-comment",
      "command:cut",
      "command:copy"
    ),
    "goto line" -> List(
      "command:goto-line",
      "command:lsp-hover",
      "command:lsp-completion",
      "command:apply-writing-preset",
      "command:clear-recent-files"
    ),
    "test" -> List(
      "command:project-test",
      "command:save-config",
      "command:toggle-project-output-panel",
      "command:toggle-tab-list",
      "command:toggle-diagnostics-panel"
    ),
    "run tests" -> List(
      "command:project-test",
      "command:show-build-details",
      "command:lsp-trust-folder",
      "command:toggle-project-output-panel",
      "command:lsp-completion"
    ),
    "build" -> List(
      "command:project-build",
      "command:show-build-details",
      "command:toggle-project-output-panel",
      "command:lsp-trust-folder",
      "command:about"
    ),
    "theme" -> List(
      "command:theme-chooser",
      "command:theme-creator",
      "command:toggle-theme",
      "command:reload-theme",
      "command:export-theme"
    ),
    "dark" -> List(
      "command:toggle-theme",
      "group:settings-follow-system",
      "command:toggle-markdown-preview-panel",
      "command:markdown-toggle-read",
      "command:toggle-bookmark"
    ),
    "font size" -> List(
      "setting:settings-font-sizes/font-sizes-code",
      "setting:settings-font-sizes/font-sizes-prose",
      "setting:settings-font-sizes/font-sizes-ui",
      "setting:settings-text-scale/text-scale-mode",
      "setting:settings-font-sizes/set-all-font-sizes"
    ),
    "font" -> List(
      "group:settings-font-sizes",
      "group:settings-code-font",
      "group:settings-ui-font",
      "group:settings-preset-edit",
      "command:focus-comments-panel"
    ),
    "tab" -> List(
      "setting:settings-keymap-editor/keymap-editor-tab",
      "command:toggle-tab-list",
      "command:next-tab",
      "command:previous-tab",
      "command:return-to-start-page"
    ),
    "tab width" -> List(
      "setting:settings-look-advanced/tab-max-width",
      "command:toggle-tab-list",
      "command:change-indentation",
      "command:toggle-explorer-panel",
      "command:toggle-outline-panel"
    ),
    "split" -> List(
      "command:split-pane-horizontal",
      "command:split-pane-vertical",
      "command:apply-documentation-preset",
      "group:settings-document-defaults",
      "group:settings-keymap-global-navigation"
    ),
    "close" -> List(
      "command:close",
      "command:close-all",
      "command:close-others",
      "command:close-pane",
      "group:settings-keymap-global-navigation"
    ),
    "undo" -> List(
      "command:undo",
      "setting:settings-keymap-global-editing/keymap-global-undo",
      "command:redo",
      "command:project-test",
      "command:project-run"
    ),
    "find" -> List(
      "command:find",
      "setting:settings-keymap-global-editing/keymap-global-find",
      "command:find-all",
      "command:lsp-references",
      "command:replace"
    ),
    "replace" -> List(
      "command:replace",
      "setting:settings-keymap-global-editing/keymap-global-replace",
      "command:replace-all",
      "command:reopen-document-comment",
      "command:clear-recent-files"
    ),
    "comment" -> List(
      "command:comment-lens",
      "command:focus-comments-panel",
      "command:toggle-comments-panel",
      "command:toggle-line-comment",
      "command:add-document-comment"
    ),
    "format" -> List(
      "command:format",
      "command:save-config",
      "command:lsp-hover",
      "command:navigate-forward",
      "command:open-chapter-note"
    ),
    "rename" -> List(
      "command:rename-session",
      "command:lsp-rename",
      "group:settings-preset-edit",
      "command:restart-safe-mode",
      "command:reopen-with-encoding"
    ),
    "settings" -> List(
      "command:open-settings",
      "command:reset-settings",
      "command:save-config",
      "command:restart-normally",
      "command:restart-safe-mode"
    ),
    "key" -> List(
      "command:toggle-shortcuts-help",
      "group:settings-interface-layout",
      "group:settings-keymap-editor",
      "group:settings-keymap-command-runner",
      "group:settings-keymap-dialogs"
    ),
    "preset" -> List(
      "command:apply-code-preset",
      "command:apply-review-preset",
      "command:apply-writing-preset",
      "command:apply-compact-preset",
      "command:apply-documentation-preset"
    ),
    "wrap" -> List(
      "command:toggle-block-comment",
      "group:settings-text-display",
      "command:show-build-details",
      "command:toggle-recent-in-mode",
      "command:open-settings"
    ),
    "tgogle theme" -> List(
      "command:toggle-diagnostics-panel",
      "command:open-logs-folder"
    ),
    "markdown" -> List(
      "setting:buffer-language/lang-markdown",
      "command:markdown-preview",
      "command:markdown-toggle-read",
      "command:focus-markdown-preview-panel",
      "command:toggle-markdown-preview-panel"
    ),
    "sesion" -> List(
      "command:save-session",
      "command:save-session-as",
      "command:show-spelling-suggestions",
      "command:restore-session",
      "command:open-session"
    )
  )

  private val proseRows: List[(String, List[String])] = List(
    "bold" -> List(
      "command:bold",
      "setting:settings-heading-h1/heading-h1-bold",
      "setting:settings-heading-h2/heading-h2-bold",
      "setting:settings-heading-h3/heading-h3-bold",
      "setting:settings-heading-h4/heading-h4-bold"
    ),
    "save" -> List(
      "command:save",
      "setting:settings-keymap-global-files/keymap-global-save",
      "command:save-as",
      "command:save-config",
      "command:save-session"
    ),
    "chapter" -> List(
      "command:open-chapter-note",
      "command:toggle-chapter-ghosts",
      "command:toggle-notes-pin",
      "group:settings-keymap-global-view",
      "command:export-manuscript"
    ),
    "darlings" -> List(
      "command:cut-to-darlings",
      "command:restore-darling",
      "command:split-pane-horizontal",
      "command:split-pane-vertical",
      "command:reformat-headings"
    ),
    "theme" -> List(
      "command:theme-chooser",
      "command:theme-creator",
      "command:toggle-theme",
      "command:reload-theme",
      "command:export-theme"
    ),
    "font size" -> List(
      "setting:settings-font-sizes/font-sizes-code",
      "setting:settings-font-sizes/font-sizes-prose",
      "setting:settings-font-sizes/font-sizes-ui",
      "setting:settings-text-scale/text-scale-mode",
      "setting:settings-font-sizes/set-all-font-sizes"
    ),
    "spell" -> List(
      "command:show-spelling-suggestions",
      "group:settings-language-tools",
      "group:settings-preset-edit",
      "command:ignore-misspelled-word-once",
      "command:ignore-misspelled-word"
    ),
    "export" -> List(
      "command:export-manuscript",
      "command:export-manuscript-docx",
      "command:export-manuscript-epub",
      "command:export-manuscript-pdf",
      "command:export-theme"
    )
  )

  for (query, expected) <- codeRows do
    s"In a code workspace, '$query'" should "list today's top five" in {
      topFive(code, query) shouldBe expected
    }

  for (query, expected) <- proseRows do
    s"In a prose workspace, '$query'" should "list today's top five" in {
      topFive(prose, query) shouldBe expected
    }
