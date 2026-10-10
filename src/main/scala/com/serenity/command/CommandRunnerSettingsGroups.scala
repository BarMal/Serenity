package com.serenity.command

import com.serenity.config.{AppMode, StatusSegment}
import com.serenity.frontend.FrontendCapabilities
import com.serenity.state.models.Shell
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.presets.UiPreset

/** Builds the settings tree from schema rows and current option selections.
  *
  * The tree is cut by the task someone is doing, not by where a value happens to live in `AppConfig`: Workspace (what
  * am I working on), Editor (what the text area shows), Typography, Look, Language Tools and Keys. Each group that has
  * knobs nobody needs day to day keeps them in one nested "Advanced" leaf rather than spreading them across the top
  * level.
  */
object CommandRunnerSettingsGroups:

  /** Rows with no visible effect on the running frontend (`capabilities`) are hidden, the same way the app mode hides
    * the other mode's rows; Show All Settings brings both back. Which rows are [[FrontendSupport.GuiOnly]] follows epic
    * #1103's accepted cell-space degradations: fonts, text scale, and pixel effects (outlines, window chrome).
    */
  def build(
    optionSelections: Map[String, Int],
    inputItems: List[CommandSurfaceItem.InputItem],
    uiPresetPreviews: List[UiPreset.Preview],
    editingPresetName: Option[String],
    capabilities: FrontendCapabilities = FrontendCapabilities.gui,
    // Defaults to whatever's actually installed on the running machine; tests that search this tree pass a
    // deterministic catalog instead so the result doesn't depend on the host's installed fonts (issue: Windows
    // Desktop Publish release-blocker -- a Windows-only font whose family name happened to contain a search term
    // used in a settings-search test).
    fontFamilies: FontLoader.FontFamilyCatalog = FontLoader.FontFamilyCatalog.system,
    // The status line's actual current segment order, so its reorder commands reflect what is on screen (#1298).
    statusSegments: List[StatusSegment] = Nil,
    context: CommandRunnerContext = CommandRunnerContext.empty
  ): List[CommandSurfaceItem.GroupItem] =
    // Read off `optionSelections` rather than taking a separate `AppConfig` parameter: it is already the one place
    // every setting's current value reaches this builder, so mode filtering can't drift from what the app-mode
    // toggle itself displays as selected.
    val appMode = if optionSelections.getOrElse("app-mode", 0) == 1 then AppMode.Prose else AppMode.Code
    // issue #1044: "settings-show-all" now encodes On=0/Off=1 (`CommandRunnerOptionSelections.enabledIndex`), like
    // every other boolean toggle -- was On=1/Off=0 (the one hand-rolled encoding `showAllSettingsOptionItem` used
    // before being normalized onto the shared `enabledOptionItem` convention).
    val showAllSettingsRegardlessOfMode = optionSelections.getOrElse("settings-show-all", 1) == 0
    val showProseSettings               = showAllSettingsRegardlessOfMode || appMode == AppMode.Prose
    val showCodeSettings                = showAllSettingsRegardlessOfMode || appMode == AppMode.Code
    val onFrontend = SettingsFrontendFilter(Shell.of(capabilities), showAllSettingsRegardlessOfMode)
    val guiOnly    = FrontendSupport.GuiOnly
    def input(ids: String*): List[CommandSurfaceItem.InputItem] =
      ids.toList.flatMap(id => inputItems.find(_.id == id))
    def group(id: String, label: String, hint: String, children: List[CommandSurfaceItem]) =
      CommandSurfaceItem.GroupItem(id, label, children, CommandCategory.Settings, Some(hint))

    // Preset pages are built from the same definitions, over the edited preset's own values (see `PresetGroups`).
    val editable              = CommandRunnerSettingsEditableGroups.build(optionSelections, inputItems, fontFamilies)
    val cursorGroup           = editable.cursor
    val proseFontGroup        = editable.proseFont
    val codeFontGroup         = editable.codeFont
    val uiFontGroup           = editable.uiFont
    val documentDefaultsGroup = editable.documentDefaults
    val spellCheckGroup       = editable.spellCheck

    val workspaceLayoutGroup = group(
      "settings-workspace-layout",
      "Panels",
      "Which panels show, where, and where Escape returns focus",
      CommandRunnerSettingsPanelItems.workspaceLayoutItems ++ List(
        Option.when(showCodeSettings)(
          CommandRunnerSettingsPanelItems.escapeTargetOptionItem(optionSelections, AppMode.Code)
        ),
        Option.when(showProseSettings)(
          CommandRunnerSettingsPanelItems.escapeTargetOptionItem(optionSelections, AppMode.Prose)
        )
      ).flatten
    )
    val textDisplayGroup = group(
      "settings-text-display",
      "Text Display",
      "Line numbers, wrap, scrolling, focus, toolbar",
      List(
        CommandRunnerSettingsTextDisplayItems.lineNumbersOptionItem(optionSelections),
        CommandRunnerSettingsTextDisplayItems.lineNumberSideOptionItem(optionSelections)
      ) ++ input("line-number-margin-left", "line-number-margin-right", "line-number-padding") ++ List(
        CommandRunnerSettingsTextDisplayItems.lineWrapOptionItem(optionSelections),
        CommandRunnerSettingsTextDisplayItems.visualLineNavigationOptionItem(optionSelections),
        CommandRunnerSettingsTextDisplayItems.typewriterScrollingOptionItem(optionSelections)
      ) ++ input("columns", "column-gap") ++ input("wheel-scroll-lines") ++ List(
        CommandRunnerSettingsTextDisplayItems.focusedTextBodyOptionItem(optionSelections),
        CommandRunnerSettingsTextDisplayItems.contextualToolbarOptionItem(optionSelections),
        CommandRunnerSettingsTextDisplayItems.contextualToolbarDisplayModeOptionItem(optionSelections)
      )
    )
    val statusLineGroup = group(
      "settings-status-line",
      "Status Line",
      "Placement, segments, and their order",
      CommandRunnerSettingsStatusLineItems.placementOptionItem(optionSelections) ::
        CommandRunnerSettingsStatusLineItems.segmentItems(optionSelections, statusSegments) ++
        input("word-goal")
    )
    val textAreaGroup = group(
      "settings-text-area",
      "Text Area",
      "Resize editor margins",
      input("text-area-left", "text-area-right", "text-area-top", "text-area-bottom")
    )
    val savingGroup = group(
      "settings-saving",
      "Saving",
      "Write files automatically after a pause or when you leave them",
      CommandRunnerSettingsItems.autoSaveModeOptionItem(optionSelections) :: input("auto-save-delay")
    )
    // issue #1057: the one-shot navigation commands that used to sit here are ordinary palette commands. The one item
    // that stays is authoring a document comment's text, a real input rather than an action.
    val commentsGroup = group(
      "settings-navigation",
      "Comments",
      "Author or reply to a document comment",
      input("document-comment", "reply-document-comment")
    )
    val textScaleGroup = group(
      "settings-text-scale",
      "Text Scale",
      "Adapt all text to display scale",
      CommandRunnerSettingsItems.textScaleModeOptionItem(optionSelections) :: input("text-scale")
    )
    // issue #1060: font family is a picker now, matching code/prose/UI font family -- no longer typed free text.
    val richTextGroup = group(
      "settings-rich-text",
      "Rich Text",
      "Selection family, size, colour",
      onFrontend
        .row(guiOnly, CommandRunnerSettingsItems.richTextFontGroupItem(optionSelections, fontFamilies.text))
        .toList ++ onFrontend.rows(guiOnly, input("rich-text-font-size")) ++ input("rich-text-color")
    )
    val themeGroup = CommandRunnerSettingsItems.themeGroupItem(context.themeNames, context.currentThemeName)
    val followSystemGroup = group(
      "settings-follow-system",
      "OS Appearance",
      "Follow the OS light, dark or high-contrast setting, and the theme for each",
      CommandRunnerSettingsItems.followSystemOptionItem(optionSelections) ::
        input("follow-system-light", "follow-system-dark", "follow-system-high-contrast")
    )
    // issue #1046: command-runner row count/spacing (visible rows, item gap, cursor gap) is not editable here as
    // three separate knobs -- Interface Density is the one control that governs all three; the underlying config
    // keys still parse as explicit overrides for back-compat, they just aren't palette rows.
    // issue #1549: that consolidation also buried the setting from a single-word settings search (a query short
    // enough to match by group rather than leaf) -- naming it in this group's own hint, which
    // `CommandRunnerSearch.directGroupSearchText` already searches, is what makes "palette" alone find this group.
    val interfaceLayoutGroup = group(
      "settings-interface-layout",
      "Interface Layout",
      "Density, window chrome, key hints, and how many command runner/palette items are visible",
      List(
        Some(CommandRunnerSettingsAppearanceItems.interfaceDensityOptionItem(optionSelections)),
        onFrontend.row(guiOnly, CommandRunnerSettingsAppearanceItems.windowChromeOptionItem(optionSelections)),
        Some(CommandRunnerSettingsTextDisplayItems.commandRunnerKeyHintsOptionItem(optionSelections))
      ).flatten
    )
    val lookAdvancedGroup = group(
      "settings-look-advanced",
      "Advanced",
      "Spacing, outlines, render cadence",
      input("ui-element-gap") ++
        onFrontend.rows(guiOnly, input("ui-outline-thickness")) ++ List(
          CommandRunnerSettingsRenderItems.renderFpsOptionItem(optionSelections),
          CommandRunnerSettingsRenderItems.renderDamageGranularityOptionItem(optionSelections)
        )
    )
    val bufferLanguageGroup = CommandRunnerSettingsItems.bufferLanguageGroupItem(context.bufferLanguage)
    val keysGroup = group(
      "settings-keymap",
      "Keys",
      "Inspect and edit bindings",
      CommandRunnerSettingsKeymapItems.keymapGroups(inputItems.filter(_.id.startsWith("keymap-")))
    )

    val presetsGroup =
      CommandRunnerSettingsPresetGroups.build(
        optionSelections,
        inputItems,
        uiPresetPreviews,
        editingPresetName,
        capabilities,
        fontFamilies,
        showAllSettingsRegardlessOfMode
      )

    val workspaceGroup = group(
      "settings-workspace",
      "Workspace",
      "Code or prose mode, presets, panels",
      List(
        CommandRunnerSettingsItems.appModeOptionItem(optionSelections),
        CommandRunnerSettingsItems.showAllSettingsOptionItem(optionSelections),
        workspaceLayoutGroup,
        presetsGroup
      )
    )
    val editorGroup = group(
      "settings-editor",
      "Editor",
      "Display, status line, margins, documents, comments",
      List(textDisplayGroup, statusLineGroup, textAreaGroup, documentDefaultsGroup, savingGroup, commentsGroup)
    )
    val typographyGroup = group(
      "settings-typography",
      "Typography",
      "Typefaces for prose, code, and interface",
      onFrontend.rows(
        guiOnly,
        List(
          Option.when(showProseSettings)(proseFontGroup),
          Option.when(showCodeSettings)(codeFontGroup),
          Some(uiFontGroup),
          Some(textScaleGroup)
        ).flatten
      ) ++ Option.when(showProseSettings && richTextGroup.children.nonEmpty)(richTextGroup)
    )
    val lookGroup = group(
      "settings-look",
      "Look",
      "Theme, density, cursor",
      List(themeGroup, followSystemGroup, interfaceLayoutGroup, cursorGroup, lookAdvancedGroup)
    )
    val languageToolsGroup = group(
      "settings-language-tools",
      "Language Tools",
      "Buffer language and spelling",
      bufferLanguageGroup :: Option.when(showProseSettings)(spellCheckGroup).toList
    )
    // A code workspace on a terminal has no typography row left to show.
    List(workspaceGroup, editorGroup) ++ Option.when(typographyGroup.children.nonEmpty)(typographyGroup) ++
      List(lookGroup, languageToolsGroup, keysGroup)
