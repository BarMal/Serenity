package com.serenity.command

import com.serenity.config.{AppMode, StatusSegment}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.presets.UiPreset

/** Builds the settings tree from schema rows and current option selections.
  *
  * The tree is cut by the task someone is doing, not by where a value happens to live in `AppConfig`: Workspace (what
  * am I working on), Editor (what the text area shows), Typography, Look, Motion, Language Tools and Keys. Each group
  * that has knobs nobody needs day to day keeps them in one nested "Advanced" leaf rather than spreading them across
  * the top level.
  */
object CommandRunnerSettingsGroups:

  /** `isTuiMode` hides no controls -- every setting still applies its intent identically in TUI mode, so hiding one
    * would make it impossible to prepare a config while running headless. Instead the groups epic #1103 called out as
    * inert in cell space (post-processing effects, typography) get their hint annotated to say so.
    */
  def build(
    optionSelections: Map[String, Int],
    inputItems: List[CommandSurfaceItem.InputItem],
    uiPresetPreviews: List[UiPreset.Preview],
    editingPresetName: Option[String],
    isTuiMode: Boolean = false,
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
    val showAllSettingsRegardlessOfMode = optionSelections.getOrElse("settings-show-all", 0) == 1
    val showProseSettings               = showAllSettingsRegardlessOfMode || appMode == AppMode.Prose
    val showCodeSettings                = showAllSettingsRegardlessOfMode || appMode == AppMode.Code
    def input(ids: String*): List[CommandSurfaceItem.InputItem] =
      ids.toList.flatMap(id => inputItems.find(_.id == id))
    def group(id: String, label: String, hint: String, children: List[CommandSurfaceItem]) =
      CommandSurfaceItem.GroupItem(id, label, children, CommandCategory.Settings, Some(hint))

    val workspaceLayoutGroup = group(
      "settings-workspace-layout",
      "Panels",
      "Pin, focus, expand, and unpin panels",
      CommandRunnerSettingsPanelItems.workspaceLayoutItems(optionSelections)
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
      ) ++ input("wheel-scroll-lines") ++ List(
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
        CommandRunnerSettingsStatusLineItems.segmentItems(optionSelections, statusSegments)
    )
    val textAreaGroup = group(
      "settings-text-area",
      "Text Area",
      "Resize editor margins",
      input("text-area-left", "text-area-right", "text-area-top", "text-area-bottom")
    )
    val documentDefaultsGroup = group(
      "settings-document-defaults",
      "Document Defaults",
      "New document mode and Markdown view",
      List(
        CommandRunnerSettingsItems.defaultDocumentModeOptionItem(optionSelections),
        CommandRunnerSettingsItems.markdownViewOptionItem(optionSelections)
      )
    )
    // issue #1057: the one-shot navigation commands that used to sit here are ordinary palette commands. The one item
    // that stays is authoring a document comment's text, a real input rather than an action.
    val commentsGroup = group("settings-navigation", "Comments", "Author a document comment", input("document-comment"))
    val fontHint      = inertInTuiHint("Family, size, ligatures", isTuiMode)
    val proseFontGroup = group(
      "settings-prose-font",
      "Prose Font",
      fontHint,
      List(
        CommandRunnerSettingsItems.textFontGroupItem(optionSelections, fontFamilies.text),
        CommandRunnerSettingsItems.textLigaturesOptionItem(optionSelections)
      ) ++ input("text-font-size")
    )
    val codeFontGroup = group(
      "settings-code-font",
      "Code Font",
      fontHint,
      List(
        CommandRunnerSettingsItems.codeFontGroupItem(optionSelections, fontFamilies.monospace),
        CommandRunnerSettingsItems.codeLigaturesOptionItem(optionSelections)
      ) ++ input("code-font-size")
    )
    val uiFontGroup = group(
      "settings-ui-font",
      "UI Font",
      fontHint,
      List(
        CommandRunnerSettingsItems.uiFontGroupItem(optionSelections, fontFamilies.ui),
        CommandRunnerSettingsItems.uiLigaturesOptionItem(optionSelections)
      ) ++ input("ui-font-size")
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
      CommandRunnerSettingsItems.richTextFontGroupItem(optionSelections, fontFamilies.text) ::
        inputItems.filter(_.id.startsWith("rich-text-"))
    )
    val themeGroup = CommandRunnerSettingsItems.themeGroupItem(context.themeNames, context.currentThemeName)
    val surfaceAppearanceGroup = group(
      "settings-surface-appearance",
      "Surface Appearance",
      "Background, material, and effects",
      List(
        CommandRunnerSettingsAppearanceItems.backgroundStyleOptionItem(optionSelections),
        CommandRunnerSettingsAppearanceItems.materialPresetOptionItem(optionSelections),
        annotateInertInTui(CommandRunnerSettingsAppearanceItems.postProcessingOptionItem(optionSelections), isTuiMode),
        CommandRunnerSettingsAppearanceItems.uiShadowsOptionItem(optionSelections)
      )
    )
    // issue #1046: command-runner row count/spacing (visible rows, item gap, cursor gap) is not editable here as
    // three separate knobs -- Interface Density is the one control that governs all three; the underlying config
    // keys still parse as explicit overrides for back-compat, they just aren't palette rows.
    val interfaceLayoutGroup = group(
      "settings-interface-layout",
      "Interface Layout",
      "Density, window chrome, key hints",
      List(
        CommandRunnerSettingsAppearanceItems.interfaceDensityOptionItem(optionSelections),
        CommandRunnerSettingsAppearanceItems.windowChromeOptionItem(optionSelections),
        CommandRunnerSettingsTextDisplayItems.commandRunnerKeyHintsOptionItem(optionSelections)
      )
    )
    val cursorGroup = group(
      "settings-cursor",
      "Cursor",
      "Blink or breathe",
      List(CommandRunnerSettingsCursorItems.cursorModeOptionItem(optionSelections))
    )
    val lookAdvancedGroup = group(
      "settings-look-advanced",
      "Advanced",
      "Blur, spacing, corners, outlines, render cadence, decorative extras",
      input("blur-radius", "ui-element-gap", "ui-corner-radius", "ui-outline-thickness") ++ List(
        CommandRunnerSettingsMotionItems.renderFpsOptionItem(optionSelections),
        CommandRunnerSettingsMotionItems.renderDamageGranularityOptionItem(optionSelections),
        CommandRunnerSettingsAppearanceItems.visualFlairLevelOptionItem(optionSelections),
        CommandRunnerSettingsAppearanceItems.companionSpriteEnabledOptionItem(optionSelections)
      )
    )
    val customMotionInputIds =
      if optionSelections.get("motion-preset").contains(4) then List("animation-duration", "animation-steps") else Nil
    val motionAdvancedGroup = group(
      "settings-motion-advanced",
      "Advanced",
      "Per-family speed, custom timing, window sitter tuning",
      input(
        "cursor-speed-scale",
        "element-transition-speed-scale",
        "editor-text-speed-scale",
        "command-runner-speed-scale",
        "ui-speed-scale"
      ) ++ input(customMotionInputIds*) ++ input(
        "window-sitter-frames",
        "window-sitter-active-ticks",
        "window-sitter-fast-active-ticks",
        "window-sitter-fast-threshold-ms"
      )
    )
    val motionGroup = group(
      "settings-animation",
      "Motion",
      "Accessibility, preset, reveal style, window sitter",
      List(
        CommandRunnerSettingsMotionItems.motionAccessibilityOptionItem(optionSelections),
        CommandRunnerSettingsMotionItems.motionPresetOptionItem(optionSelections),
        CommandRunnerSettingsMotionItems.editorTextTransitionOptionItem(optionSelections),
        CommandRunnerSettingsMotionItems.panelOpenTransitionOptionItem(optionSelections),
        CommandRunnerSettingsMotionItems.panelCloseTransitionOptionItem(optionSelections),
        CommandRunnerSettingsMotionItems.commandRunnerTransitionOptionItem(optionSelections),
        CommandRunnerSettingsMotionItems.commandRunnerFadeOptionItem(optionSelections),
        CommandRunnerSettingsMotionItems.uiAnimationOptionItem(optionSelections),
        CommandRunnerSettingsAppearanceItems.windowSitterEnabledOptionItem(optionSelections),
        CommandRunnerSettingsAppearanceItems.windowSitterActionOptionItem(optionSelections),
        motionAdvancedGroup
      )
    )
    val bufferLanguageGroup = CommandRunnerSettingsItems.bufferLanguageGroupItem(context.bufferLanguage)
    val spellCheckGroup = group(
      "settings-spellcheck",
      "Spell Check",
      "Enable, languages, dictionaries, accepted words",
      CommandRunnerSettingsItems.spellCheckOptionItem(optionSelections) ::
        input("spellcheck-languages", "spellcheck-dictionaries", "spellcheck-words")
    )
    val keysGroup = group(
      "settings-keymap",
      "Keys",
      "Inspect and edit bindings",
      inputItems.filter(_.id.startsWith("keymap-"))
    )

    // issue #1058: editing a preset reuses these same canonical groups verbatim, only retagged (`settings-preset-*`)
    // so they stay addressable as distinct pages from their top-level counterparts.
    val presetScoped = List(
      workspaceLayoutGroup.copy(id = "settings-preset-workspace-layout"),
      surfaceAppearanceGroup.copy(id = "settings-preset-surface-appearance"),
      cursorGroup.copy(id = "settings-preset-cursor"),
      motionGroup.copy(id = "settings-preset-animation"),
      proseFontGroup.copy(id = "settings-preset-prose-font"),
      codeFontGroup.copy(id = "settings-preset-code-font"),
      uiFontGroup.copy(id = "settings-preset-ui-font"),
      documentDefaultsGroup.copy(id = "settings-preset-document-defaults"),
      spellCheckGroup.copy(id = "settings-preset-spellcheck")
    )
    val presetsGroup =
      CommandRunnerSettingsPresetGroups.build(
        optionSelections,
        inputItems,
        uiPresetPreviews,
        editingPresetName,
        presetScoped
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
      List(textDisplayGroup, statusLineGroup, textAreaGroup, documentDefaultsGroup, commentsGroup)
    )
    val typographyGroup = group(
      "settings-typography",
      "Typography",
      inertInTuiHint("Typefaces for prose, code, and interface", isTuiMode),
      List(
        Option.when(showProseSettings)(proseFontGroup),
        Option.when(showCodeSettings)(codeFontGroup),
        Some(uiFontGroup),
        Some(textScaleGroup),
        Option.when(showProseSettings)(richTextGroup)
      ).flatten
    )
    val lookGroup = group(
      "settings-look",
      "Look",
      "Theme, surfaces, density, cursor",
      List(themeGroup, surfaceAppearanceGroup, interfaceLayoutGroup, cursorGroup, lookAdvancedGroup)
    )
    val languageToolsGroup = group(
      "settings-language-tools",
      "Language Tools",
      "Buffer language and spelling",
      bufferLanguageGroup :: Option.when(showProseSettings)(spellCheckGroup).toList
    )
    List(workspaceGroup, editorGroup, typographyGroup, lookGroup, motionGroup, languageToolsGroup, keysGroup)

  /** Lead a hint with the note that the control it describes has no visible effect on a fixed-cell terminal surface --
    * the setting still applies and persists identically, it just paints nothing different in TUI mode (see epic #1103's
    * accepted degradations).
    *
    * The note leads rather than trails because the settings surface's hint column is a fixed share of the panel width
    * and elides from the right (`TextOverlayRenderer.fitCellText`): appended to a hint as long as Post-processing's,
    * the annotation was cut off before it could ever be read, at any terminal width.
    */
  private def inertInTuiHint(hint: String, isTuiMode: Boolean): String =
    if isTuiMode then s"Inert in TUI mode -- $hint" else hint

  private def annotateInertInTui(
    item: CommandSurfaceItem.OptionItem,
    isTuiMode: Boolean
  ): CommandSurfaceItem.OptionItem =
    item.copy(hint = Some(inertInTuiHint(item.hint.getOrElse(item.label), isTuiMode)))
