package com.serenity.command

import com.serenity.config.{AppMode, CursorInfoBarSegment}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.presets.UiPreset

/** Builds command-runner settings groups from schema rows and current option selections. */
object CommandRunnerSettingsGroups:

  /** Derive the nested settings menu without depending on command-runner navigation state. */
  /** `isTuiMode` hides no controls -- every setting still applies its intent identically in TUI mode, so hiding one
    * would make it impossible to prepare a config while running headless. Instead the two groups epic #1103 called out
    * as inert in cell space (post-processing effects, typography) get their hint annotated to say so, exactly as
    * `postProcessingOptionItem`'s own hint documents Cells-only granularity today.
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
    // The segments' actual current order (`AppConfig.cursorInfoBarSegments`) -- threaded through so the reorder
    // commands `cursorInfoBarSegmentItems` builds reflect it (issue #1298). `Nil` falls back to that function's own
    // fixed-order, ungated default.
    cursorInfoBarSegments: List[CursorInfoBarSegment] = Nil
  ): List[CommandSurfaceItem.GroupItem] =
    // Read off `optionSelections` rather than taking a separate `AppConfig` parameter: it is already the one place
    // every setting's current value reaches this builder, so mode filtering can't drift from what the app-mode
    // toggle itself displays as selected.
    val appMode = if optionSelections.getOrElse("app-mode", 0) == 1 then AppMode.Prose else AppMode.Code
    val showAllSettingsRegardlessOfMode = optionSelections.getOrElse("settings-show-all", 0) == 1
    val cursorModeItem                  = CommandRunnerSettingsCursorItems.cursorModeOptionItem(optionSelections)
    val cursorInfoBarItems =
      CommandRunnerSettingsCursorItems.cursorInfoBarSegmentItems(optionSelections, cursorInfoBarSegments)
    val cursorInfoPlacement     = CommandRunnerSettingsCursorItems.cursorInfoBarPlacementOptionItem(optionSelections)
    val backgroundStyleItem     = CommandRunnerSettingsAppearanceItems.backgroundStyleOptionItem(optionSelections)
    val interfaceDensityItem    = CommandRunnerSettingsAppearanceItems.interfaceDensityOptionItem(optionSelections)
    val windowChromeItem        = CommandRunnerSettingsAppearanceItems.windowChromeOptionItem(optionSelections)
    val windowSitterEnabledItem = CommandRunnerSettingsAppearanceItems.windowSitterEnabledOptionItem(optionSelections)
    val windowSitterActionItem  = CommandRunnerSettingsAppearanceItems.windowSitterActionOptionItem(optionSelections)
    val companionSpriteEnabledItem =
      CommandRunnerSettingsAppearanceItems.companionSpriteEnabledOptionItem(optionSelections)
    val visualFlairLevelItem = CommandRunnerSettingsAppearanceItems.visualFlairLevelOptionItem(optionSelections)
    val materialPresetItem   = CommandRunnerSettingsAppearanceItems.materialPresetOptionItem(optionSelections)
    val postProcessingItem =
      annotateInertInTui(CommandRunnerSettingsAppearanceItems.postProcessingOptionItem(optionSelections), isTuiMode)
    val uiShadowsItem           = CommandRunnerSettingsAppearanceItems.uiShadowsOptionItem(optionSelections)
    val motionPresetItem        = CommandRunnerSettingsMotionItems.motionPresetOptionItem(optionSelections)
    val motionAccessibilityItem = CommandRunnerSettingsMotionItems.motionAccessibilityOptionItem(optionSelections)
    val commandRunnerFade       = CommandRunnerSettingsMotionItems.commandRunnerFadeOptionItem(optionSelections)
    val uiAnimationItem         = CommandRunnerSettingsMotionItems.uiAnimationOptionItem(optionSelections)
    val renderFpsItem           = CommandRunnerSettingsMotionItems.renderFpsOptionItem(optionSelections)
    val renderDamageGranularityItem =
      CommandRunnerSettingsMotionItems.renderDamageGranularityOptionItem(optionSelections)
    val editorTextItem      = CommandRunnerSettingsMotionItems.editorTextTransitionOptionItem(optionSelections)
    val panelOpenItem       = CommandRunnerSettingsMotionItems.panelOpenTransitionOptionItem(optionSelections)
    val panelCloseItem      = CommandRunnerSettingsMotionItems.panelCloseTransitionOptionItem(optionSelections)
    val commandRunnerReveal = CommandRunnerSettingsMotionItems.commandRunnerTransitionOptionItem(optionSelections)
    val markdownViewItem    = CommandRunnerSettingsItems.markdownViewOptionItem(optionSelections)
    val defaultDocumentItem = CommandRunnerSettingsItems.defaultDocumentModeOptionItem(optionSelections)
    val spellCheckItem      = CommandRunnerSettingsItems.spellCheckOptionItem(optionSelections)
    val textScaleModeItem   = CommandRunnerSettingsItems.textScaleModeOptionItem(optionSelections)
    val lineNumbersItem     = CommandRunnerSettingsTextDisplayItems.lineNumbersOptionItem(optionSelections)
    val wordCountItem       = CommandRunnerSettingsTextDisplayItems.wordCountOptionItem(optionSelections)
    val gutterItem          = CommandRunnerSettingsTextDisplayItems.gutterOptionItem(optionSelections)
    val lineWrapItem        = CommandRunnerSettingsTextDisplayItems.lineWrapOptionItem(optionSelections)
    val visualLineNavigationItem =
      CommandRunnerSettingsTextDisplayItems.visualLineNavigationOptionItem(optionSelections)
    val typewriterScrollingItem = CommandRunnerSettingsTextDisplayItems.typewriterScrollingOptionItem(optionSelections)
    val focusedTextBodyItem     = CommandRunnerSettingsTextDisplayItems.focusedTextBodyOptionItem(optionSelections)
    val contextualToolbarItem   = CommandRunnerSettingsTextDisplayItems.contextualToolbarOptionItem(optionSelections)
    val contextualToolbarDisplayItem =
      CommandRunnerSettingsTextDisplayItems.contextualToolbarDisplayModeOptionItem(optionSelections)
    val commandRunnerKeyHintsItem =
      CommandRunnerSettingsTextDisplayItems.commandRunnerKeyHintsOptionItem(optionSelections)
    val keymapItems = inputItems.filter(_.id.startsWith("keymap-"))
    val workspaceLayoutGroup = CommandSurfaceItem.GroupItem(
      id = "settings-workspace-layout",
      label = "Panels & Workspace",
      children = CommandRunnerSettingsPanelItems.workspaceLayoutItems(optionSelections),
      category = CommandCategory.Settings,
      hint = Some("Pin, focus, expand, and unpin panels")
    )
    // issue #1057: Next/Previous Bookmark, Navigate Back/Forward, etc. used to live here too, as fake "settings"
    // that just executed a one-shot action with no persisted value. They are ordinary CommandRegistry commands now
    // (already were, in fact -- this group's own construction just duplicated them), reachable only via the palette.
    // The one item that stays: authoring a document comment's text is a real input, not a one-shot action.
    val navigationGroup = CommandSurfaceItem.GroupItem(
      id = "settings-navigation",
      label = "Navigation",
      children = inputItems.filter(_.id == "document-comment"),
      category = CommandCategory.Settings,
      hint = Some("Author a document comment")
    )
    val textDisplayGroup = CommandSurfaceItem.GroupItem(
      id = "settings-text-display",
      label = "Text Display",
      children = List(
        lineNumbersItem,
        gutterItem,
        lineWrapItem,
        visualLineNavigationItem,
        typewriterScrollingItem,
        wordCountItem,
        focusedTextBodyItem,
        contextualToolbarItem,
        contextualToolbarDisplayItem
      ),
      category = CommandCategory.Settings,
      hint = Some("Line numbers, gutter, wrap, visual-line navigation, word count, focus, toolbar")
    )
    val motionInputIds = Set(
      "element-transition-speed-scale",
      "editor-text-speed-scale",
      "command-runner-speed-scale",
      "ui-speed-scale"
    )
    val advancedMotionInputIds =
      if optionSelections.get("motion-preset").contains(4) then Set("animation-duration", "animation-steps")
      else Set.empty[String]
    val windowSitterInputIds = Set(
      "window-sitter-frames",
      "window-sitter-active-ticks",
      "window-sitter-fast-active-ticks",
      "window-sitter-fast-threshold-ms"
    )
    val animationGroup = CommandSurfaceItem.GroupItem(
      id = "settings-animation",
      label = "Motion & Animation",
      children = List(
        motionAccessibilityItem,
        motionPresetItem,
        editorTextItem,
        panelOpenItem,
        panelCloseItem,
        commandRunnerReveal,
        commandRunnerFade,
        uiAnimationItem
      ) ++ inputItems.filter(item => item.id == "cursor-speed-scale" || motionInputIds.contains(item.id)) ++
        List(windowSitterEnabledItem, windowSitterActionItem) ++
        inputItems.filter(item => windowSitterInputIds.contains(item.id) || advancedMotionInputIds.contains(item.id)),
      category = CommandCategory.Settings,
      hint = Some("Reveal style, timing, speed, and window sitter")
    )
    val cursorGroup = CommandSurfaceItem.GroupItem(
      id = "settings-cursor",
      label = "Cursor",
      children = List(cursorModeItem) ++ cursorInfoBarItems ++ List(cursorInfoPlacement),
      category = CommandCategory.Settings,
      hint = Some("Cursor style, info bar, placement")
    )
    val surfaceAppearanceGroup = CommandSurfaceItem.GroupItem(
      id = "settings-surface-appearance",
      label = "Surface Appearance",
      children = List(backgroundStyleItem, materialPresetItem, postProcessingItem, uiShadowsItem) ++ inputItems.filter(
        _.id == "blur-radius"
      ),
      category = CommandCategory.Settings,
      hint = Some("Background, material, blur, and effects")
    )
    val interfaceLayoutGroup = CommandSurfaceItem.GroupItem(
      id = "settings-interface-layout",
      label = "Interface Layout",
      children = List(interfaceDensityItem, windowChromeItem, commandRunnerKeyHintsItem) ++ inputItems.filter(item =>
        item.id == "ui-element-gap" ||
          item.id == "ui-corner-radius" ||
          item.id == "ui-outline-thickness" ||
          item.id == "command-runner-visible-rows" ||
          item.id == "command-runner-item-gap-rows" ||
          item.id == "command-runner-cursor-gap-rows"
      ),
      category = CommandCategory.Settings,
      hint = Some("Density, spacing, window chrome, command rows, key hints")
    )
    val renderingGroup = CommandSurfaceItem.GroupItem(
      id = "settings-rendering",
      label = "Rendering",
      children = List(renderFpsItem, renderDamageGranularityItem),
      category = CommandCategory.Settings,
      hint = Some("Render loop cadence and performance")
    )
    val textAreaGroup = CommandSurfaceItem.GroupItem(
      id = "settings-text-area",
      label = "Text Area",
      children = inputItems.filter(item =>
        item.id == "text-area-left" ||
          item.id == "text-area-right" ||
          item.id == "text-area-top" ||
          item.id == "text-area-bottom"
      ),
      category = CommandCategory.Settings,
      hint = Some("Resize editor margins")
    )
    val codeFontGroup = CommandSurfaceItem.GroupItem(
      id = "settings-code-font",
      label = "Code Font",
      children = List(
        CommandRunnerSettingsItems.codeFontGroupItem(optionSelections, fontFamilies.monospace),
        CommandRunnerSettingsItems.codeLigaturesOptionItem(optionSelections)
      ) ++ inputItems.filter(_.id == "code-font-size"),
      category = CommandCategory.Settings,
      hint = Some(inertInTuiHint("Family, size, ligatures", isTuiMode))
    )
    val proseFontGroup = CommandSurfaceItem.GroupItem(
      id = "settings-prose-font",
      label = "Prose Font",
      children = List(
        CommandRunnerSettingsItems.textFontGroupItem(optionSelections, fontFamilies.text),
        CommandRunnerSettingsItems.textLigaturesOptionItem(optionSelections)
      ) ++ inputItems.filter(_.id == "text-font-size"),
      category = CommandCategory.Settings,
      hint = Some(inertInTuiHint("Family, size, ligatures", isTuiMode))
    )
    val richTextGroup = CommandSurfaceItem.GroupItem(
      id = "settings-rich-text",
      label = "Rich Text",
      children = inputItems.filter(_.id.startsWith("rich-text-")),
      category = CommandCategory.Settings,
      hint = Some("Selection family, size, colour")
    )
    val uiFontGroup = CommandSurfaceItem.GroupItem(
      id = "settings-ui-font",
      label = "UI Font",
      children = List(
        CommandRunnerSettingsItems.uiFontGroupItem(optionSelections, fontFamilies.ui),
        CommandRunnerSettingsItems.uiLigaturesOptionItem(optionSelections)
      ) ++ inputItems.filter(_.id == "ui-font-size"),
      category = CommandCategory.Settings,
      hint = Some(inertInTuiHint("Family, size, ligatures", isTuiMode))
    )
    val textScaleGroup = CommandSurfaceItem.GroupItem(
      id = "settings-text-scale",
      label = "Text Scale",
      children = List(textScaleModeItem) ++ inputItems.filter(_.id == "text-scale"),
      category = CommandCategory.Settings,
      hint = Some("Adapt all text to display scale")
    )
    val documentDefaultsGroup = CommandSurfaceItem.GroupItem(
      id = "settings-document-defaults",
      label = "Document Defaults",
      children = List(defaultDocumentItem, markdownViewItem),
      category = CommandCategory.Settings,
      hint = Some("New document mode and previews")
    )
    val spellCheckGroup = CommandSurfaceItem.GroupItem(
      id = "settings-spellcheck",
      label = "Spell Check",
      children = List(spellCheckItem) ++ inputItems.filter(item =>
        item.id == "spellcheck-languages" || item.id == "spellcheck-dictionaries" || item.id == "spellcheck-words"
      ),
      category = CommandCategory.Settings,
      hint = Some("Enable, languages, dictionaries, accepted words")
    )
    val keymapGroup = CommandSurfaceItem.GroupItem(
      id = "settings-keymap",
      label = "Keymap",
      children = keymapItems,
      category = CommandCategory.Settings,
      hint = Some("Inspect and edit bindings")
    )
    val documentWritingGroup = CommandSurfaceItem.GroupItem(
      id = "settings-document-writing",
      label = "Document Writing",
      children = List(navigationGroup, documentDefaultsGroup, richTextGroup, spellCheckGroup),
      category = CommandCategory.Settings,
      hint = Some("Comments, previews, styling, and spelling")
    )
    val editorViewGroup = CommandSurfaceItem.GroupItem(
      id = "settings-editor-view",
      label = "Editor View",
      children = List(textDisplayGroup, textAreaGroup, textScaleGroup),
      category = CommandCategory.Settings,
      hint = Some("Wrap, gutters, margins, display scale")
    )
    val typographyGroup = CommandSurfaceItem.GroupItem(
      id = "settings-typography",
      label = "Typography",
      children = List(proseFontGroup, codeFontGroup, uiFontGroup),
      category = CommandCategory.Settings,
      hint = Some(inertInTuiHint("Typefaces for prose, code, and interface", isTuiMode))
    )
    val appearanceMotionGroup = CommandSurfaceItem.GroupItem(
      id = "settings-appearance-motion",
      label = "Appearance & Motion",
      children = List(cursorGroup, surfaceAppearanceGroup, interfaceLayoutGroup, renderingGroup, animationGroup),
      category = CommandCategory.Settings,
      hint = Some("Visual styling, spacing, and movement")
    )
    val editingPreset = presetEditContextName(optionSelections, uiPresetPreviews, editingPresetName)
    val presetInputItems =
      inputItems.filter(_.id.startsWith("ui-preset-")).map(withPresetInputContext(_, editingPreset))
    val createPresetItems = presetInputItems.filter(_.id == "ui-preset-save-as-new")
    val renamePresetItems = presetInputItems.filter(_.id == "ui-preset-rename")
    val presetActionItems = presetInputItems.filter(item =>
      item.id == "ui-preset-apply" ||
        item.id == "ui-preset-overwrite" ||
        item.id == "ui-preset-duplicate" ||
        item.id == "ui-preset-delete" ||
        item.id == "ui-preset-reset"
    )
    val presetNameGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-name",
      label = "Name",
      children = renamePresetItems,
      category = CommandCategory.Settings,
      hint = Some("Rename this preset")
    )
    val presetActionsGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-actions",
      label = "Preset Actions",
      children = presetActionItems,
      category = CommandCategory.Settings,
      hint = Some("Apply, overwrite, duplicate, delete, or reset")
    )
    val createPresetNameGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-create-name",
      label = "Name",
      children = createPresetItems,
      category = CommandCategory.Settings,
      hint = Some("Save the current workspace as a new preset")
    )
    // issue #1058: editing a preset used to walk a hand-maintained parallel tree of clone groups (Active Panels,
    // Theme & Surface > Surface Material, Animations > Cursor/Text Entry/UI Surface Motion, Fonts > Editor/Code/UI
    // Typography, Document Defaults > New Documents/Markdown Preview/Spelling) that re-sliced and re-labelled the
    // exact same canonical items built above under new ids, several levels deeper than the equivalent top-level
    // screen. Editing a preset already reads and writes the same live settings as the top-level Settings screens
    // (see `presetEditContextName`/`CommandRunner.editingPresetName` -- explicit Apply/Overwrite/Reset actions are
    // what move values between live config and a stored preset), so "scoped settings screens" means reusing those
    // same canonical groups verbatim -- only their id is retagged (`settings-preset-*`) so they remain addressable
    // as distinct pages from their top-level counterparts in the same navigation tree.
    val presetScopedGroups = List(
      workspaceLayoutGroup.copy(id = "settings-preset-workspace-layout"),
      surfaceAppearanceGroup.copy(id = "settings-preset-surface-appearance"),
      cursorGroup.copy(id = "settings-preset-cursor"),
      animationGroup.copy(id = "settings-preset-animation"),
      proseFontGroup.copy(id = "settings-preset-prose-font"),
      codeFontGroup.copy(id = "settings-preset-code-font"),
      uiFontGroup.copy(id = "settings-preset-ui-font"),
      documentDefaultsGroup.copy(id = "settings-preset-document-defaults"),
      spellCheckGroup.copy(id = "settings-preset-spellcheck")
    )
    val selectPresetGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-select",
      label = "Select Preset",
      children = List(CommandRunnerSettingsItems.uiPresetSelectOptionItem(uiPresetPreviews, optionSelections)),
      category = CommandCategory.Settings,
      hint = Some("Browse available presets")
    )
    val createPresetGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-create",
      label = "Create New Preset",
      children = createPresetNameGroup :: presetScopedGroups,
      category = CommandCategory.Settings,
      hint = Some("Start from current workspace settings")
    )
    val editPresetGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-edit",
      label = editingPreset.fold("Edit Preset")(name => s"Edit Preset: $name"),
      children = List(presetNameGroup, presetActionsGroup) ++ presetScopedGroups,
      category = CommandCategory.Settings,
      hint = Some(editingPreset.fold("Document, layout, typography, motion")(name => s"Editing $name"))
    )
    val uiPresetsGroup = CommandSurfaceItem.GroupItem(
      id = "settings-ui-presets",
      label = "UI Presets",
      children = List(selectPresetGroup, createPresetGroup, editPresetGroup),
      category = CommandCategory.Settings,
      hint = Some("Save or apply named layouts")
    )
    val accessibilityGroup = CommandSurfaceItem.GroupItem(
      id = "settings-accessibility",
      label = "Accessibility",
      children = List(motionAccessibilityItem),
      category = CommandCategory.Settings,
      hint = Some("Motion accessibility and reading comfort")
    )
    val performanceGroup = CommandSurfaceItem.GroupItem(
      id = "settings-performance",
      label = "Performance",
      children = List(visualFlairLevelItem, companionSpriteEnabledItem),
      category = CommandCategory.Settings,
      hint = Some("Trim purely decorative extras on a slow link or a battery-powered machine")
    )
    val appModeGroup = CommandSurfaceItem.GroupItem(
      id = "settings-app-mode",
      label = "App Mode",
      children = List(
        CommandRunnerSettingsItems.appModeOptionItem(optionSelections),
        CommandRunnerSettingsItems.showAllSettingsOptionItem(optionSelections)
      ),
      category = CommandCategory.Settings,
      hint = Some("Code or prose workspace -- filters which settings are shown below")
    )
    // Always shown regardless of mode, otherwise a user in prose mode could never find the toggle back to code mode.
    val showProseSettings = showAllSettingsRegardlessOfMode || appMode == AppMode.Prose
    val showCodeSettings  = showAllSettingsRegardlessOfMode || appMode == AppMode.Code
    val filteredTypographyGroup = typographyGroup.copy(children = typographyGroup.children.filter {
      case item if item.id == "settings-prose-font" => showProseSettings
      case item if item.id == "settings-code-font"  => showCodeSettings
      case _                                        => true
    })
    List(appModeGroup, workspaceLayoutGroup) ++
      (if showProseSettings then List(documentWritingGroup) else Nil) ++
      List(
        editorViewGroup,
        filteredTypographyGroup,
        appearanceMotionGroup,
        uiPresetsGroup,
        accessibilityGroup,
        performanceGroup,
        keymapGroup
      )

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

  private[command] def presetEditContextName(
    optionSelections: Map[String, Int],
    uiPresetPreviews: List[UiPreset.Preview],
    editingPresetName: Option[String]
  ): Option[String] =
    editingPresetName
      .map(_.trim)
      .filter(_.nonEmpty)
      .orElse(
        optionSelections
          .get("ui-preset-custom")
          .flatMap(index => uiPresetPreviews.lift(index))
          .map(_.name)
      )
      .orElse(
        optionSelections
          .get("ui-preset-built-in")
          .flatMap(index => UiPreset.builtIns.lift(index))
          .map(_.name)
      )
      .orElse(UiPreset.builtIns.headOption.map(_.name))

  private def withPresetInputContext(
    item: CommandSurfaceItem.InputItem,
    presetName: Option[String]
  ): CommandSurfaceItem.InputItem =
    presetName match
      case Some(name) =>
        item.id match
          case "ui-preset-overwrite" | "ui-preset-apply" | "ui-preset-delete" | "ui-preset-reset" =>
            item.copy(currentValue = name)
          case "ui-preset-duplicate" | "ui-preset-rename" =>
            item.copy(currentValue = s"$name -> ")
          case _ =>
            item
      case None =>
        item
