package com.serenity.command

import com.serenity.ui.presets.UiPreset

/** Builds command-runner settings groups from schema rows and current option selections. */
object CommandRunnerSettingsGroups:

  /** Derive the nested settings menu without depending on command-runner navigation state. */
  def build(
    optionSelections: Map[String, Int],
    inputItems: List[CommandSurfaceItem.InputItem],
    uiPresetPreviews: List[UiPreset.Preview],
    editingPresetName: Option[String]
  ): List[CommandSurfaceItem.GroupItem] =
    val cursorModeItem          = CommandRunnerSettingsItems.cursorModeOptionItem(optionSelections)
    val cursorInfoBarItem       = CommandRunnerSettingsItems.cursorInfoBarOptionItem(optionSelections)
    val cursorInfoPlacement     = CommandRunnerSettingsItems.cursorInfoBarPlacementOptionItem(optionSelections)
    val backgroundStyleItem     = CommandRunnerSettingsItems.backgroundStyleOptionItem(optionSelections)
    val interfaceDensityItem    = CommandRunnerSettingsItems.interfaceDensityOptionItem(optionSelections)
    val windowChromeItem        = CommandRunnerSettingsItems.windowChromeOptionItem(optionSelections)
    val windowSitterEnabledItem = CommandRunnerSettingsItems.windowSitterEnabledOptionItem(optionSelections)
    val windowSitterActionItem  = CommandRunnerSettingsItems.windowSitterActionOptionItem(optionSelections)
    val materialPresetItem      = CommandRunnerSettingsItems.materialPresetOptionItem(optionSelections)
    val postProcessingItem      = CommandRunnerSettingsItems.postProcessingOptionItem(optionSelections)
    val uiShadowsItem           = CommandRunnerSettingsItems.uiShadowsOptionItem(optionSelections)
    val motionPresetItem        = CommandRunnerSettingsItems.motionPresetOptionItem(optionSelections)
    val motionAccessibilityItem = CommandRunnerSettingsItems.motionAccessibilityOptionItem(optionSelections)
    val commandRunnerFade       = CommandRunnerSettingsItems.commandRunnerFadeOptionItem(optionSelections)
    val uiAnimationItem         = CommandRunnerSettingsItems.uiAnimationOptionItem(optionSelections)
    val renderFpsItem           = CommandRunnerSettingsItems.renderFpsOptionItem(optionSelections)
    val editorTextItem          = CommandRunnerSettingsItems.editorTextTransitionOptionItem(optionSelections)
    val panelOpenItem           = CommandRunnerSettingsItems.panelOpenTransitionOptionItem(optionSelections)
    val panelCloseItem          = CommandRunnerSettingsItems.panelCloseTransitionOptionItem(optionSelections)
    val commandRunnerReveal     = CommandRunnerSettingsItems.commandRunnerTransitionOptionItem(optionSelections)
    val markdownViewItem        = CommandRunnerSettingsItems.markdownViewOptionItem(optionSelections)
    val defaultDocumentItem     = CommandRunnerSettingsItems.defaultDocumentModeOptionItem(optionSelections)
    val spellCheckItem          = CommandRunnerSettingsItems.spellCheckOptionItem(optionSelections)
    val textScaleModeItem       = CommandRunnerSettingsItems.textScaleModeOptionItem(optionSelections)
    val lineNumbersItem         = CommandRunnerSettingsItems.lineNumbersOptionItem(optionSelections)
    val gutterItem              = CommandRunnerSettingsItems.gutterOptionItem(optionSelections)
    val lineWrapItem            = CommandRunnerSettingsItems.lineWrapOptionItem(optionSelections)
    val focusedTextBodyItem     = CommandRunnerSettingsItems.focusedTextBodyOptionItem(optionSelections)
    val contextualToolbarItem   = CommandRunnerSettingsItems.contextualToolbarOptionItem(optionSelections)
    val contextualToolbarDisplayItem =
      CommandRunnerSettingsItems.contextualToolbarDisplayModeOptionItem(optionSelections)
    val keymapItems = inputItems.filter(_.id.startsWith("keymap-"))
    val workspaceLayoutGroup = CommandSurfaceItem.GroupItem(
      id = "settings-workspace-layout",
      label = "Panels & Workspace",
      children = CommandRunnerSettingsItems.workspaceLayoutItems(optionSelections),
      category = CommandCategory.Settings,
      hint = Some("Pin, focus, expand, and unpin panels")
    )
    val navigationGroup = CommandSurfaceItem.GroupItem(
      id = "settings-navigation",
      label = "Navigation",
      children = inputItems.filter(_.id == "document-comment") ++ CommandRunnerSettingsItems.navigationItems,
      category = CommandCategory.Settings,
      hint = Some("Comments, bookmarks, headings, history")
    )
    val textDisplayGroup = CommandSurfaceItem.GroupItem(
      id = "settings-text-display",
      label = "Text Display",
      children = List(
        lineNumbersItem,
        gutterItem,
        lineWrapItem,
        focusedTextBodyItem,
        contextualToolbarItem,
        contextualToolbarDisplayItem
      ),
      category = CommandCategory.Settings,
      hint = Some("Line numbers, gutter, wrap, focus, toolbar")
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
      children = List(cursorModeItem, cursorInfoBarItem, cursorInfoPlacement),
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
      children = List(interfaceDensityItem, windowChromeItem) ++ inputItems.filter(item =>
        item.id == "ui-element-gap" ||
          item.id == "ui-corner-radius" ||
          item.id == "ui-outline-thickness" ||
          item.id == "command-runner-visible-rows" ||
          item.id == "command-runner-item-gap-rows" ||
          item.id == "command-runner-cursor-gap-rows"
      ),
      category = CommandCategory.Settings,
      hint = Some("Density, spacing, window chrome, command rows")
    )
    val renderingGroup = CommandSurfaceItem.GroupItem(
      id = "settings-rendering",
      label = "Rendering",
      children = List(renderFpsItem),
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
        CommandRunnerSettingsItems.codeFontGroupItem(optionSelections),
        CommandRunnerSettingsItems.codeLigaturesOptionItem(optionSelections)
      ) ++ inputItems.filter(_.id == "code-font-size"),
      category = CommandCategory.Settings,
      hint = Some("Family, size, ligatures")
    )
    val proseFontGroup = CommandSurfaceItem.GroupItem(
      id = "settings-prose-font",
      label = "Prose Font",
      children = List(
        CommandRunnerSettingsItems.textFontGroupItem(optionSelections),
        CommandRunnerSettingsItems.textLigaturesOptionItem(optionSelections)
      ) ++ inputItems.filter(_.id == "text-font-size"),
      category = CommandCategory.Settings,
      hint = Some("Family, size, ligatures")
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
        CommandRunnerSettingsItems.uiFontGroupItem(optionSelections),
        CommandRunnerSettingsItems.uiLigaturesOptionItem(optionSelections)
      ) ++ inputItems.filter(_.id == "ui-font-size"),
      category = CommandCategory.Settings,
      hint = Some("Family, size, ligatures")
    )
    val textScaleGroup = CommandSurfaceItem.GroupItem(
      id = "settings-text-scale",
      label = "Text Scale",
      children = List(textScaleModeItem) ++ inputItems.filter(_.id == "text-scale"),
      category = CommandCategory.Settings,
      hint = Some("Adapt all text to display scale")
    )
    val markdownGroup = CommandSurfaceItem.GroupItem(
      id = "settings-markdown",
      label = "Markdown",
      children = List(markdownViewItem),
      category = CommandCategory.Settings,
      hint = Some("Source, split preview, or inline lens")
    )
    val documentDefaultsGroup = CommandSurfaceItem.GroupItem(
      id = "settings-document-defaults",
      label = "Document Defaults",
      children = List(defaultDocumentItem, markdownViewItem),
      category = CommandCategory.Settings,
      hint = Some("New document mode and previews")
    )
    val languageGroup = CommandSurfaceItem.GroupItem(
      id = "settings-language",
      label = "Current Buffer Language",
      children = CommandRunnerSettingsItems.languageItems,
      category = CommandCategory.Settings,
      hint = Some("Set syntax mode for the active buffer")
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
      children = List(navigationGroup, documentDefaultsGroup, languageGroup, richTextGroup, spellCheckGroup),
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
      hint = Some("Typefaces for prose, code, and interface")
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
    val createPresetItems = presetInputItems.filter(_.id == "ui-preset-create")
    val renamePresetItems = presetInputItems.filter(_.id == "ui-preset-rename")
    val presetActionItems = presetInputItems.filter(item =>
      item.id == "ui-preset-save" ||
        item.id == "ui-preset-discard" ||
        item.id == "ui-preset-cancel-switch" ||
        item.id == "ui-preset-apply" ||
        item.id == "ui-preset-edit" ||
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
      hint = Some("Save, discard, cancel, apply, duplicate, delete, or reset")
    )
    val createPresetNameGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-create-name",
      label = "Name",
      children = createPresetItems,
      category = CommandCategory.Settings,
      hint = Some("Name and save the current workspace")
    )
    val activePanelsGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-active-panels",
      label = "Active Panels",
      children = workspaceLayoutGroup.children,
      category = CommandCategory.Settings,
      hint = Some("Choose pinned panels and panel actions")
    )
    val presetCursorMotionGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-cursor-motion",
      label = "Cursor Motion",
      children = List(cursorModeItem) ++
        inputItems.filter(_.id == "cursor-speed-scale") ++
        List(cursorInfoBarItem, cursorInfoPlacement),
      category = CommandCategory.Settings,
      hint = Some("Cursor style, speed, and info placement")
    )
    val presetTextEntryMotionGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-text-entry-motion",
      label = "Text Entry Motion",
      children = List(editorTextItem) ++ inputItems.filter(item =>
        item.id == "editor-text-speed-scale" || item.id == "element-transition-speed-scale"
      ),
      category = CommandCategory.Settings,
      hint = Some("Editor text reveal and typing speed")
    )
    val presetUiSurfaceMotionGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-ui-surface-motion",
      label = "UI Surface Motion",
      children = List(
        motionPresetItem,
        panelOpenItem,
        panelCloseItem,
        commandRunnerReveal,
        commandRunnerFade,
        uiAnimationItem
      ) ++ inputItems.filter(item =>
        item.id == "animation-duration" ||
          item.id == "animation-steps" ||
          item.id == "command-runner-speed-scale" ||
          item.id == "ui-speed-scale"
      ),
      category = CommandCategory.Settings,
      hint = Some("Panels, command runner, overlays, and render cadence")
    )
    val presetAnimationsGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-animations",
      label = "Animations",
      children = List(presetCursorMotionGroup, presetTextEntryMotionGroup, presetUiSurfaceMotionGroup),
      category = CommandCategory.Settings,
      hint = Some("Cursor, text entry, and UI motion")
    )
    val presetEditorTypographyGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-editor-typography",
      label = "Editor Typography",
      children = proseFontGroup.children,
      category = CommandCategory.Settings,
      hint = Some("Prose editor family, size, and ligatures")
    )
    val presetCodeTypographyGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-code-typography",
      label = "Code Typography",
      children = codeFontGroup.children,
      category = CommandCategory.Settings,
      hint = Some("Code editor family, size, and ligatures")
    )
    val presetUiTypographyGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-ui-typography",
      label = "UI Typography",
      children = uiFontGroup.children,
      category = CommandCategory.Settings,
      hint = Some("Interface family, size, and ligatures")
    )
    val presetFontsGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-fonts",
      label = "Fonts",
      children = List(presetEditorTypographyGroup, presetCodeTypographyGroup, presetUiTypographyGroup),
      category = CommandCategory.Settings,
      hint = Some("Editor, code, and interface typography")
    )
    val presetNewDocumentsGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-new-documents",
      label = "New Documents",
      children = List(defaultDocumentItem),
      category = CommandCategory.Settings,
      hint = Some("Default mode for new buffers")
    )
    val presetMarkdownPreviewGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-markdown-preview",
      label = "Markdown Preview",
      children = markdownGroup.children,
      category = CommandCategory.Settings,
      hint = Some("Source, split preview, or inline lens")
    )
    val presetSpellingGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-spelling",
      label = "Spelling",
      children = spellCheckGroup.children,
      category = CommandCategory.Settings,
      hint = Some("Enable, languages, dictionaries, accepted words")
    )
    val presetDocumentDefaultsGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-document-defaults",
      label = "Document Defaults",
      children = List(presetNewDocumentsGroup, presetMarkdownPreviewGroup, presetSpellingGroup),
      category = CommandCategory.Settings,
      hint = Some("Default mode, Markdown view, spelling")
    )
    val presetThemeSelectionGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-theme-selection",
      label = "Theme Selection",
      children = CommandRunnerSettingsItems.themeItems,
      category = CommandCategory.Settings,
      hint = Some("Choose, create, toggle, or reload themes")
    )
    val presetSurfaceMaterialGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-surface-material",
      label = "Surface Material",
      children = surfaceAppearanceGroup.children,
      category = CommandCategory.Settings,
      hint = Some("Background, material, and blur")
    )
    val presetThemeGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-theme",
      label = "Theme & Surface",
      children = List(presetThemeSelectionGroup, presetSurfaceMaterialGroup),
      category = CommandCategory.Settings,
      hint = Some("Theme, material, and background")
    )
    val presetEditingSections =
      List(activePanelsGroup, presetThemeGroup, presetAnimationsGroup, presetFontsGroup, presetDocumentDefaultsGroup)
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
      children = createPresetNameGroup :: presetEditingSections,
      category = CommandCategory.Settings,
      hint = Some("Start from current workspace settings")
    )
    val editPresetGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-edit",
      label = editingPreset.fold("Edit Preset")(name => s"Edit Preset: $name"),
      children = List(presetNameGroup, presetActionsGroup) ++ presetEditingSections,
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
    List(
      workspaceLayoutGroup,
      documentWritingGroup,
      editorViewGroup,
      typographyGroup,
      appearanceMotionGroup,
      uiPresetsGroup,
      accessibilityGroup,
      keymapGroup
    )

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
          case "ui-preset-save" | "ui-preset-apply" | "ui-preset-edit" | "ui-preset-delete" | "ui-preset-reset" =>
            item.copy(currentValue = name)
          case "ui-preset-duplicate" | "ui-preset-rename" =>
            item.copy(currentValue = s"$name -> ")
          case _ =>
            item
      case None =>
        item
