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
    val animationItem        = CommandRunnerSettingsItems.animationOptionItem(optionSelections)
    val cursorModeItem       = CommandRunnerSettingsItems.cursorModeOptionItem(optionSelections)
    val cursorInfoBarItem    = CommandRunnerSettingsItems.cursorInfoBarOptionItem(optionSelections)
    val cursorInfoPlacement  = CommandRunnerSettingsItems.cursorInfoBarPlacementOptionItem(optionSelections)
    val backgroundStyleItem  = CommandRunnerSettingsItems.backgroundStyleOptionItem(optionSelections)
    val interfaceDensityItem = CommandRunnerSettingsItems.interfaceDensityOptionItem(optionSelections)
    val materialPresetItem   = CommandRunnerSettingsItems.materialPresetOptionItem(optionSelections)
    val motionPresetItem     = CommandRunnerSettingsItems.motionPresetOptionItem(optionSelections)
    val commandRunnerFade    = CommandRunnerSettingsItems.commandRunnerFadeOptionItem(optionSelections)
    val uiAnimationItem      = CommandRunnerSettingsItems.uiAnimationOptionItem(optionSelections)
    val renderFpsItem        = CommandRunnerSettingsItems.renderFpsOptionItem(optionSelections)
    val editorTextItem       = CommandRunnerSettingsItems.editorTextTransitionOptionItem(optionSelections)
    val panelOpenItem        = CommandRunnerSettingsItems.panelOpenTransitionOptionItem(optionSelections)
    val panelCloseItem       = CommandRunnerSettingsItems.panelCloseTransitionOptionItem(optionSelections)
    val markdownViewItem     = CommandRunnerSettingsItems.markdownViewOptionItem(optionSelections)
    val defaultDocumentItem  = CommandRunnerSettingsItems.defaultDocumentModeOptionItem(optionSelections)
    val spellCheckItem       = CommandRunnerSettingsItems.spellCheckOptionItem(optionSelections)
    val textScaleModeItem    = CommandRunnerSettingsItems.textScaleModeOptionItem(optionSelections)
    val lineNumbersItem      = CommandRunnerSettingsItems.lineNumbersOptionItem(optionSelections)
    val gutterItem           = CommandRunnerSettingsItems.gutterOptionItem(optionSelections)
    val lineWrapItem         = CommandRunnerSettingsItems.lineWrapOptionItem(optionSelections)
    val keymapItems          = inputItems.filter(_.id.startsWith("keymap-"))
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
      children = List(lineNumbersItem, gutterItem, lineWrapItem),
      category = CommandCategory.Settings,
      hint = Some("Line numbers, gutter, wrap")
    )
    val motionInputIds = Set(
      "animation-duration",
      "animation-steps",
      "element-transition-speed-scale",
      "editor-text-speed-scale",
      "command-runner-speed-scale",
      "ui-speed-scale"
    )
    val animationGroup = CommandSurfaceItem.GroupItem(
      id = "settings-animation",
      label = "Motion & Animation",
      children = List(
        motionPresetItem,
        animationItem,
        editorTextItem,
        panelOpenItem,
        panelCloseItem,
        commandRunnerFade,
        uiAnimationItem,
        renderFpsItem
      ) ++ inputItems.filter(item => motionInputIds.contains(item.id)),
      category = CommandCategory.Settings,
      hint = Some("Reveal style, timing, and speed")
    )
    val cursorGroup = CommandSurfaceItem.GroupItem(
      id = "settings-cursor",
      label = "Cursor",
      children = List(cursorModeItem, cursorInfoBarItem, cursorInfoPlacement) ++
        inputItems.filter(_.id == "cursor-speed-scale"),
      category = CommandCategory.Settings,
      hint = Some("Cursor style, info bar, placement")
    )
    val surfaceAppearanceGroup = CommandSurfaceItem.GroupItem(
      id = "settings-surface-appearance",
      label = "Surface Appearance",
      children = List(backgroundStyleItem, materialPresetItem) ++ inputItems.filter(_.id == "blur-radius"),
      category = CommandCategory.Settings,
      hint = Some("Background, material, and blur")
    )
    val interfaceLayoutGroup = CommandSurfaceItem.GroupItem(
      id = "settings-interface-layout",
      label = "Interface Layout",
      children = List(interfaceDensityItem) ++ inputItems.filter(item =>
        item.id == "ui-element-gap" || item.id == "ui-corner-radius" || item.id == "command-runner-visible-rows"
      ),
      category = CommandCategory.Settings,
      hint = Some("Density, spacing, command rows")
    )
    val textAreaGroup = CommandSurfaceItem.GroupItem(
      id = "settings-text-area",
      label = "Text Area",
      children = inputItems.filter(item => item.id == "text-area-left" || item.id == "text-area-right"),
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
      children = List(defaultDocumentItem, markdownViewItem, spellCheckItem),
      category = CommandCategory.Settings,
      hint = Some("New document mode, previews, spelling")
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
      children = List(cursorGroup, surfaceAppearanceGroup, interfaceLayoutGroup, animationGroup),
      category = CommandCategory.Settings,
      hint = Some("Visual styling, spacing, and movement")
    )
    val editingPreset = presetEditContextName(optionSelections, uiPresetPreviews, editingPresetName)
    val presetInputItems =
      inputItems.filter(_.id.startsWith("ui-preset-")).map(withPresetInputContext(_, editingPreset))
    val createPresetItems    = presetInputItems.filter(_.id == "ui-preset-create")
    val remainingPresetItems = presetInputItems.filterNot(_.id == "ui-preset-create")
    val presetNameGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-name",
      label = "Name",
      children = remainingPresetItems,
      category = CommandCategory.Settings,
      hint = Some("Save, apply, duplicate, rename, delete, reset")
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
    val presetAnimationsGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-animations",
      label = "Animations",
      children = List(cursorGroup, animationGroup),
      category = CommandCategory.Settings,
      hint = Some("Cursor, text entry, and UI motion")
    )
    val presetFontsGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-fonts",
      label = "Fonts",
      children = List(proseFontGroup, codeFontGroup, uiFontGroup),
      category = CommandCategory.Settings,
      hint = Some("Text entry, code, and UI fonts")
    )
    val presetDocumentDefaultsGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-document-defaults",
      label = "Document Defaults",
      children = List(defaultDocumentItem, markdownGroup, spellCheckGroup),
      category = CommandCategory.Settings,
      hint = Some("Default mode, Markdown view, spelling")
    )
    val presetThemeGroup = CommandSurfaceItem.GroupItem(
      id = "settings-preset-theme",
      label = "Theme & Surface",
      children = List(surfaceAppearanceGroup) ++ CommandRunnerSettingsItems.themeItems,
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
      children = presetNameGroup :: presetEditingSections,
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
    List(
      workspaceLayoutGroup,
      documentWritingGroup,
      editorViewGroup,
      typographyGroup,
      appearanceMotionGroup,
      uiPresetsGroup,
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
          case "ui-preset-save" | "ui-preset-apply" | "ui-preset-delete" | "ui-preset-reset" =>
            item.copy(currentValue = name)
          case "ui-preset-duplicate" | "ui-preset-rename" =>
            item.copy(currentValue = s"$name -> ")
          case _ =>
            item
      case None =>
        item
