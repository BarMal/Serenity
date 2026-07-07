package com.serenity.command

import com.serenity.animation.{AnimationConfig, TransitionKind}
import com.serenity.config.*
import com.serenity.lsp.config.LanguageId
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.TextScaleMode
import com.serenity.ui.layout.PanelPosition
import com.serenity.ui.presets.UiPreset

/** Builds command-runner settings option rows and static settings command rows. */
object CommandRunnerSettingsItems:

  private[command] def cursorModeOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "cursor-mode",
      label = "Cursor Style",
      options = List(
        CommandOption("Blink", CommandIntent.SetCursorMode(CursorMode.Blink)),
        CommandOption("Breathe", CommandIntent.SetCursorMode(CursorMode.Breathe))
      ),
      selectedIndex = optionSelections.getOrElse("cursor-mode", 0),
      category = CommandCategory.Settings,
      hint = Some("Blink or breathe")
    )

  private[command] def editorTextTransitionOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "editor-text-transition",
      label = "Text Reveal",
      options = List(
        CommandOption("Fade", CommandIntent.SetEditorInsertionTransitionKind(TransitionKind.Fade)),
        CommandOption("Typed", CommandIntent.SetEditorInsertionTransitionKind(TransitionKind.TypedText)),
        CommandOption("Directional", CommandIntent.SetEditorInsertionTransitionKind(TransitionKind.DirectionalSweep)),
        CommandOption(
          "Tandem",
          CommandIntent.SetEditorInsertionTransitionKind(TransitionKind.LineAndCharacterTandem)
        ),
        CommandOption("Off", CommandIntent.SetEditorInsertionTransitionKind(TransitionKind.Disabled))
      ),
      selectedIndex = optionSelections.getOrElse("editor-text-transition", 0),
      category = CommandCategory.Settings,
      hint = Some("Editor insertion reveal style")
    )

  private[command] def panelOpenTransitionOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    panelTransitionOptionItem(
      id = "panel-open-transition",
      label = "Panel Open Reveal",
      selectedIndex = optionSelections.getOrElse("panel-open-transition", 3),
      setIntent = CommandIntent.SetPanelOpenTransitionKind.apply,
      hint = "Pinned panel opening reveal style"
    )

  private[command] def panelCloseTransitionOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    panelTransitionOptionItem(
      id = "panel-close-transition",
      label = "Panel Close Reveal",
      selectedIndex = optionSelections.getOrElse("panel-close-transition", 0),
      setIntent = CommandIntent.SetPanelCloseTransitionKind.apply,
      hint = "Pinned panel closing reveal style"
    )

  private def panelTransitionOptionItem(
    id: String,
    label: String,
    selectedIndex: Int,
    setIntent: TransitionKind => CommandIntent,
    hint: String
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = id,
      label = label,
      options = List(
        CommandOption("Fade", setIntent(TransitionKind.Fade)),
        CommandOption("Directional", setIntent(TransitionKind.DirectionalSweep)),
        CommandOption("Tandem", setIntent(TransitionKind.LineAndCharacterTandem)),
        CommandOption("Outline", setIntent(TransitionKind.OutlineThenContent)),
        CommandOption("Off", setIntent(TransitionKind.Disabled))
      ),
      selectedIndex = selectedIndex,
      category = CommandCategory.Settings,
      hint = Some(hint)
    )

  private[command] def commandRunnerFadeOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "command-runner-fade",
      label = "Command Runner Fade",
      options = List(
        CommandOption("Off", CommandIntent.SetCommandRunnerAnimation(None)),
        CommandOption("Subtle", CommandIntent.SetCommandRunnerAnimation(AnimationConfig.subtle)),
        CommandOption("Smooth", CommandIntent.SetCommandRunnerAnimation(AnimationConfig.smooth)),
        CommandOption("Expressive", CommandIntent.SetCommandRunnerAnimation(AnimationConfig.quick))
      ),
      selectedIndex = optionSelections.getOrElse("command-runner-fade", 2),
      category = CommandCategory.Settings,
      hint = Some("Palette fade in and out")
    )

  private[command] def uiAnimationOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "ui-animation",
      label = "UI Animation",
      options = List(
        CommandOption("Off", CommandIntent.SetUiAnimation(None)),
        CommandOption("Subtle", CommandIntent.SetUiAnimation(AnimationConfig.subtle)),
        CommandOption("Smooth", CommandIntent.SetUiAnimation(AnimationConfig.smooth)),
        CommandOption("Expressive", CommandIntent.SetUiAnimation(AnimationConfig.quick))
      ),
      selectedIndex = optionSelections.getOrElse("ui-animation", 2),
      category = CommandCategory.Settings,
      hint = Some("Panels, overlays, and view transitions")
    )

  private[command] def renderFpsOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "render-fps",
      label = "Render FPS",
      options = List(
        CommandOption("30 FPS", CommandIntent.SetRenderFpsTarget(RenderFpsTarget.Fps30)),
        CommandOption("60 FPS", CommandIntent.SetRenderFpsTarget(RenderFpsTarget.Fps60)),
        CommandOption("90 FPS", CommandIntent.SetRenderFpsTarget(RenderFpsTarget.Fps90)),
        CommandOption("120 FPS", CommandIntent.SetRenderFpsTarget(RenderFpsTarget.Fps120)),
        CommandOption("Uncapped", CommandIntent.SetRenderFpsTarget(RenderFpsTarget.Uncapped))
      ),
      selectedIndex = optionSelections.getOrElse("render-fps", 1),
      category = CommandCategory.Settings,
      hint = Some("Render loop cadence")
    )

  private[command] def cursorInfoBarOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "cursor-info-bar",
      label = "Cursor Info Bar",
      options = List(
        CommandOption("Off", CommandIntent.SetCursorInfoBarMode(CursorInfoBarMode.Off)),
        CommandOption("Position", CommandIntent.SetCursorInfoBarMode(CursorInfoBarMode.Position)),
        CommandOption("Detailed", CommandIntent.SetCursorInfoBarMode(CursorInfoBarMode.Detailed))
      ),
      selectedIndex = optionSelections.getOrElse("cursor-info-bar", 0),
      category = CommandCategory.Settings,
      hint = Some("Off, position, or detailed")
    )

  private[command] def cursorInfoBarPlacementOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "cursor-info-bar-placement",
      label = "Cursor Info Placement",
      options = List(
        CommandOption("Floating", CommandIntent.SetCursorInfoBarPlacement(CursorInfoBarPlacement.Floating)),
        CommandOption("Pinned Bottom", CommandIntent.SetCursorInfoBarPlacement(CursorInfoBarPlacement.PinnedBottom))
      ),
      selectedIndex = optionSelections.getOrElse("cursor-info-bar-placement", 0),
      category = CommandCategory.Settings,
      hint = Some("Floating or pinned")
    )

  private[command] def backgroundStyleOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "background-style",
      label = "Background Style",
      options = List(
        CommandOption("Solid", CommandIntent.SetBackgroundStyle(BackgroundStyle.Solid)),
        CommandOption("Transparent", CommandIntent.SetBackgroundStyle(BackgroundStyle.Transparent)),
        CommandOption("Frosted", CommandIntent.SetBackgroundStyle(BackgroundStyle.Frosted)),
        CommandOption("Glass", CommandIntent.SetBackgroundStyle(BackgroundStyle.GlassLike))
      ),
      selectedIndex = optionSelections.getOrElse("background-style", 2),
      category = CommandCategory.Settings,
      hint = Some("Solid, transparent, frosted, or glass")
    )

  private[command] def interfaceDensityOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "interface-density",
      label = "Interface Density",
      options = List(
        CommandOption("Compact", CommandIntent.SetInterfaceDensity(InterfaceDensity.Compact)),
        CommandOption("Comfortable", CommandIntent.SetInterfaceDensity(InterfaceDensity.Comfortable)),
        CommandOption("Spacious", CommandIntent.SetInterfaceDensity(InterfaceDensity.Spacious))
      ),
      selectedIndex = optionSelections.getOrElse("interface-density", 1),
      category = CommandCategory.Settings,
      hint = Some("Compact, comfortable, or spacious")
    )

  private[command] def materialPresetOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "material-preset",
      label = "Material Preset",
      options = List(
        CommandOption("Solid", CommandIntent.SetMaterialPreset(MaterialPreset.Solid)),
        CommandOption("Clear", CommandIntent.SetMaterialPreset(MaterialPreset.Clear)),
        CommandOption("Frosted", CommandIntent.SetMaterialPreset(MaterialPreset.Frosted)),
        CommandOption("Crystal", CommandIntent.SetMaterialPreset(MaterialPreset.Crystal)),
        CommandOption("Custom", CommandIntent.SetMaterialPreset(MaterialPreset.Custom))
      ),
      selectedIndex = optionSelections.getOrElse("material-preset", 2),
      category = CommandCategory.Settings,
      hint = Some("Material baseline for panels and overlays")
    )

  private[command] def motionPresetOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "motion-preset",
      label = "Motion Preset",
      options = List(
        CommandOption("Reduced", CommandIntent.SetMotionPreset(MotionPreset.Reduced)),
        CommandOption("Subtle", CommandIntent.SetMotionPreset(MotionPreset.Subtle)),
        CommandOption("Smooth", CommandIntent.SetMotionPreset(MotionPreset.Smooth)),
        CommandOption("Expressive", CommandIntent.SetMotionPreset(MotionPreset.Expressive)),
        CommandOption("Custom", CommandIntent.SetMotionPreset(MotionPreset.Custom))
      ),
      selectedIndex = optionSelections.getOrElse("motion-preset", 2),
      category = CommandCategory.Settings,
      hint = Some("Animation baseline for UI motion")
    )

  private[command] def markdownViewOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "markdown-view",
      label = "Markdown View",
      options = List(
        CommandOption("Source", CommandIntent.SetMarkdownViewMode(MarkdownViewMode.Source)),
        CommandOption("Split Preview", CommandIntent.SetMarkdownViewMode(MarkdownViewMode.SplitPreview)),
        CommandOption("Inline Lens", CommandIntent.SetMarkdownViewMode(MarkdownViewMode.InlineLens))
      ),
      selectedIndex = optionSelections.getOrElse("markdown-view", 0),
      category = CommandCategory.Settings,
      hint = Some("Source, side preview, or inline editing lens")
    )

  private[command] def defaultDocumentModeOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "default-document-mode",
      label = "Default Document",
      options = List(
        CommandOption("Plain Text", CommandIntent.SetDefaultDocumentMode(DefaultDocumentMode.PlainText)),
        CommandOption("Markdown", CommandIntent.SetDefaultDocumentMode(DefaultDocumentMode.Markdown)),
        CommandOption("Rich Text", CommandIntent.SetDefaultDocumentMode(DefaultDocumentMode.RichText))
      ),
      selectedIndex = optionSelections.getOrElse("default-document-mode", 0),
      category = CommandCategory.Settings,
      hint = Some("Mode for newly-created documents")
    )

  private[command] def spellCheckOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "spellcheck-enabled",
      label = "Spell Check",
      options = List(
        CommandOption("Off", CommandIntent.SetSpellCheckEnabled(false)),
        CommandOption("On", CommandIntent.SetSpellCheckEnabled(true))
      ),
      selectedIndex = optionSelections.getOrElse("spellcheck-enabled", 0),
      category = CommandCategory.Settings,
      hint = Some("Check prose buffers")
    )

  private[command] def animationOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "animation-mode",
      label = "Animation Style",
      options = List(
        CommandOption("None", CommandIntent.SetAnimationMode(AnimationMode.None)),
        CommandOption("Subtle", CommandIntent.SetAnimationMode(AnimationMode.Subtle)),
        CommandOption("Full", CommandIntent.SetAnimationMode(AnimationMode.Smooth))
      ),
      selectedIndex = optionSelections.getOrElse("animation-mode", 2),
      category = CommandCategory.Settings,
      hint = Some("None, subtle, or full")
    )

  private[command] def uiPresetSelectOptionItem(
    previews: List[UiPreset.Preview],
    optionSelections: Map[String, Int] = Map.empty
  ): CommandSurfaceItem.OptionItem =
    val builtInOptions = UiPreset.builtIns.map { preset =>
      val preview = UiPreset.Preview.fromPreset(preset)
      CommandOption(preview.name, CommandIntent.ApplyUiPreset(preview.name), hint = Some(preview.hint))
    }
    val customOptions = normalizedUiPresetPreviews(previews).map { preview =>
      CommandOption(preview.name, CommandIntent.ApplyUiPreset(preview.name), hint = Some(preview.hint))
    }
    val options = builtInOptions ++ customOptions
    val selectedIndex =
      optionSelections
        .get("ui-preset-custom")
        .filter(_ => customOptions.nonEmpty)
        .map(index => builtInOptions.size + boundedOptionIndex(index, customOptions))
        .getOrElse(boundedOptionIndex(optionSelections.getOrElse("ui-preset-built-in", 0), builtInOptions))

    CommandSurfaceItem.OptionItem(
      id = "ui-preset-select",
      label = "Select Preset",
      options = options,
      selectedIndex = boundedOptionIndex(selectedIndex, options),
      category = CommandCategory.Settings,
      hint = Some("Built-in and saved presets")
    )

  private[command] val themeItems: List[CommandSurfaceItem] =
    List(
      CommandSurfaceItem.CommandItem(
        Command.typed(
          "theme-chooser",
          "Choose a theme with live preview.",
          CommandIntent.OpenThemeChooser,
          CommandCategory.Settings,
          label = "Theme Chooser"
        )
      ),
      CommandSurfaceItem.CommandItem(
        Command.typed(
          "theme-creator",
          "Create and save a custom theme with live colour previews.",
          CommandIntent.OpenThemeCreator,
          CommandCategory.Settings,
          label = "Theme Creator"
        )
      ),
      CommandSurfaceItem.CommandItem(
        Command.typed(
          "toggle-theme",
          "Switch between the light and dark themes.",
          CommandIntent.ToggleTheme,
          CommandCategory.Settings,
          label = "Toggle Theme"
        )
      ),
      CommandSurfaceItem.CommandItem(
        Command.typed(
          "reload-theme",
          "Reload the current theme configuration.",
          CommandIntent.ReloadTheme,
          CommandCategory.Settings,
          label = "Reload Theme"
        )
      )
    )

  private[command] def workspaceLayoutItems(optionSelections: Map[String, Int]): List[CommandSurfaceItem] =
    val panelDefinitions = List(
      ("Explorer", PanelKind.Explorer, "panel-explorer-pin"),
      ("Outline", PanelKind.Outline, "panel-outline-pin"),
      ("Diagnostics", PanelKind.Diagnostics, "panel-diagnostics-pin"),
      ("Markdown Preview", PanelKind.MarkdownPreview, "panel-markdown-preview-pin")
    )
    val panelPinItems = panelDefinitions.map {
      case (label, kind, optionId) =>
        panelPinOptionItem(optionId, label, kind, optionSelections)
    }
    val pinnedPanels = panelDefinitions.flatMap {
      case (label, kind, optionId) =>
        selectedPanelPosition(optionSelections, optionId).map(PinnedPanelRow(label, kind, _))
    }
    val reorderablePositions = pinnedPanels
      .groupBy(_.position)
      .collect { case (position, panels) if panels.size >= 2 => position }
      .toSet
    val panelOrderItems =
      pinnedPanels.filter(panel => reorderablePositions(panel.position)).flatMap { panel =>
        List(
          CommandSurfaceItem.CommandItem(
            Command.typed(
              s"move-${commandId(panel.label)}-panel-earlier",
              s"Move the ${panel.label} panel earlier within its pinned edge.",
              CommandIntent.MovePanelEarlier(panel.kind),
              CommandCategory.Settings,
              label = s"Move ${panel.label} Earlier"
            )
          ),
          CommandSurfaceItem.CommandItem(
            Command.typed(
              s"move-${commandId(panel.label)}-panel-later",
              s"Move the ${panel.label} panel later within its pinned edge.",
              CommandIntent.MovePanelLater(panel.kind),
              CommandCategory.Settings,
              label = s"Move ${panel.label} Later"
            )
          )
        )
      }
    val pinnedPositions = pinnedPanels.map(_.position).toSet
    val edgeActionItems = List(PanelPosition.Left, PanelPosition.Right, PanelPosition.Bottom)
      .filter(pinnedPositions)
      .flatMap(panelActionItems)
    val commandItems = edgeActionItems ++
      Option
        .when(edgeActionItems.nonEmpty)(
          CommandSurfaceItem.CommandItem(
            Command.typed(
              "collapse-expanded-panel",
              "Collapse the expanded panel back to its pinned position.",
              CommandIntent.CollapseExpandedPanel,
              CommandCategory.View,
              label = "Collapse Expanded Panel"
            )
          )
        )
        .toList
    val panelPinsGroup = CommandSurfaceItem.GroupItem(
      id = "settings-panel-pins",
      label = "Panel Pins",
      children = panelPinItems,
      category = CommandCategory.Settings,
      hint = Some("Choose panel edge placement")
    )
    val panelOrderGroup = Option.when(panelOrderItems.nonEmpty)(
      CommandSurfaceItem.GroupItem(
        id = "settings-panel-order",
        label = "Panel Order",
        children = panelOrderItems,
        category = CommandCategory.Settings,
        hint = Some("Reorder panels on the same edge")
      )
    )
    val panelActionsGroup = Option.when(commandItems.nonEmpty)(
      CommandSurfaceItem.GroupItem(
        id = "settings-panel-actions",
        label = "Panel Actions",
        children = commandItems,
        category = CommandCategory.Settings,
        hint = Some("Focus, expand, unpin, collapse")
      )
    )

    panelPinsGroup :: panelOrderGroup.toList ::: panelActionsGroup.toList

  private def commandId(label: String): String =
    label.toLowerCase.replaceAll("[^a-z0-9]+", "-").stripPrefix("-").stripSuffix("-")

  private def panelActionItems(position: PanelPosition): List[CommandSurfaceItem.CommandItem] =
    val label = position match
      case PanelPosition.Left   => "Left"
      case PanelPosition.Right  => "Right"
      case PanelPosition.Bottom => "Bottom"
      case PanelPosition.Top    => "Top"
    val id = label.toLowerCase
    List(
      CommandSurfaceItem.CommandItem(
        Command.typed(
          s"focus-$id-panel",
          s"Focus the $id pinned panel.",
          CommandIntent.FocusPanel(position),
          CommandCategory.View,
          label = s"Focus $label Panel"
        )
      ),
      CommandSurfaceItem.CommandItem(
        Command.typed(
          s"expand-$id-panel",
          s"Expand the $id pinned panel.",
          CommandIntent.ExpandPanel(position),
          CommandCategory.View,
          label = s"Expand $label Panel"
        )
      ),
      CommandSurfaceItem.CommandItem(
        Command.typed(
          s"unpin-$id-panel",
          s"Unpin the $id panel.",
          CommandIntent.UnpinPanel(position),
          CommandCategory.View,
          label = s"Unpin $label Panel"
        )
      )
    )

  private case class PinnedPanelRow(label: String, kind: PanelKind, position: PanelPosition)

  private def selectedPanelPosition(optionSelections: Map[String, Int], optionId: String): Option[PanelPosition] =
    List(None, Some(PanelPosition.Top), Some(PanelPosition.Right), Some(PanelPosition.Bottom), Some(PanelPosition.Left))
      .lift(optionSelections.getOrElse(optionId, 0).max(0).min(4))
      .flatten

  private[command] def panelPinOptionItem(
    id: String,
    label: String,
    kind: PanelKind,
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    val options = List(
      CommandOption("Off", CommandIntent.SetPanelPin(kind, None), hint = Some(s"Hide $label")),
      CommandOption("Top", CommandIntent.SetPanelPin(kind, Some(PanelPosition.Top))),
      CommandOption("Right", CommandIntent.SetPanelPin(kind, Some(PanelPosition.Right))),
      CommandOption("Bottom", CommandIntent.SetPanelPin(kind, Some(PanelPosition.Bottom))),
      CommandOption("Left", CommandIntent.SetPanelPin(kind, Some(PanelPosition.Left)))
    )
    CommandSurfaceItem.OptionItem(
      id = id,
      label = label,
      options = options,
      selectedIndex = boundedOptionIndex(optionSelections.getOrElse(id, 0), options),
      category = CommandCategory.Settings,
      hint = Some("Pin this panel to an edge")
    )

  private[command] def lineNumbersOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    enabledOptionItem(
      id = "line-numbers",
      label = "Line Numbers",
      selectedIndex = optionSelections.getOrElse("line-numbers", 0),
      enabledIntent = CommandIntent.SetLineNumbers(true),
      disabledIntent = CommandIntent.SetLineNumbers(false),
      hint = "Show or hide line numbers"
    )

  private[command] def gutterOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    enabledOptionItem(
      id = "gutter",
      label = "Gutter",
      selectedIndex = optionSelections.getOrElse("gutter", 0),
      enabledIntent = CommandIntent.SetGutter(true),
      disabledIntent = CommandIntent.SetGutter(false),
      hint = "Show or hide the status gutter"
    )

  private[command] def lineWrapOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    enabledOptionItem(
      id = "line-wrap",
      label = "Line Wrap",
      selectedIndex = optionSelections.getOrElse("line-wrap", optionSelections.getOrElse("word-wrap", 0)),
      enabledIntent = CommandIntent.SetWordWrap(true),
      disabledIntent = CommandIntent.SetWordWrap(false),
      hint = "Wrap long logical lines to the editor width"
    )

  private[command] def focusedTextBodyOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    enabledOptionItem(
      id = "focused-text-body",
      label = "Text Body Focus",
      selectedIndex = optionSelections.getOrElse("focused-text-body", 1),
      enabledIntent = CommandIntent.SetFocusedTextBody(true),
      disabledIntent = CommandIntent.SetFocusedTextBody(false),
      hint = "Dim text outside the active body"
    )

  private[command] val navigationItems: List[CommandSurfaceItem.CommandItem] =
    List(
      Command.typed(
        "comment-lens",
        "Show or hide the rendered comment at the cursor.",
        CommandIntent.ToggleCommentLens,
        CommandCategory.View,
        label = "Comment Lens"
      ),
      Command.typed(
        "add-document-comment",
        "Add a document comment at the current cursor or selection.",
        CommandIntent.AddDocumentComment("Comment"),
        CommandCategory.Edit,
        label = "Add Document Comment"
      ),
      Command.typed(
        "delete-document-comment",
        "Delete the document comment at the current cursor.",
        CommandIntent.DeleteDocumentComment,
        CommandCategory.Edit,
        label = "Delete Document Comment"
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
        "next-document-comment",
        "Go to the next document comment.",
        CommandIntent.NextDocumentComment,
        CommandCategory.View,
        label = "Next Document Comment"
      ),
      Command.typed(
        "previous-document-comment",
        "Go to the previous document comment.",
        CommandIntent.PreviousDocumentComment,
        CommandCategory.View,
        label = "Previous Document Comment"
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
      )
    ).map(CommandSurfaceItem.CommandItem(_))

  private def boundedOptionIndex(index: Int, options: List[CommandOption]): Int =
    if options.isEmpty then 0
    else index.max(0).min(options.length - 1)

  private def enabledOptionItem(
    id: String,
    label: String,
    selectedIndex: Int,
    enabledIntent: CommandIntent,
    disabledIntent: CommandIntent,
    hint: String
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = id,
      label = label,
      options = List(
        CommandOption("On", enabledIntent),
        CommandOption("Off", disabledIntent)
      ),
      selectedIndex = selectedIndex,
      category = CommandCategory.Settings,
      hint = Some(hint)
    )

  private[command] def normalizedUiPresetNames(names: List[String]): List[String] =
    names
      .map(_.trim)
      .filter(_.nonEmpty)
      .distinctBy(_.toLowerCase)
      .sortBy(_.toLowerCase)

  private[command] def normalizedUiPresetPreviews(previews: List[UiPreset.Preview]): List[UiPreset.Preview] =
    previews
      .map(preview => preview.copy(name = preview.name.trim, hint = preview.hint.trim))
      .filter(_.name.nonEmpty)
      .distinctBy(_.name.toLowerCase)
      .sortBy(_.name.toLowerCase)

  private[command] def codeFontGroupItem(optionSelections: Map[String, Int]): CommandSurfaceItem.GroupItem =
    fontFamilyGroupItem(
      id = "code-font",
      label = "Code Font",
      selectedIndex = optionSelections.getOrElse("code-font", 0),
      families = FontLoader.availableMonospaceFamilies,
      intent = CommandIntent.SetCodeFontFamily(_),
      hint = "Used in code buffers"
    )

  private[command] def textFontGroupItem(optionSelections: Map[String, Int]): CommandSurfaceItem.GroupItem =
    fontFamilyGroupItem(
      id = "text-font",
      label = "Text Font",
      selectedIndex = optionSelections.getOrElse("text-font", 0),
      families = FontLoader.availableTextFamilies,
      intent = CommandIntent.SetTextFontFamily(_),
      hint = "Used in prose buffers"
    )

  private[command] def uiFontGroupItem(optionSelections: Map[String, Int]): CommandSurfaceItem.GroupItem =
    fontFamilyGroupItem(
      id = "ui-font",
      label = "UI Font",
      selectedIndex = optionSelections.getOrElse("ui-font", 0),
      families = FontLoader.availableUiFamilies,
      intent = CommandIntent.SetUiFontFamily(_),
      hint = "Used in the app interface"
    )

  private[command] def textScaleModeOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    val options = List(
      CommandOption("Auto", CommandIntent.SetTextScaleMode(TextScaleMode.Auto), Some("Use display transform")),
      CommandOption("Manual", CommandIntent.SetTextScaleMode(TextScaleMode.Manual), Some("Use configured multiplier")),
      CommandOption("Off", CommandIntent.SetTextScaleMode(TextScaleMode.Off), Some("Use unscaled point sizes"))
    )
    CommandSurfaceItem.OptionItem(
      id = "text-scale-mode",
      label = "Text Scale Mode",
      options = options,
      selectedIndex = boundedOptionIndex(optionSelections.getOrElse("text-scale-mode", 0), options),
      category = CommandCategory.Settings,
      hint = Some("How font sizes adapt to the display")
    )

  private def fontFamilyGroupItem(
    id: String,
    label: String,
    selectedIndex: Int,
    families: List[String],
    intent: String => CommandIntent,
    hint: String
  ): CommandSurfaceItem.GroupItem =
    val selectedFamily = families.lift(selectedIndex).orElse(families.headOption).getOrElse("")
    val children = families.zipWithIndex.map {
      case (family, index) =>
        CommandSurfaceItem.CommandItem(
          Command.typed(
            s"$id-$index-${family.toLowerCase.replaceAll("[^a-z0-9]+", "-").stripPrefix("-").stripSuffix("-")}",
            hint,
            intent(family),
            CommandCategory.Settings,
            label = family
          )
        )
    }
    CommandSurfaceItem.GroupItem(
      id = id,
      label = label,
      children = children,
      category = CommandCategory.Settings,
      hint = Some(selectedFamily)
    )

  private[command] def codeLigaturesOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "code-ligatures",
      label = "Ligature Shaping",
      options = List(
        CommandOption("On", CommandIntent.SetCodeLigatures(true)),
        CommandOption("Off", CommandIntent.SetCodeLigatures(false))
      ),
      selectedIndex = optionSelections.getOrElse("code-ligatures", 0),
      category = CommandCategory.Settings,
      hint = Some("Enable or disable glyph ligatures")
    )

  private[command] def textLigaturesOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "text-ligatures",
      label = "Ligature Shaping",
      options = List(
        CommandOption("On", CommandIntent.SetTextLigatures(true)),
        CommandOption("Off", CommandIntent.SetTextLigatures(false))
      ),
      selectedIndex = optionSelections.getOrElse("text-ligatures", 0),
      category = CommandCategory.Settings,
      hint = Some("Enable or disable glyph ligatures")
    )

  private[command] def uiLigaturesOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "ui-ligatures",
      label = "Ligature Shaping",
      options = List(
        CommandOption("On", CommandIntent.SetUiLigatures(true)),
        CommandOption("Off", CommandIntent.SetUiLigatures(false))
      ),
      selectedIndex = optionSelections.getOrElse("ui-ligatures", 0),
      category = CommandCategory.Settings,
      hint = Some("Enable or disable glyph ligatures")
    )

  private[command] val languageItems: List[CommandSurfaceItem] =
    val plainText = CommandSurfaceItem.CommandItem(
      Command.typed(
        "lang-plain-text",
        "Use plain text mode for the current buffer.",
        CommandIntent.SetBufferLanguage(None),
        CommandCategory.Settings,
        label = "Plain Text"
      )
    )
    val langItems = LanguageId.values.toList.sortBy(_.displayName).map { lang =>
      CommandSurfaceItem.CommandItem(
        Command.typed(
          s"lang-${lang.id}",
          s"Use ${lang.displayName} mode for the current buffer.",
          CommandIntent.SetBufferLanguage(Some(lang)),
          CommandCategory.Settings,
          label = lang.displayName
        )
      )
    }
    plainText :: langItems
