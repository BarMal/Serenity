package com.serenity.command

import com.serenity.animation.{AnimationConfig, TransitionKind, WindowSitterAction}
import com.serenity.config.*
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
        CommandOption(
          "Blink",
          CommandIntent.Settings(SettingsIntent.Cursor(CursorIntent.SetCursorMode(CursorMode.Blink)))
        ),
        CommandOption(
          "Breathe",
          CommandIntent.Settings(SettingsIntent.Cursor(CursorIntent.SetCursorMode(CursorMode.Breathe)))
        )
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
        CommandOption(
          "Fade",
          CommandIntent.Settings(
            SettingsIntent.Motion(MotionIntent.SetEditorInsertionTransitionKind(TransitionKind.Fade))
          )
        ),
        CommandOption(
          "Typed",
          CommandIntent.Settings(
            SettingsIntent.Motion(MotionIntent.SetEditorInsertionTransitionKind(TransitionKind.TypedText))
          )
        ),
        CommandOption(
          "Directional",
          CommandIntent.Settings(
            SettingsIntent.Motion(MotionIntent.SetEditorInsertionTransitionKind(TransitionKind.DirectionalSweep))
          )
        ),
        CommandOption(
          "Tandem",
          CommandIntent.Settings(
            SettingsIntent.Motion(MotionIntent.SetEditorInsertionTransitionKind(TransitionKind.LineAndCharacterTandem))
          )
        ),
        CommandOption(
          "Off",
          CommandIntent.Settings(
            SettingsIntent.Motion(MotionIntent.SetEditorInsertionTransitionKind(TransitionKind.Disabled))
          )
        )
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
      setIntent = kind => CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetPanelOpenTransitionKind(kind))),
      hint = "Pinned panel opening reveal style"
    )

  private[command] def panelCloseTransitionOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    panelTransitionOptionItem(
      id = "panel-close-transition",
      label = "Panel Close Reveal",
      selectedIndex = optionSelections.getOrElse("panel-close-transition", 0),
      setIntent = kind => CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetPanelCloseTransitionKind(kind))),
      hint = "Pinned panel closing reveal style"
    )

  private[command] def commandRunnerTransitionOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    panelTransitionOptionItem(
      id = "command-runner-transition",
      label = "Command Runner Reveal",
      selectedIndex = optionSelections.getOrElse("command-runner-transition", 0),
      setIntent =
        kind => CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerTransitionKind(kind))),
      hint = "Palette opening reveal style"
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
        CommandOption(
          "Off",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerAnimation(None)))
        ),
        CommandOption(
          "Subtle",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerAnimation(AnimationConfig.subtle)))
        ),
        CommandOption(
          "Smooth",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerAnimation(AnimationConfig.smooth)))
        ),
        CommandOption(
          "Expressive",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerAnimation(AnimationConfig.quick)))
        )
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
        CommandOption("Off", CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetUiAnimation(None)))),
        CommandOption(
          "Subtle",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetUiAnimation(AnimationConfig.subtle)))
        ),
        CommandOption(
          "Smooth",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetUiAnimation(AnimationConfig.smooth)))
        ),
        CommandOption(
          "Expressive",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetUiAnimation(AnimationConfig.quick)))
        )
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
        CommandOption(
          "30 FPS",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetRenderFpsTarget(RenderFpsTarget.Fps30))
          )
        ),
        CommandOption(
          "60 FPS",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetRenderFpsTarget(RenderFpsTarget.Fps60))
          )
        ),
        CommandOption(
          "90 FPS",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetRenderFpsTarget(RenderFpsTarget.Fps90))
          )
        ),
        CommandOption(
          "120 FPS",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetRenderFpsTarget(RenderFpsTarget.Fps120))
          )
        ),
        CommandOption(
          "Uncapped",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetRenderFpsTarget(RenderFpsTarget.Uncapped))
          )
        )
      ),
      selectedIndex = optionSelections.getOrElse("render-fps", 1),
      category = CommandCategory.Settings,
      hint = Some("Render loop cadence")
    )

  private[command] def renderDamageGranularityOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "render-damage-granularity",
      label = "Repaint Granularity",
      options = List(
        CommandOption(
          "Rows",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetRenderDamageGranularity(RenderDamageGranularity.Rows))
          )
        ),
        CommandOption(
          "Cells",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetRenderDamageGranularity(RenderDamageGranularity.Cells))
          )
        )
      ),
      selectedIndex = optionSelections.getOrElse("render-damage-granularity", 0),
      category = CommandCategory.Settings,
      hint = Some("Cells applies to monospaced code buffers only")
    )

  /** One boolean include/exclude toggle per segment (mirroring `enabledOptionItem`'s On/Off shape), plus Move
    * Earlier/Later commands for whichever segments are currently included -- the same discrete-move pattern
    * `workspaceLayoutItems`' panel order group uses, rather than a drag gesture this keyboard-driven app has no
    * mechanism for.
    *
    * `currentOrder` is the segments' actual current order (`AppConfig.cursorInfoBarSegments`), so the listed move
    * commands reflect which segment is really earlier/later and a segment already at an end doesn't offer a no-op move
    * in that direction (issue #1298). Callers that don't have it yet fall back to `segmentDefinitions`' fixed order
    * with neither direction gated, exactly as before -- that fixed order can't be trusted to match the real one, so
    * gating on it could wrongly hide a move that would in fact do something.
    *
    * Every move command is `keepMenuOpenOnSubmit` (issue #1298): reordering is naturally a repeated action, so
    * submitting one leaves the "Cursor" settings group open at its current position instead of closing the whole
    * command-runner overlay, letting the next move be submitted immediately.
    */
  private[command] def cursorInfoBarSegmentItems(
    optionSelections: Map[String, Int],
    currentOrder: List[CursorInfoBarSegment] = Nil
  ): List[CommandSurfaceItem] =
    val segmentDefinitions = List(
      (CursorInfoBarSegment.Title, "Title", "cursor-info-bar-title"),
      (CursorInfoBarSegment.Position, "Position", "cursor-info-bar-position"),
      (CursorInfoBarSegment.WordCount, "Word Count", "cursor-info-bar-word-count"),
      (CursorInfoBarSegment.CharCount, "Char Count", "cursor-info-bar-char-count"),
      (CursorInfoBarSegment.ReadingTime, "Reading Time", "cursor-info-bar-reading-time")
    )
    // Menu label is prefixed "Info Bar: <segment>" to stay distinct in command-runner search from unrelated
    // settings that happen to share the bare segment name -- "Word Count" already labels the status-bar toggle at
    // `wordCountOptionItem`. Command descriptions/hints use the shorter segment name on its own instead.
    val toggleItems = segmentDefinitions.map {
      case (segment, shortLabel, optionId) =>
        enabledOptionItem(
          id = optionId,
          label = s"Info Bar: $shortLabel",
          selectedIndex = optionSelections.getOrElse(optionId, 1),
          enabledIntent = CommandIntent.Settings(
            SettingsIntent.Cursor(CursorIntent.SetCursorInfoBarSegmentIncluded(segment, included = true))
          ),
          disabledIntent = CommandIntent.Settings(
            SettingsIntent.Cursor(CursorIntent.SetCursorInfoBarSegmentIncluded(segment, included = false))
          ),
          hint = s"Include $shortLabel in the cursor info bar"
        )
    }
    val includedByCurrentOrder = currentOrder.flatMap(segment => segmentDefinitions.find(_._1 == segment))
    val knowsCurrentOrder      = includedByCurrentOrder.nonEmpty
    val includedSegments =
      if knowsCurrentOrder then includedByCurrentOrder
      else segmentDefinitions.filter { case (_, _, optionId) => optionSelections.getOrElse(optionId, 1) == 0 }
    val orderItems =
      if includedSegments.size < 2 then Nil
      else
        includedSegments.zipWithIndex.flatMap {
          case ((segment, shortLabel, _), index) =>
            val offerEarlier = !knowsCurrentOrder || index > 0
            val offerLater   = !knowsCurrentOrder || index < includedSegments.size - 1
            List(
              Option.when(offerEarlier)(
                CommandSurfaceItem.CommandItem(
                  Command.typed(
                    s"move-cursor-info-bar-${segment.configKey}-earlier",
                    s"Move the $shortLabel segment earlier in the cursor info bar.",
                    CommandIntent.Settings(
                      SettingsIntent.Cursor(CursorIntent.MoveCursorInfoBarSegmentEarlier(segment))
                    ),
                    CommandCategory.Settings,
                    label = s"Move Info Bar $shortLabel Earlier",
                    keepMenuOpenOnSubmit = true
                  )
                )
              ),
              Option.when(offerLater)(
                CommandSurfaceItem.CommandItem(
                  Command.typed(
                    s"move-cursor-info-bar-${segment.configKey}-later",
                    s"Move the $shortLabel segment later in the cursor info bar.",
                    CommandIntent.Settings(SettingsIntent.Cursor(CursorIntent.MoveCursorInfoBarSegmentLater(segment))),
                    CommandCategory.Settings,
                    label = s"Move Info Bar $shortLabel Later",
                    keepMenuOpenOnSubmit = true
                  )
                )
              )
            ).flatten
        }
    toggleItems ++ orderItems

  private[command] def cursorInfoBarPlacementOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "cursor-info-bar-placement",
      label = "Cursor Info Placement",
      options = List(
        CommandOption(
          "Floating",
          CommandIntent.Settings(
            SettingsIntent.Cursor(CursorIntent.SetCursorInfoBarPlacement(CursorInfoBarPlacement.Floating))
          )
        ),
        CommandOption(
          "Pinned Bottom",
          CommandIntent.Settings(
            SettingsIntent.Cursor(CursorIntent.SetCursorInfoBarPlacement(CursorInfoBarPlacement.PinnedBottom))
          )
        )
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
        CommandOption(
          "Solid",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetBackgroundStyle(BackgroundStyle.Solid))
          )
        ),
        CommandOption(
          "Transparent",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetBackgroundStyle(BackgroundStyle.Transparent))
          )
        ),
        CommandOption(
          "Frosted",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetBackgroundStyle(BackgroundStyle.Frosted))
          )
        ),
        CommandOption(
          "Glass",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetBackgroundStyle(BackgroundStyle.GlassLike))
          )
        )
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
        CommandOption(
          "Compact",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(PanelChromeIntent.SetInterfaceDensity(InterfaceDensity.Compact))
          )
        ),
        CommandOption(
          "Comfortable",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(PanelChromeIntent.SetInterfaceDensity(InterfaceDensity.Comfortable))
          )
        ),
        CommandOption(
          "Spacious",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(PanelChromeIntent.SetInterfaceDensity(InterfaceDensity.Spacious))
          )
        )
      ),
      selectedIndex = optionSelections.getOrElse("interface-density", 1),
      category = CommandCategory.Settings,
      hint = Some("Compact, comfortable, or spacious")
    )

  private[command] def windowChromeOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "window-chrome",
      label = "Window Chrome",
      options = List(
        CommandOption(
          "Auto (Linux Rounded)",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(PanelChromeIntent.SetWindowChromeMode(WindowChromeMode.Auto))
          )
        ),
        CommandOption(
          "Native",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(PanelChromeIntent.SetWindowChromeMode(WindowChromeMode.Native))
          )
        ),
        CommandOption(
          "Native Themed (Windows)",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(PanelChromeIntent.SetWindowChromeMode(WindowChromeMode.NativeThemed))
          )
        ),
        CommandOption(
          "Custom",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(PanelChromeIntent.SetWindowChromeMode(WindowChromeMode.Custom))
          )
        )
      ),
      selectedIndex = optionSelections.getOrElse("window-chrome", 0),
      category = CommandCategory.Settings,
      hint = Some("Applies after restart; auto uses Serenity chrome on Linux")
    )

  private[command] def windowSitterEnabledOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "window-sitter-enabled",
      label = "Window Sitter",
      options = List(
        CommandOption(
          "On",
          CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetWindowSitterEnabled(true)))
        ),
        CommandOption(
          "Off",
          CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetWindowSitterEnabled(false)))
        )
      ),
      selectedIndex = optionSelections.getOrElse("window-sitter-enabled", 0),
      category = CommandCategory.Settings,
      hint = Some("Typing-reactive window decoration")
    )

  private[command] def windowSitterActionOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "window-sitter-action",
      label = "Sitter Action",
      options = List(
        CommandOption(
          "Cycle",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(PanelChromeIntent.SetWindowSitterAction(WindowSitterAction.Cycle))
          )
        ),
        CommandOption(
          "Pulse",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(PanelChromeIntent.SetWindowSitterAction(WindowSitterAction.Pulse))
          )
        ),
        CommandOption(
          "Blink",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(PanelChromeIntent.SetWindowSitterAction(WindowSitterAction.Blink))
          )
        )
      ),
      selectedIndex = optionSelections.getOrElse("window-sitter-action", 1),
      category = CommandCategory.Settings,
      hint = Some("Frame action after typing")
    )

  private[command] def materialPresetOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "material-preset",
      label = "Material Preset",
      options = List(
        CommandOption(
          "Solid",
          CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetMaterialPreset(MaterialPreset.Solid)))
        ),
        CommandOption(
          "Clear",
          CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetMaterialPreset(MaterialPreset.Clear)))
        ),
        CommandOption(
          "Frosted",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetMaterialPreset(MaterialPreset.Frosted))
          )
        ),
        CommandOption(
          "Crystal",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetMaterialPreset(MaterialPreset.Crystal))
          )
        ),
        CommandOption(
          "Custom",
          CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetMaterialPreset(MaterialPreset.Custom)))
        )
      ),
      selectedIndex = optionSelections.getOrElse("material-preset", 2),
      category = CommandCategory.Settings,
      hint = Some("Material baseline for panels and overlays")
    )

  private[command] def postProcessingOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "post-processing",
      label = "Post-processing",
      options = List(
        CommandOption(
          "Off",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetPostProcessingEffect(PostProcessingEffect.Off))
          )
        ),
        CommandOption(
          "Scanlines",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetPostProcessingEffect(PostProcessingEffect.Scanlines))
          )
        ),
        CommandOption(
          "Glow",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetPostProcessingEffect(PostProcessingEffect.Glow))
          )
        ),
        CommandOption(
          "Scanlines + Glow",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetPostProcessingEffect(PostProcessingEffect.ScanlinesAndGlow))
          )
        )
      ),
      selectedIndex = optionSelections.getOrElse("post-processing", 0),
      category = CommandCategory.Settings,
      hint = Some("Frame-wide scanlines, glow, or both")
    )

  private[command] def uiShadowsOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "ui-shadows",
      label = "Menu & Panel Shadows",
      options = List(
        CommandOption(
          "Off",
          CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetUiShadowsEnabled(false)))
        ),
        CommandOption(
          "On",
          CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetUiShadowsEnabled(true)))
        )
      ),
      selectedIndex = optionSelections.getOrElse("ui-shadows", 1),
      category = CommandCategory.Settings,
      hint = Some("Draw soft depth shadows behind menus and panels")
    )

  private[command] def motionPresetOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "motion-preset",
      label = "Motion Preset",
      options = List(
        CommandOption(
          "Reduced",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetMotionPreset(MotionPreset.Reduced)))
        ),
        CommandOption(
          "Subtle",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetMotionPreset(MotionPreset.Subtle)))
        ),
        CommandOption(
          "Smooth",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetMotionPreset(MotionPreset.Smooth)))
        ),
        CommandOption(
          "Expressive",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetMotionPreset(MotionPreset.Expressive)))
        ),
        CommandOption(
          "Custom",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetMotionPreset(MotionPreset.Custom)))
        )
      ),
      selectedIndex = optionSelections.getOrElse("motion-preset", 2),
      category = CommandCategory.Settings,
      hint = Some("Animation baseline for UI motion")
    )

  private[command] def motionAccessibilityOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "motion-accessibility",
      label = "Motion Accessibility",
      options = List(
        CommandOption(
          "Standard",
          CommandIntent.Settings(
            SettingsIntent.Motion(MotionIntent.SetMotionAccessibility(MotionAccessibility.Standard))
          )
        ),
        CommandOption(
          "Reduced",
          CommandIntent.Settings(
            SettingsIntent.Motion(MotionIntent.SetMotionAccessibility(MotionAccessibility.Reduced))
          )
        ),
        CommandOption(
          "Off",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetMotionAccessibility(MotionAccessibility.Off)))
        )
      ),
      selectedIndex = optionSelections.getOrElse("motion-accessibility", 0),
      category = CommandCategory.Settings,
      hint = Some("Always overrides preset and family motion")
    )

  private[command] def markdownViewOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "markdown-view",
      label = "Markdown View",
      options = List(
        CommandOption("Source", CommandIntent.View(ViewIntent.SetMarkdownViewMode(MarkdownViewMode.Source))),
        CommandOption(
          "Split Preview",
          CommandIntent.View(ViewIntent.SetMarkdownViewMode(MarkdownViewMode.SplitPreview))
        ),
        CommandOption("Inline Lens", CommandIntent.View(ViewIntent.SetMarkdownViewMode(MarkdownViewMode.InlineLens)))
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
        CommandOption(
          "Plain Text",
          CommandIntent.View(ViewIntent.SetDefaultDocumentMode(DefaultDocumentMode.PlainText))
        ),
        CommandOption("Markdown", CommandIntent.View(ViewIntent.SetDefaultDocumentMode(DefaultDocumentMode.Markdown))),
        CommandOption("Rich Text", CommandIntent.View(ViewIntent.SetDefaultDocumentMode(DefaultDocumentMode.RichText)))
      ),
      selectedIndex = optionSelections.getOrElse("default-document-mode", 0),
      category = CommandCategory.Settings,
      hint = Some("Mode for newly-created documents")
    )

  private[command] def appModeOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "app-mode",
      label = "App Mode",
      options = List(
        CommandOption("Code", CommandIntent.View(ViewIntent.SetAppMode(AppMode.Code))),
        CommandOption("Prose", CommandIntent.View(ViewIntent.SetAppMode(AppMode.Prose)))
      ),
      selectedIndex = optionSelections.getOrElse("app-mode", 0),
      category = CommandCategory.Settings,
      hint = Some("Code or prose workspace -- filters which settings are shown below")
    )

  private[command] def showAllSettingsOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "settings-show-all",
      label = "Show All Settings",
      options = List(
        CommandOption("Off", CommandIntent.View(ViewIntent.SetShowAllSettingsRegardlessOfMode(false))),
        CommandOption("On", CommandIntent.View(ViewIntent.SetShowAllSettingsRegardlessOfMode(true)))
      ),
      selectedIndex = optionSelections.getOrElse("settings-show-all", 0),
      category = CommandCategory.Settings,
      hint = Some("Show settings hidden by the app mode filter above")
    )

  private[command] def spellCheckOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "spellcheck-enabled",
      label = "Spell Check",
      options = List(
        CommandOption(
          "Off",
          CommandIntent.Settings(SettingsIntent.SpellCheck(SpellCheckIntent.SetSpellCheckEnabled(false)))
        ),
        CommandOption(
          "On",
          CommandIntent.Settings(SettingsIntent.SpellCheck(SpellCheckIntent.SetSpellCheckEnabled(true)))
        )
      ),
      selectedIndex = optionSelections.getOrElse("spellcheck-enabled", 0),
      category = CommandCategory.Settings,
      hint = Some("Check prose buffers")
    )

  private[command] def uiPresetSelectOptionItem(
    previews: List[UiPreset.Preview],
    optionSelections: Map[String, Int] = Map.empty
  ): CommandSurfaceItem.OptionItem =
    val builtInOptions = UiPreset.builtIns.map { preset =>
      val preview = UiPreset.Preview.fromPreset(preset)
      CommandOption(
        preview.name,
        CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset(preview.name)),
        hint = Some(preview.hint)
      )
    }
    val customOptions = normalizedUiPresetPreviews(previews).map { preview =>
      CommandOption(
        preview.name,
        CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset(preview.name)),
        hint = Some(preview.hint)
      )
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

  private[command] def workspaceLayoutItems(optionSelections: Map[String, Int]): List[CommandSurfaceItem] =
    val panelDefinitions = List(
      ("Explorer", PanelKind.Explorer, "panel-explorer-pin"),
      ("Outline", PanelKind.Outline, "panel-outline-pin"),
      ("Comments", PanelKind.Comments, "panel-comments-pin"),
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
              CommandIntent.View(ViewIntent.MovePanelEarlier(panel.kind)),
              CommandCategory.Settings,
              label = s"Move ${panel.label} Earlier"
            )
          ),
          CommandSurfaceItem.CommandItem(
            Command.typed(
              s"move-${commandId(panel.label)}-panel-later",
              s"Move the ${panel.label} panel later within its pinned edge.",
              CommandIntent.View(ViewIntent.MovePanelLater(panel.kind)),
              CommandCategory.Settings,
              label = s"Move ${panel.label} Later"
            )
          )
        )
      }
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
    // issue #1057: this used to also build a "Panel Actions" group here (Focus/Expand/Unpin per pinned edge, plus
    // Collapse Expanded Panel) -- those are one-shot actions with no persisted value, already duplicated verbatim as
    // ordinary CommandRegistry commands (`focus-left-panel` etc.), so they are reachable only via the palette now.
    panelPinsGroup :: panelOrderGroup.toList

  private def commandId(label: String): String =
    label.toLowerCase.replaceAll("[^a-z0-9]+", "-").stripPrefix("-").stripSuffix("-")

  final private case class PinnedPanelRow(label: String, kind: PanelKind, position: PanelPosition)

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
      CommandOption("Off", CommandIntent.View(ViewIntent.SetPanelPin(kind, None)), hint = Some(s"Hide $label")),
      CommandOption("Top", CommandIntent.View(ViewIntent.SetPanelPin(kind, Some(PanelPosition.Top)))),
      CommandOption("Right", CommandIntent.View(ViewIntent.SetPanelPin(kind, Some(PanelPosition.Right)))),
      CommandOption("Bottom", CommandIntent.View(ViewIntent.SetPanelPin(kind, Some(PanelPosition.Bottom)))),
      CommandOption("Left", CommandIntent.View(ViewIntent.SetPanelPin(kind, Some(PanelPosition.Left))))
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
      enabledIntent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetLineNumbers(true))),
      disabledIntent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetLineNumbers(false))),
      hint = "Show or hide line numbers"
    )

  private[command] def wordCountOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    enabledOptionItem(
      id = "show-word-count",
      label = "Word Count",
      selectedIndex = optionSelections.getOrElse("show-word-count", 1),
      enabledIntent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetShowWordCount(true))),
      disabledIntent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetShowWordCount(false))),
      hint = "Show word count, character count, and reading time in the status bar"
    )

  private[command] def gutterOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    enabledOptionItem(
      id = "gutter",
      label = "Gutter",
      selectedIndex = optionSelections.getOrElse("gutter", 0),
      enabledIntent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetGutter(true))),
      disabledIntent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetGutter(false))),
      hint = "Show or hide the status gutter"
    )

  private[command] def lineWrapOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    enabledOptionItem(
      id = "line-wrap",
      label = "Line Wrap",
      selectedIndex = optionSelections.getOrElse("line-wrap", optionSelections.getOrElse("word-wrap", 0)),
      enabledIntent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetWordWrap(true))),
      disabledIntent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetWordWrap(false))),
      hint = "Wrap long logical lines to the editor width"
    )

  private[command] def visualLineNavigationOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    enabledOptionItem(
      id = "visual-line-navigation",
      label = "Visual Line Navigation",
      selectedIndex = optionSelections.getOrElse("visual-line-navigation", 0),
      enabledIntent =
        CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetVisualLineCursorNavigation(true))),
      disabledIntent =
        CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetVisualLineCursorNavigation(false))),
      hint = "Move Up/Down by wrapped visual row instead of logical line"
    )

  private[command] def typewriterScrollingOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    enabledOptionItem(
      id = "typewriter-scrolling",
      label = "Typewriter Scrolling",
      selectedIndex = optionSelections.getOrElse("typewriter-scrolling", 1),
      enabledIntent =
        CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetTypewriterScrolling(true))),
      disabledIntent =
        CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetTypewriterScrolling(false))),
      hint = "Keep the cursor's line vertically centred as you type, padding past the document's end"
    )

  private[command] def focusedTextBodyOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    enabledOptionItem(
      id = "focused-text-body",
      label = "Text Body Focus",
      selectedIndex = optionSelections.getOrElse("focused-text-body", 1),
      enabledIntent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetFocusedTextBody(true))),
      disabledIntent = CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetFocusedTextBody(false))),
      hint = "Dim text outside the active body"
    )

  private[command] def contextualToolbarOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    enabledOptionItem(
      id = "contextual-toolbar",
      label = "Contextual Toolbar",
      selectedIndex = optionSelections.getOrElse("contextual-toolbar", 0),
      enabledIntent =
        CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetContextualToolbarEnabled(true))),
      disabledIntent =
        CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetContextualToolbarEnabled(false))),
      hint = "Show the floating rich-text toolbar near the cursor"
    )

  private[command] def contextualToolbarDisplayModeOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "contextual-toolbar-display",
      label = "Contextual Toolbar Style",
      options = List(
        CommandOption(
          "Icon Only",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(PanelChromeIntent.SetContextualToolbarDisplayMode(ToolbarDisplayMode.IconOnly))
          )
        ),
        CommandOption(
          "Text Only",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(PanelChromeIntent.SetContextualToolbarDisplayMode(ToolbarDisplayMode.TextOnly))
          )
        ),
        CommandOption(
          "Icon + Text",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(
              PanelChromeIntent.SetContextualToolbarDisplayMode(ToolbarDisplayMode.IconAndText)
            )
          )
        )
      ),
      selectedIndex = optionSelections.getOrElse("contextual-toolbar-display", 2),
      category = CommandCategory.Settings,
      hint = Some("Toolbar labels as icons, text, or both")
    )

  private[command] def commandRunnerKeyHintsOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    enabledOptionItem(
      id = "command-runner-key-hints",
      label = "Command Runner Key Hints",
      selectedIndex = optionSelections.getOrElse("command-runner-key-hints", 0),
      enabledIntent =
        CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetCommandRunnerShowKeyHints(true))),
      disabledIntent =
        CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetCommandRunnerShowKeyHints(false))),
      hint = "Persistent key-binding footer in the palette and settings surface"
    )

  // issue #1057: this list of Comment/Bookmark/Navigation one-shot actions used to feed the "Navigation" settings
  // group (`CommandRunnerSettingsGroups.navigationGroup`). Removed -- each of these was already registered verbatim
  // in `CommandRegistry.defaultCommands`, so nothing is lost; they are reachable only via the palette now.

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

  private[command] def codeFontGroupItem(
    optionSelections: Map[String, Int],
    availableFamilies: List[String] = FontLoader.availableMonospaceFamilies
  ): CommandSurfaceItem.GroupItem =
    fontFamilyGroupItem(
      id = "code-font",
      label = "Code Font",
      selectedIndex = optionSelections.getOrElse("code-font", 0),
      families = availableFamilies,
      intent =
        commandIntentArg => CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetCodeFontFamily(commandIntentArg))),
      hint = "Used in code buffers"
    )

  private[command] def textFontGroupItem(
    optionSelections: Map[String, Int],
    availableFamilies: List[String] = FontLoader.availableTextFamilies
  ): CommandSurfaceItem.GroupItem =
    fontFamilyGroupItem(
      id = "text-font",
      label = "Text Font",
      selectedIndex = optionSelections.getOrElse("text-font", 0),
      families = availableFamilies,
      intent =
        commandIntentArg => CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetTextFontFamily(commandIntentArg))),
      hint = "Used in prose buffers"
    )

  private[command] def uiFontGroupItem(
    optionSelections: Map[String, Int],
    availableFamilies: List[String] = FontLoader.availableUiFamilies
  ): CommandSurfaceItem.GroupItem =
    fontFamilyGroupItem(
      id = "ui-font",
      label = "UI Font",
      selectedIndex = optionSelections.getOrElse("ui-font", 0),
      families = availableFamilies,
      intent =
        commandIntentArg => CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetUiFontFamily(commandIntentArg))),
      hint = "Used in the app interface"
    )

  private[command] def textScaleModeOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    val options = List(
      CommandOption(
        "Auto",
        CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetTextScaleMode(TextScaleMode.Auto))),
        Some("Use display transform")
      ),
      CommandOption(
        "Manual",
        CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetTextScaleMode(TextScaleMode.Manual))),
        Some("Use configured multiplier")
      ),
      CommandOption(
        "Off",
        CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetTextScaleMode(TextScaleMode.Off))),
        Some("Use unscaled point sizes")
      )
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
      label = "Code Ligatures",
      options = List(
        CommandOption("On", CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetCodeLigatures(true)))),
        CommandOption("Off", CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetCodeLigatures(false))))
      ),
      selectedIndex = optionSelections.getOrElse("code-ligatures", 0),
      category = CommandCategory.Settings,
      hint = Some("Enable or disable glyph ligatures")
    )

  private[command] def textLigaturesOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "text-ligatures",
      label = "Prose Ligatures",
      options = List(
        CommandOption("On", CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetTextLigatures(true)))),
        CommandOption("Off", CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetTextLigatures(false))))
      ),
      selectedIndex = optionSelections.getOrElse("text-ligatures", 0),
      category = CommandCategory.Settings,
      hint = Some("Enable or disable glyph ligatures")
    )

  private[command] def uiLigaturesOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "ui-ligatures",
      label = "UI Ligatures",
      options = List(
        CommandOption("On", CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetUiLigatures(true)))),
        CommandOption("Off", CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetUiLigatures(false))))
      ),
      selectedIndex = optionSelections.getOrElse("ui-ligatures", 0),
      category = CommandCategory.Settings,
      hint = Some("Enable or disable glyph ligatures")
    )

  // issue #1057: this built the "Current Buffer Language" settings group's rows. Removed -- buffer-language
  // switchers are one-shot actions with no persisted "current" value shown in their own row, and are now registered
  // directly as `CommandRegistry.languageCommands`, reachable only via the palette.
