package com.serenity.command

import com.serenity.animation.{AnimationConfig, TransitionKind}
import com.serenity.config.*

/** Motion, animation, and render-cadence settings items. Split out of `CommandRunnerSettingsItems` to keep both under
  * the architecture size targets -- see that object's doc.
  */
private[command] object CommandRunnerSettingsMotionItems:

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
