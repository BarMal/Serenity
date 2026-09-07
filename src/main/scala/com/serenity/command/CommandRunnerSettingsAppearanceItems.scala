package com.serenity.command

import com.serenity.animation.WindowSitterAction
import com.serenity.config.*

/** Background, material, window-chrome, and window-sitter appearance settings items. Split out of
  * `CommandRunnerSettingsItems` to keep both under the architecture size targets -- see that object's doc.
  */
private[command] object CommandRunnerSettingsAppearanceItems:

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

  private[command] def companionSpriteEnabledOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "companion-sprite-enabled",
      label = "Companion Sprite",
      options = List(
        CommandOption(
          "On",
          CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetCompanionSpriteEnabled(true)))
        ),
        CommandOption(
          "Off",
          CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetCompanionSpriteEnabled(false)))
        )
      ),
      selectedIndex = optionSelections.getOrElse("companion-sprite-enabled", 1),
      category = CommandCategory.Settings,
      hint = Some("A small pixel-art companion pane, idling and occasionally performing an action")
    )

  private[command] def visualFlairLevelOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "visual-flair-level",
      label = "Visual Flair",
      options = List(
        CommandOption(
          "Full",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(PanelChromeIntent.SetVisualFlairLevel(VisualFlairLevel.Full))
          )
        ),
        CommandOption(
          "Reduced",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(PanelChromeIntent.SetVisualFlairLevel(VisualFlairLevel.Reduced))
          )
        ),
        CommandOption(
          "Off",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(PanelChromeIntent.SetVisualFlairLevel(VisualFlairLevel.Off))
          )
        )
      ),
      selectedIndex = optionSelections.getOrElse("visual-flair-level", 0),
      category = CommandCategory.Settings,
      hint = Some("Performance/battery tier for purely decorative extras -- the companion sprite, background blur")
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
