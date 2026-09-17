package com.serenity.command

import com.serenity.animation.sprite.SpriteFrameCycle
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
            SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetInterfaceDensity(InterfaceDensity.Compact))
          )
        ),
        CommandOption(
          "Comfortable",
          CommandIntent.Settings(
            SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetInterfaceDensity(InterfaceDensity.Comfortable))
          )
        ),
        CommandOption(
          "Spacious",
          CommandIntent.Settings(
            SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetInterfaceDensity(InterfaceDensity.Spacious))
          )
        )
      ),
      selectedIndex = optionSelections.getOrElse("interface-density", 1),
      category = CommandCategory.Settings,
      // issue #1046 folded the command runner/palette's standalone "visible rows" setting into this one density
      // control; issue #1549 is that folding it in also made it unfindable by search, since nothing about this
      // item's label or id ever said so. Naming it here (searched via `CommandRunnerSearch.settingSearchRank`) is
      // what lets a search for "command runner", "palette", or "visible items" surface this row.
      hint = Some(
        "Compact, comfortable, or spacious -- also controls how many command runner and palette items are visible at once"
      )
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
            SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetWindowChromeMode(WindowChromeMode.Auto))
          )
        ),
        CommandOption(
          "Native",
          CommandIntent.Settings(
            SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetWindowChromeMode(WindowChromeMode.Native))
          )
        ),
        CommandOption(
          "Native Themed (Windows)",
          CommandIntent.Settings(
            SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetWindowChromeMode(WindowChromeMode.NativeThemed))
          )
        ),
        CommandOption(
          "Custom",
          CommandIntent.Settings(
            SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetWindowChromeMode(WindowChromeMode.Custom))
          )
        )
      ),
      selectedIndex = optionSelections.getOrElse("window-chrome", 0),
      category = CommandCategory.Settings,
      hint = Some("Applies after restart; auto uses Serenity chrome on Linux")
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
          CommandIntent.Settings(SettingsIntent.Decoration(DecorationIntent.SetCompanionSpriteEnabled(true)))
        ),
        CommandOption(
          "Off",
          CommandIntent.Settings(SettingsIntent.Decoration(DecorationIntent.SetCompanionSpriteEnabled(false)))
        )
      ),
      selectedIndex = optionSelections.getOrElse("companion-sprite-enabled", 1),
      category = CommandCategory.Settings,
      hint = Some(
        "A small pixel-art companion pane, idling, occasionally performing a trick, and reacting to typing"
      )
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
            SettingsIntent.Decoration(DecorationIntent.SetVisualFlairLevel(VisualFlairLevel.Full))
          )
        ),
        CommandOption(
          "Reduced",
          CommandIntent.Settings(
            SettingsIntent.Decoration(DecorationIntent.SetVisualFlairLevel(VisualFlairLevel.Reduced))
          )
        ),
        CommandOption(
          "Off",
          CommandIntent.Settings(
            SettingsIntent.Decoration(DecorationIntent.SetVisualFlairLevel(VisualFlairLevel.Off))
          )
        )
      ),
      selectedIndex = optionSelections.getOrElse("visual-flair-level", 0),
      category = CommandCategory.Settings,
      hint = Some("Performance/battery tier for purely decorative extras -- the companion sprite, background blur")
    )

  private[command] def companionSpriteTypingCycleOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "companion-sprite-typing-cycle",
      label = "Typing Cycle",
      options = List(
        CommandOption(
          "Cycle",
          CommandIntent.Settings(
            SettingsIntent.Decoration(DecorationIntent.SetCompanionSpriteTypingCycle(SpriteFrameCycle.Cycle))
          )
        ),
        CommandOption(
          "Pulse",
          CommandIntent.Settings(
            SettingsIntent.Decoration(DecorationIntent.SetCompanionSpriteTypingCycle(SpriteFrameCycle.Pulse))
          )
        ),
        CommandOption(
          "Blink",
          CommandIntent.Settings(
            SettingsIntent.Decoration(DecorationIntent.SetCompanionSpriteTypingCycle(SpriteFrameCycle.Blink))
          )
        )
      ),
      selectedIndex = optionSelections.getOrElse("companion-sprite-typing-cycle", 1),
      category = CommandCategory.Settings,
      hint = Some("Frame cycle style while the companion sprite reacts to typing")
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

  // issue #1044: was ordered Off/On (the one boolean toggle in this file built inline instead of through
  // `CommandRunnerSettingsOptionItemHelpers.enabledOptionItem`) -- normalized to that helper's On/Off convention,
  // the one every other boolean toggle in the settings tree already follows.
  private[command] def uiShadowsOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandRunnerSettingsOptionItemHelpers.enabledOptionItem(
      id = "ui-shadows",
      label = "Menu & Panel Shadows",
      selectedIndex = optionSelections.getOrElse("ui-shadows", 0),
      enabledIntent = CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetUiShadowsEnabled(true))),
      disabledIntent = CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetUiShadowsEnabled(false))),
      hint = "Draw soft depth shadows behind menus and panels"
    )
