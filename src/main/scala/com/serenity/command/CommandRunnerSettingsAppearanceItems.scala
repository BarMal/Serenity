package com.serenity.command

import com.serenity.animation.sprite.SpriteFrameCycle
import com.serenity.config.*

/** Interface-density, window-chrome, and companion-sprite appearance settings items. Split out of
  * `CommandRunnerSettingsItems` to keep both under the architecture size targets -- see that object's doc.
  */
private[command] object CommandRunnerSettingsAppearanceItems:

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
