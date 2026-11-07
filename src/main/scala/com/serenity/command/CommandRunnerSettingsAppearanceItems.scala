package com.serenity.command

import com.serenity.config.*

/** Interface-density and window-chrome appearance settings items. Split out of `CommandRunnerSettingsItems` to keep
  * both under the architecture size targets -- see that object's doc.
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

  private[command] def layoutOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    def option(label: String, layout: UiLayout) =
      CommandOption(
        label,
        CommandIntent.Settings(SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetUiLayout(layout)))
      )
    CommandSurfaceItem.OptionItem(
      id = "ui-layout",
      label = "Layout",
      options = List(option("Minimal", UiLayout.Minimal), option("Conventional", UiLayout.Conventional)),
      selectedIndex = optionSelections.getOrElse("ui-layout", 0),
      category = CommandCategory.Settings,
      hint =
        Some("Line numbers, pane headers, status bar, tab strip and scrollbar; settings you changed keep their value")
    )

  private[command] def menuBarOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    def option(label: String, mode: MenuBarMode) =
      CommandOption(
        label,
        CommandIntent.Settings(SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetMenuBarMode(mode)))
      )
    CommandSurfaceItem.OptionItem(
      id = "menu-bar",
      label = "Menu Bar",
      options = List(
        option("Hidden", MenuBarMode.Hidden),
        option("Alt Toggles", MenuBarMode.Alt),
        option("Visible", MenuBarMode.Visible)
      ),
      selectedIndex = optionSelections.getOrElse("menu-bar", 0),
      category = CommandCategory.Settings,
      hint = Some("Applies after restart; every menu command stays in this palette whichever you pick")
    )

  private[command] def dockedCornerStyleOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    def option(label: String, style: DockedCornerStyle) =
      CommandOption(
        label,
        CommandIntent.Settings(SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetDockedCornerStyle(style)))
      )
    CommandSurfaceItem.OptionItem(
      id = "docked-corner-style",
      label = "Docked Corner Style",
      options = List(option("Floating", DockedCornerStyle.Floating), option("Flush", DockedCornerStyle.Flush)),
      selectedIndex = optionSelections.getOrElse("docked-corner-style", 0),
      category = CommandCategory.Settings,
      hint = Some("Floating insets the panels, status bar and tab strip by the element gap, so every corner rounds")
    )

  private[command] def paneHeaderCornersOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    def option(label: String, corners: PaneHeaderCorners) =
      CommandOption(
        label,
        CommandIntent.Settings(SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetPaneHeaderCorners(corners)))
      )
    CommandSurfaceItem.OptionItem(
      id = "pane-header-corners",
      label = "Pane Header Corners",
      options = List(option("Rounded", PaneHeaderCorners.Rounded), option("Square", PaneHeaderCorners.Square)),
      selectedIndex = optionSelections.getOrElse("pane-header-corners", 0),
      category = CommandCategory.Settings,
      hint = Some("The top corners of an editor pane's header")
    )

  private[command] def windowChromeOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "window-chrome",
      label = "Window Chrome",
      options = List(
        CommandOption(
          "Auto (Linux Custom)",
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
