package com.serenity.command

import com.serenity.config.{AppMode, PanelEscapeTarget}

/** The Panels settings group. Placement and order live in the Arrange Panels list, which shows every edge at once, so
  * the group opens that, and holds where Escape from a focused panel returns focus in each app mode.
  */
private[command] object CommandRunnerSettingsPanelItems:

  private[command] val arrangePanelsCommand: Command =
    Command.typed(
      "arrange-panels",
      "Choose which panels show, on which edge, and in what order.",
      CommandIntent.View(ViewIntent.ArrangePanels),
      CommandCategory.View,
      label = "Arrange Panels…"
    )

  private[command] def workspaceLayoutItems: List[CommandSurfaceItem] =
    List(CommandSurfaceItem.CommandItem(arrangePanelsCommand))

  private[command] def escapeTargetOptionId(mode: AppMode): String = s"panel-escape-${mode.configKey}"

  private[command] def escapeTargetOptionItem(
    optionSelections: Map[String, Int],
    mode: AppMode
  ): CommandSurfaceItem.OptionItem =
    def option(label: String, target: PanelEscapeTarget) =
      CommandOption(
        label,
        CommandIntent.Settings(SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetPanelEscapeTarget(mode, target)))
      )
    val modeName = mode match
      case AppMode.Code  => "Code"
      case AppMode.Prose => "Prose"
    CommandSurfaceItem.OptionItem(
      id = escapeTargetOptionId(mode),
      label = s"Panel Escape ($modeName)",
      options = List(option("Editor", PanelEscapeTarget.Editor), option("Previous Focus", PanelEscapeTarget.Previous)),
      selectedIndex = optionSelections.getOrElse(escapeTargetOptionId(mode), 0),
      category = CommandCategory.Settings,
      hint = Some(s"Where Escape in a focused panel returns focus in $modeName mode")
    )
