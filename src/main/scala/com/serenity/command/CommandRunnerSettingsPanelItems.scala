package com.serenity.command

/** The Panels settings group. Placement and order live in the Arrange Panels list, which shows every edge at once, so
  * the group only opens it.
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
