package com.serenity.command

import com.serenity.state.models.{PanelId, PanelRegistry}
import com.serenity.ui.layout.PanelPosition

/** Pinned-panel placement and reordering settings items. Split out of `CommandRunnerSettingsItems` to keep both under
  * the architecture size targets -- see that object's doc.
  */
private[command] object CommandRunnerSettingsPanelItems:

  /** `offered` decides which panels are listed -- the settings tree passes whether each one's family fits the app mode.
    */
  private[command] def workspaceLayoutItems(
    optionSelections: Map[String, Int],
    offered: PanelId => Boolean = _ => true
  ): List[CommandSurfaceItem] =
    val panelDefinitions =
      PanelId.values.toList
        .filter(offered)
        .map(id => (PanelRegistry.registrationFor(id).label, id, s"panel-${id.key}-pin"))
    val panelPinItems = panelDefinitions.map {
      case (label, id, optionId) =>
        panelPinOptionItem(optionId, label, id, optionSelections)
    }
    val pinnedPanels = panelDefinitions.flatMap {
      case (label, id, optionId) =>
        selectedPanelPosition(optionSelections, optionId).map(PinnedPanelRow(label, id, _))
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
              CommandIntent.View(ViewIntent.MovePanelEarlier(panel.id)),
              CommandCategory.Settings,
              label = s"Move ${panel.label} Earlier"
            )
          ),
          CommandSurfaceItem.CommandItem(
            Command.typed(
              s"move-${commandId(panel.label)}-panel-later",
              s"Move the ${panel.label} panel later within its pinned edge.",
              CommandIntent.View(ViewIntent.MovePanelLater(panel.id)),
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

  final private case class PinnedPanelRow(label: String, id: PanelId, position: PanelPosition)

  private def selectedPanelPosition(optionSelections: Map[String, Int], optionId: String): Option[PanelPosition] =
    List(None, Some(PanelPosition.Top), Some(PanelPosition.Right), Some(PanelPosition.Bottom), Some(PanelPosition.Left))
      .lift(optionSelections.getOrElse(optionId, 0).max(0).min(4))
      .flatten

  private[command] def panelPinOptionItem(
    id: String,
    label: String,
    panel: PanelId,
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    val options = List(
      CommandOption("Off", CommandIntent.View(ViewIntent.SetPanelPin(panel, None)), hint = Some(s"Hide $label")),
      CommandOption("Top", CommandIntent.View(ViewIntent.SetPanelPin(panel, Some(PanelPosition.Top)))),
      CommandOption("Right", CommandIntent.View(ViewIntent.SetPanelPin(panel, Some(PanelPosition.Right)))),
      CommandOption("Bottom", CommandIntent.View(ViewIntent.SetPanelPin(panel, Some(PanelPosition.Bottom)))),
      CommandOption("Left", CommandIntent.View(ViewIntent.SetPanelPin(panel, Some(PanelPosition.Left))))
    )
    CommandSurfaceItem.OptionItem(
      id = id,
      label = label,
      options = options,
      selectedIndex =
        CommandRunnerSettingsOptionItemHelpers.boundedOptionIndex(optionSelections.getOrElse(id, 0), options),
      category = CommandCategory.Settings,
      hint = Some("Pin this panel to an edge")
    )
