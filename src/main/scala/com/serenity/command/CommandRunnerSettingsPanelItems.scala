package com.serenity.command

import com.serenity.ui.layout.PanelPosition

/** Pinned-panel placement and reordering settings items. Split out of `CommandRunnerSettingsItems` to keep both under
  * the architecture size targets -- see that object's doc.
  */
private[command] object CommandRunnerSettingsPanelItems:

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
      selectedIndex =
        CommandRunnerSettingsOptionItemHelpers.boundedOptionIndex(optionSelections.getOrElse(id, 0), options),
      category = CommandCategory.Settings,
      hint = Some("Pin this panel to an edge")
    )
