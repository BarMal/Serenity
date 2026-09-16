package com.serenity.command

import com.serenity.config.{StatusLinePlacement, StatusSegment}

/** Status line settings items: placement, which segments show, and their order. */
private[command] object CommandRunnerSettingsStatusLineItems:

  private[command] val segmentDefinitions: List[(StatusSegment, String, String)] = List(
    (StatusSegment.Position, "Position", "status-position"),
    (StatusSegment.Title, "Title", "status-title"),
    (StatusSegment.Language, "Language", "status-language"),
    (StatusSegment.Mode, "Mode", "status-mode"),
    (StatusSegment.WordCount, "Word Count", "status-word-count"),
    (StatusSegment.CharCount, "Char Count", "status-char-count"),
    (StatusSegment.ReadingTime, "Reading Time", "status-reading-time")
  )

  private[command] def placementOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    def option(label: String, placement: StatusLinePlacement) =
      CommandOption(label, CommandIntent.Settings(SettingsIntent.StatusLine(StatusLineIntent.SetPlacement(placement))))
    CommandSurfaceItem.OptionItem(
      id = "status-placement",
      label = "Status Line",
      options = List(
        option("Pinned", StatusLinePlacement.Pinned),
        option("Floating", StatusLinePlacement.Floating),
        option("Off", StatusLinePlacement.Off)
      ),
      selectedIndex = optionSelections.getOrElse("status-placement", 0),
      category = CommandCategory.Settings,
      hint = Some("A row under the workspace, a quiet row that follows the caret, or off")
    )

  /** A toggle per segment, then a move-earlier/later command per included segment in the line's actual current order
    * (`currentOrder`), so the reorder commands reflect what is on screen.
    */
  private[command] def segmentItems(
    optionSelections: Map[String, Int],
    currentOrder: List[StatusSegment]
  ): List[CommandSurfaceItem] =
    val toggleItems = segmentDefinitions.map {
      case (segment, shortLabel, optionId) =>
        CommandRunnerSettingsOptionItemHelpers.enabledOptionItem(
          id = optionId,
          label = s"Status: $shortLabel",
          selectedIndex = optionSelections.getOrElse(optionId, 1),
          enabledIntent = CommandIntent.Settings(
            SettingsIntent.StatusLine(StatusLineIntent.SetSegmentIncluded(segment, included = true))
          ),
          disabledIntent = CommandIntent.Settings(
            SettingsIntent.StatusLine(StatusLineIntent.SetSegmentIncluded(segment, included = false))
          ),
          hint = s"Show $shortLabel in the status line"
        )
    }
    val included = currentOrder.flatMap(segment => segmentDefinitions.find(_._1 == segment))
    val orderItems =
      if included.size < 2 then Nil
      else
        included.zipWithIndex.flatMap {
          case ((segment, shortLabel, _), index) =>
            List(
              Option.when(index > 0)(
                moveCommand(segment, shortLabel, "earlier", StatusLineIntent.MoveSegmentEarlier(segment))
              ),
              Option.when(index < included.size - 1)(
                moveCommand(segment, shortLabel, "later", StatusLineIntent.MoveSegmentLater(segment))
              )
            ).flatten
        }
    toggleItems ++ orderItems

  private def moveCommand(
    segment: StatusSegment,
    shortLabel: String,
    direction: String,
    intent: StatusLineIntent
  ): CommandSurfaceItem.CommandItem =
    CommandSurfaceItem.CommandItem(
      Command.typed(
        s"move-status-${segment.configKey}-$direction",
        s"Move the $shortLabel segment $direction in the status line.",
        CommandIntent.Settings(SettingsIntent.StatusLine(intent)),
        CommandCategory.Settings,
        label = s"Move Status $shortLabel ${direction.capitalize}",
        keepMenuOpenOnSubmit = true
      )
    )
