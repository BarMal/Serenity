package com.serenity.command

import com.serenity.config.*

/** Cursor style and cursor-info-bar settings items. Split out of `CommandRunnerSettingsItems` to keep both under
  * the architecture size targets -- see that object's doc.
  */
private[command] object CommandRunnerSettingsCursorItems:

  private[command] def cursorModeOptionItem(optionSelections: Map[String, Int]): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "cursor-mode",
      label = "Cursor Style",
      options = List(
        CommandOption(
          "Blink",
          CommandIntent.Settings(SettingsIntent.Cursor(CursorIntent.SetCursorMode(CursorMode.Blink)))
        ),
        CommandOption(
          "Breathe",
          CommandIntent.Settings(SettingsIntent.Cursor(CursorIntent.SetCursorMode(CursorMode.Breathe)))
        )
      ),
      selectedIndex = optionSelections.getOrElse("cursor-mode", 0),
      category = CommandCategory.Settings,
      hint = Some("Blink or breathe")
    )

  private[command] def cursorInfoBarSegmentItems(
    optionSelections: Map[String, Int],
    currentOrder: List[CursorInfoBarSegment] = Nil
  ): List[CommandSurfaceItem] =
    val segmentDefinitions = List(
      (CursorInfoBarSegment.Title, "Title", "cursor-info-bar-title"),
      (CursorInfoBarSegment.Position, "Position", "cursor-info-bar-position"),
      (CursorInfoBarSegment.WordCount, "Word Count", "cursor-info-bar-word-count"),
      (CursorInfoBarSegment.CharCount, "Char Count", "cursor-info-bar-char-count"),
      (CursorInfoBarSegment.ReadingTime, "Reading Time", "cursor-info-bar-reading-time")
    )
    // Menu label is prefixed "Info Bar: <segment>" to stay distinct in command-runner search from unrelated
    // settings that happen to share the bare segment name -- "Word Count" already labels the status-bar toggle at
    // `wordCountOptionItem`. Command descriptions/hints use the shorter segment name on its own instead.
    val toggleItems = segmentDefinitions.map {
      case (segment, shortLabel, optionId) =>
        CommandRunnerSettingsItems.enabledOptionItem(
          id = optionId,
          label = s"Info Bar: $shortLabel",
          selectedIndex = optionSelections.getOrElse(optionId, 1),
          enabledIntent = CommandIntent.Settings(
            SettingsIntent.Cursor(CursorIntent.SetCursorInfoBarSegmentIncluded(segment, included = true))
          ),
          disabledIntent = CommandIntent.Settings(
            SettingsIntent.Cursor(CursorIntent.SetCursorInfoBarSegmentIncluded(segment, included = false))
          ),
          hint = s"Include $shortLabel in the cursor info bar"
        )
    }
    val includedByCurrentOrder = currentOrder.flatMap(segment => segmentDefinitions.find(_._1 == segment))
    val knowsCurrentOrder      = includedByCurrentOrder.nonEmpty
    val includedSegments =
      if knowsCurrentOrder then includedByCurrentOrder
      else segmentDefinitions.filter { case (_, _, optionId) => optionSelections.getOrElse(optionId, 1) == 0 }
    val orderItems =
      if includedSegments.size < 2 then Nil
      else
        includedSegments.zipWithIndex.flatMap {
          case ((segment, shortLabel, _), index) =>
            val offerEarlier = !knowsCurrentOrder || index > 0
            val offerLater   = !knowsCurrentOrder || index < includedSegments.size - 1
            List(
              Option.when(offerEarlier)(
                CommandSurfaceItem.CommandItem(
                  Command.typed(
                    s"move-cursor-info-bar-${segment.configKey}-earlier",
                    s"Move the $shortLabel segment earlier in the cursor info bar.",
                    CommandIntent.Settings(
                      SettingsIntent.Cursor(CursorIntent.MoveCursorInfoBarSegmentEarlier(segment))
                    ),
                    CommandCategory.Settings,
                    label = s"Move Info Bar $shortLabel Earlier",
                    keepMenuOpenOnSubmit = true
                  )
                )
              ),
              Option.when(offerLater)(
                CommandSurfaceItem.CommandItem(
                  Command.typed(
                    s"move-cursor-info-bar-${segment.configKey}-later",
                    s"Move the $shortLabel segment later in the cursor info bar.",
                    CommandIntent.Settings(SettingsIntent.Cursor(CursorIntent.MoveCursorInfoBarSegmentLater(segment))),
                    CommandCategory.Settings,
                    label = s"Move Info Bar $shortLabel Later",
                    keepMenuOpenOnSubmit = true
                  )
                )
              )
            ).flatten
        }
    toggleItems ++ orderItems

  private[command] def cursorInfoBarPlacementOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "cursor-info-bar-placement",
      label = "Cursor Info Placement",
      options = List(
        CommandOption(
          "Floating",
          CommandIntent.Settings(
            SettingsIntent.Cursor(CursorIntent.SetCursorInfoBarPlacement(CursorInfoBarPlacement.Floating))
          )
        ),
        CommandOption(
          "Pinned Bottom",
          CommandIntent.Settings(
            SettingsIntent.Cursor(CursorIntent.SetCursorInfoBarPlacement(CursorInfoBarPlacement.PinnedBottom))
          )
        )
      ),
      selectedIndex = optionSelections.getOrElse("cursor-info-bar-placement", 0),
      category = CommandCategory.Settings,
      hint = Some("Floating or pinned")
    )
