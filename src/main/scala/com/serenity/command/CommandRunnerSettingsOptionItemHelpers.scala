package com.serenity.command

/** Small option-item builders shared across the command-runner settings item objects: bounding a selected index to a
  * valid option range, and building the common on/off option-item shape. Extracted from `CommandRunnerSettingsItems`
  * (which grew past its 600-line target) rather than owned by any one settings category, since more than one sibling
  * depends on these staying in one place instead of reaching back into the object most of them used to be split off of.
  */
private[command] object CommandRunnerSettingsOptionItemHelpers:

  def boundedOptionIndex(index: Int, options: List[CommandOption]): Int =
    if options.isEmpty then 0
    else index.max(0).min(options.length - 1)

  def enabledOptionItem(
    id: String,
    label: String,
    selectedIndex: Int,
    enabledIntent: CommandIntent,
    disabledIntent: CommandIntent,
    hint: String
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = id,
      label = label,
      options = List(
        CommandOption("On", enabledIntent),
        CommandOption("Off", disabledIntent)
      ),
      selectedIndex = selectedIndex,
      category = CommandCategory.Settings,
      hint = Some(hint)
    )
