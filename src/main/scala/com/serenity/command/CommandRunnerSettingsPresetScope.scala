package com.serenity.command

/** Re-aims settings rows at one stored preset (#1682). A row built for the live settings is renamed so it is a
  * different row from its global twin, and each intent it dispatches is wrapped so it edits that preset.
  */
private[command] object CommandRunnerSettingsPresetScope:

  private val ItemIdPrefix = "preset:"

  def isScopedItemId(id: String): Boolean = id.startsWith(ItemIdPrefix)

  private def prefixFor(presetName: String): String = s"$ItemIdPrefix$presetName:"

  /** The runner's in-flight choices for `presetName`'s rows, keyed the way the rows were built for the live settings.
    */
  def selectionsFor(presetName: String, optionSelections: Map[String, Int]): Map[String, Int] =
    val prefix = prefixFor(presetName)
    optionSelections.collect { case (id, index) if id.startsWith(prefix) => id.stripPrefix(prefix) -> index }

  def withoutScopedSelections(optionSelections: Map[String, Int]): Map[String, Int] =
    optionSelections.filterNot((id, _) => isScopedItemId(id))

  /** `group` as a page of `presetName`'s: its own id is `pageId`, everything below it is renamed and re-aimed. */
  def pageOf(presetName: String, pageId: String, group: CommandSurfaceItem.GroupItem): CommandSurfaceItem.GroupItem =
    group.copy(id = pageId, children = group.children.map(scoped(presetName, _)))

  private def scoped(presetName: String, item: CommandSurfaceItem): CommandSurfaceItem =
    val prefix = prefixFor(presetName)
    def aimed(intent: CommandIntent): CommandIntent =
      CommandIntent.Scoped(SettingsTarget.Preset(presetName), intent)
    item match
      case group: CommandSurfaceItem.GroupItem =>
        group.copy(
          id = s"settings-preset-${group.id.stripPrefix("settings-")}-options",
          children = group.children.map(scoped(presetName, _))
        )
      case option: CommandSurfaceItem.OptionItem =>
        option.copy(
          id = prefix + option.id,
          options = option.options.map(choice => choice.copy(intent = aimed(choice.intent)))
        )
      case input: CommandSurfaceItem.InputItem =>
        input.copy(id = prefix + input.id, parse = text => input.parse(text).map(aimed))
      case toggle: CommandSurfaceItem.ToggleItem => toggle.copy(id = prefix + toggle.id)
      case CommandSurfaceItem.CommandItem(command, disabledReason) =>
        CommandSurfaceItem.CommandItem(
          Command.typed(
            prefix + command.name,
            command.description,
            aimed(command.intent),
            command.category,
            command.label,
            command.keepMenuOpenOnSubmit
          ),
          disabledReason
        )
      case other => other
