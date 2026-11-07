package com.serenity.command

import com.serenity.state.models.{ListChoice, ListPicker, PickerSource}
import com.serenity.ui.layout.KeyHintVocabulary
import com.serenity.ui.widget.TextField

/** The command palette as a [[ListPicker]] (#2577): the same results the palette lists, as choices. */
object CommandPicker:

  val Title: String     = "Commands"
  val NoMatches: String = "No matching commands"

  /** A picker over `runner`'s commands, which must already be activated in the context it opens from. */
  def opened(runner: CommandRunner, registry: CommandRegistry): ListPicker =
    ListPicker(
      Title,
      com.serenity.ui.widget.Loadable.Loading(),
      query = Some(TextField()),
      footer = Some(KeyHintVocabulary.browse("run", "close"))
    )
      .withChoices(choices(runner, registry, ""), NoMatches)
      .copy(source = Some(PickerSource.Commands(runner, registry)))

  def choices(runner: CommandRunner, registry: CommandRegistry, term: String): Vector[ListChoice] =
    val searched = runner.updateSearchTerm(term)(using registry)
    searched.visibleItems.toVector.flatMap(choiceFor(searched, term))

  private def choiceFor(runner: CommandRunner, term: String)(item: CommandSurfaceItem): Option[ListChoice] =
    item match
      case CommandSurfaceItem.CommandItem(command, disabledReason) =>
        Some(
          ListChoice(
            command.label,
            Some(command.description),
            command,
            hint = runner.bindingFor(command),
            disabledReason = disabledReason,
            columns = true
          )
        )
      case setting: CommandSurfaceItem.SettingSearchItem =>
        Some(
          settingChoice(
            setting.label,
            setting.breadcrumb,
            setting.effectiveValue,
            setting.targetGroupId,
            Some(setting.targetItemId),
            term
          )
        )
      case group: CommandSurfaceItem.GroupItem =>
        val path = runner.settingsGroupBreadcrumbLabels(group.id)
        Some(settingChoice(path.mkString(" > "), group.hint.getOrElse("Settings"), None, group.id, None, term))
      case _: CommandSurfaceItem.OptionItem | _: CommandSurfaceItem.ToggleItem | _: CommandSurfaceItem.InputItem =>
        None

  /** Picking a settings hit opens the settings view drilled to it, as the palette's Enter on that hit does. Its command
    * is not in the registry, so picking it is not recorded as a recent command -- also as today.
    */
  private def settingChoice(
    label: String,
    detail: String,
    value: Option[String],
    groupId: String,
    itemId: Option[String],
    term: String
  ): ListChoice =
    val open = Command.typed(
      name = s"open-settings-at:$groupId${itemId.fold("")(item => s"/$item")}",
      description = detail,
      intent =
        CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.OpenSettingsAt(groupId, itemId, term))),
      category = CommandCategory.Settings,
      label = label
    )
    ListChoice(label, Some(detail), open, hint = value, columns = true)
