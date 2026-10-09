package com.serenity.command

/** `CommandRunner` methods for the settings search state that depends on the runner itself rather than on the settings
  * index -- the search term a group carries when entered, and the current UI-preset editing context. The search proper
  * lives in `CommandRunnerSettingsIndex`. Split out of `CommandRunner` to keep both under the architecture size targets
  * -- see that class's doc.
  */
private[command] trait CommandRunnerSettingsSearch:
  self: CommandRunner =>

  private[command] def submenuSearchTermFor(group: CommandSurfaceItem.GroupItem): String =
    val lowerTerm = CommandRunnerSearch.normalizedSearchTerm(searchTerm)
    if lowerTerm.length < 3 then ""
    else if CommandRunnerSearch.directGroupSearchText(group).contains(lowerTerm) then ""
    else if group.children.exists(child => CommandRunnerSearch.directItemSearchText(child).contains(lowerTerm)) then
      searchTerm
    else ""

  private[command] def presetEditContextName: Option[String] =
    CommandRunnerSettingsPresetGroups.presetEditContextName(
      optionSelections = optionSelections,
      uiPresetPreviews = uiPresetPreviews,
      editingPresetName = editingPresetName
    )
