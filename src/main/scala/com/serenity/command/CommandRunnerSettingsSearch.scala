package com.serenity.command

/** `CommandRunner` methods for ranking and matching settings against a search term -- turning a query into the settings
  * groups/leaves it matches, and resolving the current UI-preset editing context. Split out of `CommandRunner` to keep
  * both under the architecture size targets -- see that class's doc.
  */
private[command] trait CommandRunnerSettingsSearch:
  self: CommandRunner =>

  private[command] def matchingSettingsResults(term: String): List[CommandSurfaceItem] =
    val lowerTerm = CommandRunnerSearch.normalizedSearchTerm(term)
    if lowerTerm.length < 3 then Nil
    else
      val leafResults = matchingSettingLeaves(lowerTerm)
      exactSettingsGroup(lowerTerm) match
        case Some(exactGroup) => List(exactGroup)
        case None if leafResults.exists(CommandRunnerSearch.isExactSettingsTarget(_, lowerTerm)) =>
          leafResults.filter(CommandRunnerSearch.isExactSettingsTarget(_, lowerTerm))
        case None if CommandRunnerSearch.isSpecificSettingQuery(lowerTerm) && leafResults.nonEmpty => leafResults
        case None => matchingSettingsGroups(lowerTerm)

  private[command] def exactSettingsGroup(term: String): Option[CommandSurfaceItem.GroupItem] =
    allSettingsGroups.find { group =>
      val label = CommandRunnerSearch.normalizedSearchTerm(group.label)
      val id    = CommandRunnerSearch.normalizedSearchTerm(group.id)
      label == term || id == term
    }

  private[command] def matchingSettingsGroups(lowerTerm: String): List[CommandSurfaceItem.GroupItem] =
    if lowerTerm.length < 3 then Nil
    else
      val matchingGroups = allSettingsGroups.zipWithIndex
        .flatMap {
          case (group, index) =>
            val groupMatch = CommandRunnerSearch.directGroupSearchText(group).contains(lowerTerm)
            val childMatch = group.children.exists {
              case _: CommandSurfaceItem.GroupItem => false
              case child => CommandRunnerSearch.directItemSearchText(child).contains(lowerTerm)
            }
            Option.when(groupMatch || childMatch)(group -> (settingsSearchRank(group, lowerTerm, groupMatch), index))
        }
        .sortBy { case (group, (rank, index)) => (rank, index, group.id) }
      val directGlobalGroups = matchingGroups.collect {
        case (group, _) if !group.id.startsWith("settings-preset-") && group.children.exists {
              case _: CommandSurfaceItem.GroupItem => false
              case _                               => true
            } =>
          group
      }
      if directGlobalGroups.size == 1 then directGlobalGroups
      else directGlobalGroups ++ matchingGroups.map(_._1).filterNot(group => directGlobalGroups.contains(group))

  private[command] def matchingSettingLeaves(term: String): List[CommandSurfaceItem.SettingSearchItem] =
    val leaves           = settingLeaves
    val globalTargetIds  = leaves.filterNot(_.isPresetScoped).map(_.item.id).toSet
    val directSearchable = leaves.filter(leaf => !leaf.isPresetScoped || !globalTargetIds.contains(leaf.item.id))
    directSearchable
      .flatMap {
        case leaf =>
          val (group, item, breadcrumb) = (leaf.group, leaf.item, leaf.breadcrumb)
          CommandRunnerSearch.settingSearchRank(item, breadcrumb, term).map { rank =>
            (
              CommandSurfaceItem.SettingSearchItem(
                id = s"settings-search:${item.id}",
                targetGroupId = group.id,
                targetItemId = item.id,
                label = CommandRunnerSearch.itemLabel(item),
                breadcrumb = breadcrumb,
                effectiveValue = CommandRunnerSearch.itemEffectiveValue(item),
                sourceScope = if leaf.isPresetScoped then "Preset" else "Global",
                category = CommandCategory.Settings,
                hint = CommandRunnerSearch.itemHint(item)
              ),
              rank
            )
          }
      }
      .sortBy { case (item, rank) => (rank, item.breadcrumb, item.targetItemId) }
      .map(_._1)
      .distinctBy(_.targetItemId)
      .take(CommandRunnerSearch.MaximumSettingSearchResults)

  final private[command] case class SettingLeaf(
      group: CommandSurfaceItem.GroupItem,
      item: CommandSurfaceItem,
      breadcrumb: String,
      isPresetScoped: Boolean
  )

  private[command] def settingLeaves: List[SettingLeaf] =
    def loop(
      group: CommandSurfaceItem.GroupItem,
      ancestorIds: List[String],
      ancestorLabels: List[String]
    ): List[SettingLeaf] =
      group.children.flatMap {
        case child: CommandSurfaceItem.GroupItem =>
          loop(child, ancestorIds :+ group.id, ancestorLabels :+ group.label)
        case child =>
          List(
            SettingLeaf(
              group = group,
              item = child,
              breadcrumb = (("Settings" :: ancestorLabels) :+ group.label).mkString(" > "),
              isPresetScoped = ancestorIds.contains("settings-ui-presets") || group.id == "settings-ui-presets"
            )
          )
      }

    settingsGroups.flatMap(group => loop(group, Nil, Nil))

  private[command] def settingsSearchRank(
    group: CommandSurfaceItem.GroupItem,
    term: String,
    groupMatch: Boolean
  ): Int =
    val label = group.label.toLowerCase
    if groupMatch && label == term then 0
    else if groupMatch && label.startsWith(term) then 1
    else if groupMatch then 2
    else 3

  private[command] def submenuSearchTermFor(group: CommandSurfaceItem.GroupItem): String =
    val lowerTerm = CommandRunnerSearch.normalizedSearchTerm(searchTerm)
    if lowerTerm.length < 3 then ""
    else if CommandRunnerSearch.directGroupSearchText(group).contains(lowerTerm) then ""
    else if group.children.exists(child => CommandRunnerSearch.directItemSearchText(child).contains(lowerTerm)) then
      searchTerm
    else ""

  private[command] def allSettingsGroups: List[CommandSurfaceItem.GroupItem] =
    def loop(groups: List[CommandSurfaceItem.GroupItem]): List[CommandSurfaceItem.GroupItem] =
      groups ++ groups.flatMap { group =>
        loop(group.children.collect { case child: CommandSurfaceItem.GroupItem => child })
      }

    loop(settingsGroups).distinctBy(_.id)

  private[command] def presetEditContextName: Option[String] =
    CommandRunnerSettingsGroups.presetEditContextName(
      optionSelections = optionSelections,
      uiPresetPreviews = uiPresetPreviews,
      editingPresetName = editingPresetName
    )
