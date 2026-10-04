package com.serenity.command

import com.serenity.config.StatusSegment
import com.serenity.frontend.FrontendCapabilities
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.presets.UiPreset

/** The settings tree and its pre-normalised search haystacks, built from `inputs` and nothing else (#1854).
  *
  * A plain class rather than a case class: its lazy members are the whole point, and they only pay off because a
  * `CommandRunner.copy` carries this instance by reference -- unlike a lazy val on the runner itself, which every copy
  * starts again from scratch.
  */
final class CommandRunnerSettingsIndex(val inputs: CommandRunnerSettingsIndex.Inputs):
  import CommandRunnerSettingsIndex.*

  lazy val settingsGroups: List[CommandSurfaceItem.GroupItem] =
    CommandRunnerSettingsGroups.build(
      optionSelections = inputs.optionSelections,
      inputItems = inputs.inputItems,
      uiPresetPreviews = inputs.uiPresetPreviews,
      editingPresetName = inputs.editingPresetName,
      capabilities = inputs.capabilities,
      fontFamilies = inputs.fontFamilies,
      statusSegments = inputs.statusSegments,
      context = inputs.context
    )

  private lazy val searchableGroups: List[SearchableGroup] =
    distinctGroupsBelow(settingsGroups).map(SearchableGroup.from)

  // A preset-scoped copy of a leaf the global tree already has is never offered on its own.
  private lazy val searchableLeaves: List[SettingLeaf] =
    val leaves          = settingsGroups.flatMap(group => leavesOf(group, Nil, Nil))
    val globalTargetIds = leaves.filterNot(_.isPresetScoped).map(_.item.id).toSet
    leaves.filter(leaf => !leaf.isPresetScoped || !globalTargetIds.contains(leaf.item.id))

  def matchingSettingsResults(term: String): List[CommandSurfaceItem] =
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

  private def exactSettingsGroup(term: String): Option[CommandSurfaceItem.GroupItem] =
    def matches(group: SearchableGroup): Boolean = group.label == term || group.id == term
    // The preset-scoped copy of a group shares its label, so the global one wins whichever is met first in the tree.
    searchableGroups
      .filterNot(_.group.id.startsWith("settings-preset-"))
      .find(matches)
      .orElse(searchableGroups.find(matches))
      .map(_.group)

  private def matchingSettingsGroups(lowerTerm: String): List[CommandSurfaceItem.GroupItem] =
    val matchingGroups = searchableGroups.zipWithIndex
      .flatMap {
        case (searchable, index) =>
          val groupMatch = searchable.searchText.contains(lowerTerm)
          val childMatch = searchable.childSearchTexts.exists(_.contains(lowerTerm))
          Option.when(groupMatch || childMatch)(
            searchable.group -> (settingsSearchRank(searchable.group, lowerTerm, groupMatch), index)
          )
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

  private def matchingSettingLeaves(term: String): List[CommandSurfaceItem.SettingSearchItem] =
    val terms = term.split(" ").filter(_.nonEmpty).toList
    searchableLeaves
      .flatMap(leaf => CommandRunnerSearch.settingSearchRank(leaf.haystack, term, terms).map(leaf.searchItem -> _))
      .sortBy { case (item, rank) => (rank, item.breadcrumb, item.targetItemId) }
      .map(_._1)
      .distinctBy(_.targetItemId)
      .take(CommandRunnerSearch.MaximumSettingSearchResults)

object CommandRunnerSettingsIndex:

  /** Every runner field the settings tree is built from. Two runners with equal inputs share one index. */
  final case class Inputs(
      optionSelections: Map[String, Int],
      inputItems: List[CommandSurfaceItem.InputItem],
      uiPresetPreviews: List[UiPreset.Preview],
      editingPresetName: Option[String],
      capabilities: FrontendCapabilities,
      fontFamilies: FontLoader.FontFamilyCatalog,
      statusSegments: List[StatusSegment],
      context: CommandRunnerContext
  )

  final private case class SearchableGroup(
      group: CommandSurfaceItem.GroupItem,
      label: String,
      id: String,
      searchText: String,
      childSearchTexts: List[String]
  )

  private object SearchableGroup:

    def from(group: CommandSurfaceItem.GroupItem): SearchableGroup =
      SearchableGroup(
        group = group,
        label = CommandRunnerSearch.normalizedSearchTerm(group.label),
        id = CommandRunnerSearch.normalizedSearchTerm(group.id),
        searchText = CommandRunnerSearch.directGroupSearchText(group),
        childSearchTexts = group.children.collect {
          case child if !isGroup(child) => CommandRunnerSearch.directItemSearchText(child)
        }
      )

  final private case class SettingLeaf(
      item: CommandSurfaceItem,
      isPresetScoped: Boolean,
      haystack: CommandRunnerSearch.SettingHaystack,
      searchItem: CommandSurfaceItem.SettingSearchItem
  )

  private def leavesOf(
    group: CommandSurfaceItem.GroupItem,
    ancestorIds: List[String],
    ancestorLabels: List[String]
  ): List[SettingLeaf] =
    group.children.flatMap {
      case child: CommandSurfaceItem.GroupItem =>
        leavesOf(child, ancestorIds :+ group.id, ancestorLabels :+ group.label)
      case child =>
        val breadcrumb     = (("Settings" :: ancestorLabels) :+ group.label).mkString(" > ")
        val isPresetScoped = ancestorIds.contains("settings-ui-presets") || group.id == "settings-ui-presets"
        List(
          SettingLeaf(
            item = child,
            isPresetScoped = isPresetScoped,
            haystack = CommandRunnerSearch.SettingHaystack.of(child, breadcrumb),
            searchItem = CommandSurfaceItem.SettingSearchItem(
              id = s"settings-search:${child.id}",
              targetGroupId = group.id,
              targetItemId = child.id,
              label = CommandRunnerSearch.itemLabel(child),
              breadcrumb = breadcrumb,
              effectiveValue = CommandRunnerSearch.itemEffectiveValue(child),
              sourceScope = if isPresetScoped then "Preset" else "Global",
              category = CommandCategory.Settings,
              hint = CommandRunnerSearch.itemHint(child)
            )
          )
        )
    }

  private def settingsSearchRank(group: CommandSurfaceItem.GroupItem, term: String, groupMatch: Boolean): Int =
    val label = group.label.toLowerCase
    if groupMatch && label == term then 0
    else if groupMatch && label.startsWith(term) then 1
    else if groupMatch then 2
    else 3

  // issue #1060: a family-of-CommandItems picker (e.g. `code-font`, `rich-text-font-family`) is an inline value
  // editor embedded in its parent settings row, not an independent navigable settings section -- it has no children
  // of its own besides the option commands. Search should surface the real settings section that contains it (its
  // parent group), not let it outrank sections it happens to sit ahead of purely by tree position -- adding
  // `rich-text-font-family` as a search-matchable group in its own right, for instance, let it leapfrog "Prose Font"/
  // "Code Font" for a bare "font" query solely because Document & Writing precedes Typography in the settings tree.
  private def isValuePickerGroup(group: CommandSurfaceItem.GroupItem): Boolean =
    group.children.nonEmpty && group.children.forall {
      case _: CommandSurfaceItem.CommandItem => true
      case _                                 => false
    }

  private def isGroup(item: CommandSurfaceItem): Boolean =
    item match
      case _: CommandSurfaceItem.GroupItem => true
      case _                               => false

  private def distinctGroupsBelow(groups: List[CommandSurfaceItem.GroupItem]): List[CommandSurfaceItem.GroupItem] =
    def loop(level: List[CommandSurfaceItem.GroupItem]): List[CommandSurfaceItem.GroupItem] =
      level.filterNot(isValuePickerGroup) ++ level.flatMap { group =>
        loop(group.children.collect { case child: CommandSurfaceItem.GroupItem => child })
      }

    loop(groups).distinctBy(_.id)
