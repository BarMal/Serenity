package com.serenity.command

import java.util.Locale

/** Search-matching and ranking helpers for the palette and settings search. Split out of `CommandRunner`'s
  * companion object to keep both under the architecture size targets -- see `CommandRunner`'s doc.
  */
private[command] object CommandRunnerSearch:

  private[command] val MaximumSettingSearchResults = 10

  private[command] def normalizedSearchTerm(term: String): String =
    term.trim
      .stripPrefix("\"")
      .stripSuffix("\"")
      .toLowerCase(Locale.ROOT)
      .replaceAll("[^\\p{L}\\p{N}]+", " ")
      .trim

  private[command] def isSpecificSettingQuery(term: String): Boolean =
    term.split(" ").count(_.nonEmpty) > 1

  private[command] def isExactSettingsTarget(item: CommandSurfaceItem, term: String): Boolean =
    item match
      case item: CommandSurfaceItem.SettingSearchItem =>
        val label = normalizedSearchTerm(item.label)
        val id    = normalizedSearchTerm(item.targetItemId)
        label == term || id == term
      case item: CommandSurfaceItem.GroupItem =>
        val label = normalizedSearchTerm(item.label)
        val id    = normalizedSearchTerm(item.id)
        label == term || id == term
      case _ => false

  private[command] def settingSearchRank(item: CommandSurfaceItem, breadcrumb: String, term: String): Option[Int] =
    val label         = normalizedSearchTerm(itemLabel(item))
    val id            = normalizedSearchTerm(item.id)
    val scope         = normalizedSearchTerm(breadcrumb)
    val terms         = term.split(" ").filter(_.nonEmpty).toList
    val allTermsMatch = terms.nonEmpty && terms.forall(token => s"$label $id $scope".contains(token))
    if label == term || id == term then Some(0)
    else if label.startsWith(term) || id.startsWith(term) then Some(1)
    else if allTermsMatch then Some(2)
    else None

  private[command] def itemLabel(item: CommandSurfaceItem): String =
    item match
      case CommandSurfaceItem.CommandItem(command)    => command.label
      case item: CommandSurfaceItem.OptionItem        => item.label
      case item: CommandSurfaceItem.InputItem         => item.label
      case item: CommandSurfaceItem.SettingSearchItem => item.label
      case item: CommandSurfaceItem.GroupItem         => item.label

  private[command] def itemHint(item: CommandSurfaceItem): Option[String] =
    item match
      case item: CommandSurfaceItem.OptionItem        => item.hint
      case item: CommandSurfaceItem.InputItem         => Some(item.hint)
      case item: CommandSurfaceItem.SettingSearchItem => item.hint
      case item: CommandSurfaceItem.GroupItem         => item.hint
      case _: CommandSurfaceItem.CommandItem          => None

  private[command] def itemEffectiveValue(item: CommandSurfaceItem): Option[String] =
    item match
      case item: CommandSurfaceItem.OptionItem => Some(item.selectedOption)
      case item: CommandSurfaceItem.InputItem  => Some(item.currentValue)
      case _                                   => None

  private[command] def isStrongCommandMatch(command: Command, term: String): Boolean =
    val lowerTerm        = term.toLowerCase
    val nameLower        = command.name.toLowerCase
    val labelLower       = command.label.toLowerCase
    val descriptionLower = command.description.toLowerCase
    nameLower.startsWith(lowerTerm) ||
    labelLower.startsWith(lowerTerm) ||
    descriptionLower == lowerTerm ||
    descriptionLower.startsWith(lowerTerm)

  private[command] def isExactCommandMatch(command: Command, term: String): Boolean =
    val normalizedTerm = normalizedSearchTerm(term)
    normalizedSearchTerm(command.name) == normalizedTerm ||
    normalizedSearchTerm(command.label) == normalizedTerm

  private[command] def directGroupSearchText(group: CommandSurfaceItem.GroupItem): String =
    normalizedSearchTerm(s"${group.id} ${group.label} ${group.hint.getOrElse("")}")

  private[command] def directItemSearchText(item: CommandSurfaceItem): String =
    item match
      case group: CommandSurfaceItem.GroupItem =>
        directGroupSearchText(group)
      case other =>
        normalizedSearchTerm(s"${other.id} ${other.searchText}")
