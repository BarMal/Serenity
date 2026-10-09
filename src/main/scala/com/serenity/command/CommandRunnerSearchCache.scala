package com.serenity.command

/** The settings results for one query against one index (#1854). Computed at most once, however many runner copies
  * carry it -- a selection move or a re-render reads the same list rather than searching again.
  */
final class CommandRunnerQueryResults(val index: CommandRunnerSettingsIndex, val query: String):
  lazy val settingsResults: List[CommandSurfaceItem] = index.matchingSettingsResults(query)

/** The search state a `CommandRunner` carries from copy to copy: the settings index, and the results for its query.
  *
  * Every cache compares equal to every other. What it holds is derived entirely from the runner's own fields, so a warm
  * cache and a cold one must not make two otherwise-identical runners unequal.
  */
final class CommandRunnerSearchCache private (carried: Option[CommandRunnerQueryResults]):

  /** The carried results when they still match `inputs` and `query`; otherwise new ones, reusing the carried index when
    * only the query moved.
    */
  def resultsFor(inputs: CommandRunnerSettingsIndex.Inputs, query: String): CommandRunnerQueryResults =
    carried match
      case Some(results) if results.index.inputs == inputs && results.query == query => results
      case Some(results) if results.index.inputs == inputs => CommandRunnerQueryResults(results.index, query)
      case _ => CommandRunnerQueryResults(CommandRunnerSettingsIndex(inputs), query)

  override def equals(other: Any): Boolean =
    other match
      case _: CommandRunnerSearchCache => true
      case _                           => false

  override def hashCode: Int = 0

  override def toString: String = "CommandRunnerSearchCache"

object CommandRunnerSearchCache:
  val empty: CommandRunnerSearchCache = new CommandRunnerSearchCache(None)

  def holding(results: CommandRunnerQueryResults): CommandRunnerSearchCache =
    new CommandRunnerSearchCache(Some(results))
