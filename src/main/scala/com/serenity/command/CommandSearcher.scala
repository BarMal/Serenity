package com.serenity.command

/** Search result for a command with relevance scoring */
final case class CommandSearchResult(
    command: Command,
    relevance: Double
):
  def name: String        = command.name
  def description: String = command.description

/** Functional command searcher that filters and ranks commands.
  *
  * issue #1048: relevance used to be token prefix/contains matching only (every query token had to exactly equal,
  * prefix, or substring-match some whole token of name/label/description) with no fuzzy subsequence scoring -- so a
  * query landing mid-word, or as a scattered subsequence, either failed to match at all or scored identically to a far
  * weaker one. Ranking is now `CommandRunnerSearch.fuzzyScore` end to end (the same scorer settings search's
  * strong-match promotion uses -- see `CommandRunnerSearch.isStrongCommandMatch`), weighted per field the same way the
  * old token ladder was (name highest, then label, then description).
  */
class CommandSearcher(commands: List[Command]):

  /** Search commands by name and description, returning top results, ranked by fuzzy relevance. */
  def search(term: String, maxResults: Int = 5): List[Command] =
    if term.trim.isEmpty then commands.take(maxResults)
    else
      commands.zipWithIndex
        .flatMap {
          case (command, index) =>
            CommandSearcher.relevance(command, term).map(relevance => (command, relevance, index))
        }
        .sortBy { case (_, relevance, index) => (-relevance, index) }
        .take(maxResults)
        .map(_._1)

object CommandSearcher:

  /** The best fuzzy score across name/label/description, each weighted the way the old token ladder weighted them (name
    * highest, then label, then description) -- `None` (no result at all) only when the term fuzzy-matches none of the
    * three.
    */
  private[command] def relevance(command: Command, term: String): Option[Double] =
    List(
      CommandRunnerSearch.fuzzyScore(term, command.name).map(_ * 1.0),
      CommandRunnerSearch.fuzzyScore(term, command.label).map(_ * 0.95),
      CommandRunnerSearch.fuzzyScore(term, command.description).map(_ * 0.4)
    ).flatten.maxOption
