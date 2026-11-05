package com.serenity.command

/** What the command palette lists for a query (#2577): the one place its matching and ordering live, shared by the
  * palette surface and the command picker that replaces it.
  */
object CommandPaletteResults:

  val MaxMatches: Int = 50

  /** The commands a query matches, ranked. An empty query is every command, in registry order. Filtered to the offered
    * ones and ranked before the cap, so hidden commands can't crowd offered ones out of it, nor the cap drop a command
    * its boosts would have lifted into it.
    */
  def matching(
    registry: CommandRegistry,
    term: String,
    usage: Map[CommandId, Int],
    context: CommandRunnerContext
  ): List[Command] =
    if term.isEmpty then registry.getAllCommands
    else
      val matches = registry
        .searchCommands(term, maxResults = registry.getAllCommands.size)
        .filter(CommandAvailability.of(_, context).isOffered)
      CommandRanking.ranked(matches, term, usage, context).take(MaxMatches)

  /** The rows for `matched` (from [[matching]]) with the settings hits for the same query.
    *
    * An empty query puts Settings first, then the commands that can act on the current editing context: those it boosts
    * first, then recently used ones, every other command keeping its relative order. A query lists exact command
    * matches, exact settings matches, the other strong command matches, the other settings matches, then the weak
    * command matches.
    */
  def items(
    term: String,
    matched: List[Command],
    usage: Map[CommandId, Int],
    context: CommandRunnerContext,
    settingsResults: => List[CommandSurfaceItem]
  ): List[CommandSurfaceItem] =
    val offered = matched
      .map(command => command -> CommandAvailability.of(command, context))
      .filter((_, availability) => availability.isOffered)
    def itemsOf(commands: List[(Command, Availability)]): List[CommandSurfaceItem.CommandItem] =
      commands.map((command, availability) => CommandSurfaceItem.CommandItem(command, availability.disabledReason))
    if term.isEmpty then
      val (settingsEntry, commands) = offered.partition((command, _) => CommandRelevance.isSettingsEntry(command))
      itemsOf(settingsEntry) ++ itemsOf(
        commands
          .filter((command, _) => CommandRelevance.isRelevant(command, context.editingContext))
          .sortBy((command, availability) => (!availability.isBoosted, -usage.getOrElse(CommandId(command.name), 0)))
      )
    else
      val commandItems = itemsOf(offered)
      val (strongCommandMatches, remainingCommandMatches) =
        commandItems.partition(item => CommandRunnerSearch.isStrongCommandMatch(item.command, term))
      val (exactCommandMatches, remainingStrongCommandMatches) =
        strongCommandMatches.partition(item => CommandRunnerSearch.isExactCommandMatch(item.command, term))
      val normalizedTerm = CommandRunnerSearch.normalizedSearchTerm(term)
      val (exactSettingsMatches, remainingSettingsMatches) =
        settingsResults.partition(item => CommandRunnerSearch.isExactSettingsTarget(item, normalizedTerm))
      exactCommandMatches ++ exactSettingsMatches ++ remainingStrongCommandMatches ++ remainingSettingsMatches ++
        remainingCommandMatches
