package com.serenity.command

/** Orders palette search results by one combined score (#1861): fuzzy relevance plus bounded recency and context
  * boosts.
  */
private[command] object CommandRanking:

  /** Both boosts together stay below the gap between match tiers -- a label prefix scores 855, a description hit at
    * most 400 -- so they only reorder near-equal matches and never lift a weak match over a clearly stronger one.
    */
  val MaxRecencyBoost: Double = 50.0

  /** For a command the context holds what it acts on ([[Availability.Boosted]], #1884). */
  val ContextBoost: Double = 25.0

  /** Stable: commands with equal combined scores keep their order in `commands`. */
  def ranked(
    commands: List[Command],
    term: String,
    usage: Map[CommandId, Int],
    context: CommandRunnerContext
  ): List[Command] =
    val latestGeneration = usage.values.maxOption.getOrElse(0)
    def recencyBoost(command: Command): Double =
      usage.get(CommandId(command.name)) match
        case Some(generation) if latestGeneration > 0 => MaxRecencyBoost * generation / latestGeneration
        case _                                        => 0.0
    def contextBoost(command: Command): Double =
      if CommandAvailability.of(command, context).isBoosted then ContextBoost else 0.0
    commands.sortBy(command =>
      -(CommandSearcher.relevance(command, term).getOrElse(0.0) + recencyBoost(command) + contextBoost(command))
    )
