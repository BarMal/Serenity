package com.serenity.command

/** Orders palette search results by one combined score (#1861): fuzzy relevance plus a bounded recency boost. */
private[command] object CommandRanking:

  /** Kept below the gap between match tiers -- a label prefix scores 855, a description hit at most 400 -- so recency
    * only reorders near-equal matches and never lifts a weak match over a clearly stronger one.
    */
  val MaxRecencyBoost: Double = 50.0

  /** Stable: commands with equal combined scores keep their order in `commands`. */
  def ranked(commands: List[Command], term: String, usage: Map[CommandId, Int]): List[Command] =
    val latestGeneration = usage.values.maxOption.getOrElse(0)
    def recencyBoost(command: Command): Double =
      usage.get(CommandId(command.name)) match
        case Some(generation) if latestGeneration > 0 => MaxRecencyBoost * generation / latestGeneration
        case _                                        => 0.0
    commands.sortBy(command => -(CommandSearcher.relevance(command, term).getOrElse(0.0) + recencyBoost(command)))
