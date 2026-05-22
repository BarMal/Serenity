package com.serenity.command

import cats.effect.IO
import com.serenity.state.models.AppState

/** A command that can be executed in the command runner */
case class Command(
    name: String,
    description: String,
    action: AppState => IO[Unit]
):
  /** Execute this command with the given state */
  def execute(state: AppState): IO[Unit] = action(state)

/** Search result for a command with relevance scoring */
case class CommandSearchResult(
    command: Command,
    relevance: Double
):
  def name: String        = command.name
  def description: String = command.description

/** Functional command searcher that filters and ranks commands */
class CommandSearcher(commands: List[Command]):

  /** Search commands by name and description, returning top results */
  def search(term: String, maxResults: Int = 5): List[Command] =
    if term.isEmpty then commands.take(maxResults)
    else
      val lowercaseTerm = term.toLowerCase

      commands
        .map(cmd => CommandSearchResult(cmd, calculateRelevance(cmd, lowercaseTerm)))
        .filter(_.relevance > 0)
        .sortBy(-_.relevance)
        .take(maxResults)
        .map(_.command)

  /** Calculate relevance score for a command based on search term */
  private def calculateRelevance(command: Command, term: String): Double =
    val nameLower = command.name.toLowerCase
    val descLower = command.description.toLowerCase

    // Exact match in name gets highest score
    if nameLower == term then 100.0
    // Name starts with term gets high score
    else if nameLower.startsWith(term) then 80.0
    // Name contains term gets medium score
    else if nameLower.contains(term) then 60.0
    // Description contains term gets lower score
    else if descLower.contains(term) then 40.0
    // No match
    else 0.0
