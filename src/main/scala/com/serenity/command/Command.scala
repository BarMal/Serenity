package com.serenity.command

import cats.effect.IO
import com.serenity.state.models.AppState

enum AnimationMode:
  case None
  case Quick
  case Smooth
  case Subtle

enum CommandIntent:
  case SaveCurrentFile
  case SaveCurrentFileAs
  case OpenFile
  case QuitApp
  case NewFile
  case CloseCurrentFile
  case FindInCurrentFile
  case ReplaceInCurrentFile
  case OpenGotoLine
  case ToggleTheme
  case ReloadTheme
  case FormatCurrentFile
  case SetAnimationMode(mode: AnimationMode)
  case ToggleLineNumbers
  case ToggleGutter
  case Custom(run: AppState => IO[Unit])

/** A command that can be executed in the command runner */
case class Command private (
    name: String,
    description: String,
    intent: CommandIntent
):
  /** Execute this command directly when it carries a custom effect. */
  def execute(state: AppState): IO[Unit] =
    intent match
      case CommandIntent.Custom(run) => run(state)
      case _                         => IO.unit

  /** Compatibility accessor while callers move off raw command closures. */
  def action: AppState => IO[Unit] =
    execute

object Command:
  def apply(
    name: String,
    description: String,
    action: AppState => IO[Unit]
  ): Command =
    Command(name, description, CommandIntent.Custom(action))

  def typed(
    name: String,
    description: String,
    intent: CommandIntent
  ): Command =
    Command(name, description, intent)

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

    if nameLower == term then 100.0
    else if nameLower.startsWith(term) then 80.0
    else if nameLower.contains(term) then 60.0
    else if descLower.contains(term) then 40.0
    else 0.0
