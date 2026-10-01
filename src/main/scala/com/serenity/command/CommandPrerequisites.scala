package com.serenity.command

import com.serenity.project.ProjectPresence

/** Why a command that belongs in this mode and frontend ([[CommandScope]]) still can't run right now. Unlike an
  * out-of-scope command it stays offered, greyed out with this reason, so it can still be found.
  */
object CommandPrerequisites:

  val NoProjectDetected: String = "No project detected."

  def unmetReason(command: Command, context: CommandRunnerContext): Option[String] =
    command.intent match
      case CommandIntent.Project(intent) => projectReason(intent, context.projectPresence)
      case _                             => None

  private def projectReason(intent: ProjectIntent, presence: ProjectPresence): Option[String] =
    intent match
      case ProjectIntent.RunProjectTask(_) =>
        presence match
          case ProjectPresence.NotDetected                          => Some(NoProjectDetected)
          case ProjectPresence.Detected | ProjectPresence.Unchecked => None
      case ProjectIntent.CancelProjectTask => None
