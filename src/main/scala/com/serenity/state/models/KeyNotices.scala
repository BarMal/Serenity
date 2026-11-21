package com.serenity.state.models

import com.serenity.command.{
  Availability,
  Command,
  CommandAvailability,
  CommandFamily,
  CommandPrerequisites,
  CommandRunnerContext,
  scope
}

/** What a key says when it did nothing, so a press is never met with silence: one short sentence, the action first
  * where there is one. Every notice shares one topic: a second dead key replaces the first rather than stacking beside
  * it.
  */
object KeyNotices:

  private def info(message: String): Notice =
    Notice(NoticeLevel.Info, message, topic = Some(NoticeTopic.KeyDidNothing))

  def unknownCommand(commandId: String): Notice =
    info(s"No command named \"$commandId\".")

  /** Why `command` will not run here: what to do about it when it is disabled, or the mode it works in. */
  def commandUnavailable(command: Command, context: CommandRunnerContext): Notice =
    CommandAvailability.of(command, context) match
      case Availability.Disabled(CommandAvailability.NeedsSelection) =>
        info(s"Select text to use ${command.label}.")
      case Availability.Disabled(CommandPrerequisites.NoProjectDetected) =>
        info(s"Open a project folder to use ${command.label}.")
      case Availability.Disabled(reason) =>
        info(s"${command.label}: $reason")
      case _ =>
        val modes = command.scope.family.modes.map(_.configKey).toList.sorted.mkString(" or ")
        val where =
          if command.scope.family == CommandFamily.Prose && context.editingContext.exists(_.bufferIsCode) then
            "prose documents"
          else s"$modes mode"
        info(s"${command.label} works in $where only.")

  def openDocumentTo(action: String): Notice =
    info(s"Open or create a document to $action.")
