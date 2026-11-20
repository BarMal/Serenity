package com.serenity.state.models

import com.serenity.command.{Availability, Command, CommandAvailability, CommandRunnerContext, scope}

/** What a key says when it did nothing, so a press is never met with silence. Every notice shares one topic: a second
  * dead key replaces the first rather than stacking beside it.
  */
object KeyNotices:

  private def info(message: String): Notice =
    Notice(NoticeLevel.Info, message, topic = Some(NoticeTopic.KeyDidNothing))

  def unknownCommand(commandId: String): Notice =
    info(s"This key is bound to \"$commandId\", which is not a command.")

  /** Why `command` will not run here: the reason it is disabled, or the mode it is not part of. */
  def commandUnavailable(command: Command, context: CommandRunnerContext): Notice =
    val reason =
      CommandAvailability.of(command, context) match
        case Availability.Disabled(why) => why
        case _ =>
          context.editingContext.flatMap(command.scope.unavailableReason).getOrElse("It is not available here.")
    info(s"${command.label}: $reason")

  def startPageHasNo(surface: String): Notice =
    info(s"$surface is not available on the start page. Open or create a document first.")
