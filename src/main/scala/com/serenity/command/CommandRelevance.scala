package com.serenity.command

import com.serenity.state.models.{BufferKind, EditingContext}

/** Which commands the palette offers. A command outside the current mode or frontend ([[CommandScope]]) is not offered
  * at all, search included; of the rest, the opening list (before anything is typed) keeps only those that can act on
  * the active buffer -- rich-text formatting on a rich-text buffer, the Markdown preview on a Markdown buffer -- while
  * search still reaches them.
  */
object CommandRelevance:

  def isAvailable(command: Command, context: Option[EditingContext]): Boolean =
    context.forall(command.scope.admits)

  def isRelevant(command: Command, context: Option[EditingContext]): Boolean =
    isAvailable(command, context) && context.forall { editing =>
      command.bufferRequirement match
        case BufferRequirement.AnyBuffer => true
        case BufferRequirement.RichText  => editing.buffer.contains(BufferKind.RichText)
        case BufferRequirement.Markdown  => editing.buffer.contains(BufferKind.Markdown)
    }

  def isSettingsEntry(command: Command): Boolean =
    command.intent == CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.OpenSettings))
