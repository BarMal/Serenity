package com.serenity.command

import com.serenity.state.models.CloseWorkflowChoice

/** What the "save changes before closing?" prompt's answers run, against the close waiting on the action stack. */
object CloseCommands:

  def resolve(choice: CloseWorkflowChoice): Command =
    val description = choice match
      case CloseWorkflowChoice.Save    => "Save the buffer, then carry on closing."
      case CloseWorkflowChoice.Discard => "Close the buffer without saving it."
      case CloseWorkflowChoice.Cancel  => "Stop closing and keep the buffer open."
    Command.typed(
      s"close-${choice.toString.toLowerCase}",
      description,
      CommandIntent.Lifecycle(LifecycleIntent.ResolveClose(choice)),
      CommandCategory.File
    )
