package com.serenity.command

import com.serenity.state.models.{BufferKind, EditingContext}

/** Which commands the palette offers before anything is typed. Everything stays searchable; this only keeps the opening
  * list to what can act on the current [[EditingContext]]: code tooling in a code workspace, rich-text formatting on a
  * rich-text buffer, the Markdown preview on a Markdown buffer.
  */
object CommandRelevance:

  def isRelevant(command: Command, context: Option[EditingContext]): Boolean =
    context.forall { editing =>
      command.intent match
        case CommandIntent.Lsp(_) | CommandIntent.Project(_)    => editing.hasCodeTooling
        case CommandIntent.Edit(EditIntent.FormatCurrentFile)   => editing.hasCodeTooling
        case CommandIntent.RichText(_)                          => editing.buffer.contains(BufferKind.RichText)
        case CommandIntent.View(ViewIntent.OpenMarkdownPreview) => editing.buffer.contains(BufferKind.Markdown)
        case _                                                  => true
    }

  def isSettingsEntry(command: Command): Boolean =
    command.intent == CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.OpenSettings))
