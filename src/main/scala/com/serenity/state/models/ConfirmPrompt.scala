package com.serenity.state.models

import com.serenity.command.{Command, ExternalChangeCommands}
import com.serenity.ui.widget.{ButtonEmphasis, EndBehaviour, SelectableList}

/** What choosing an option in a [[ConfirmPrompt]] does once the prompt closes. */
enum ConfirmAction:
  /** Runs `command`, through the same path as the palette, so its scope checks and recency apply. */
  case Run(command: Command)
  case Dismiss

final case class ConfirmChoice(
    label: String,
    action: ConfirmAction,
    emphasis: ButtonEmphasis = ButtonEmphasis.Secondary
)

/** A question with a few answers, each carrying what it does as data -- so a new confirmation needs only a value of
  * this, not its own modal case, reducer, composition and routing. A `blocking` prompt is centred and holds every other
  * input until answered; a non-blocking one floats by the cursor.
  */
final case class ConfirmPrompt(
    title: String,
    message: List[String],
    choices: SelectableList[ConfirmChoice],
    blocking: Boolean
):
  def selectedChoice: Option[ConfirmChoice] = choices.selectedItem

object ConfirmPrompt:

  def of(title: String, message: List[String], choices: Seq[ConfirmChoice], blocking: Boolean): ConfirmPrompt =
    ConfirmPrompt(title, message, SelectableList.of(choices, EndBehaviour.Wrap), blocking)

  /** A save that found the file changed on disk since it was read (#1623). A data-loss decision, so it blocks. */
  def reloadConflict(bufferId: BufferId, bufferLabel: String): ConfirmPrompt =
    of(
      title = "File changed on disk",
      message = List(bufferLabel),
      choices = List(
        ConfirmChoice("Reload from disk", ConfirmAction.Run(ExternalChangeCommands.reloadFromDisk(bufferId))),
        ConfirmChoice(
          "Overwrite",
          ConfirmAction.Run(ExternalChangeCommands.overwriteOnDisk(bufferId)),
          ButtonEmphasis.Danger
        ),
        ConfirmChoice("Cancel", ConfirmAction.Dismiss)
      ),
      blocking = true
    )
