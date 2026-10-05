package com.serenity.state.models

import com.serenity.command.{
  CloseCommands,
  Command,
  ExternalChangeCommands,
  ReopenWithEncodingCommands,
  RichTextCommands,
  RichTextIntent,
  SafeModeCommands
}
import com.serenity.text.TextEncoding
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
  * input until answered; a non-blocking one floats by the cursor. `onDismiss` is what Escape does.
  */
final case class ConfirmPrompt(
    title: String,
    message: List[String],
    choices: SelectableList[ConfirmChoice],
    blocking: Boolean,
    onDismiss: ConfirmAction = ConfirmAction.Dismiss
):
  def selectedChoice: Option[ConfirmChoice] = choices.selectedItem

object ConfirmPrompt:

  def of(title: String, message: List[String], choices: Seq[ConfirmChoice], blocking: Boolean): ConfirmPrompt =
    ConfirmPrompt(title, message, SelectableList.of(choices, EndBehaviour.Wrap), blocking)

  /** The last starts did not reach a first frame (#2021). Declining carries on as normal, so Escape does that too. */
  def offerSafeMode(unfinishedStarts: Int): ConfirmPrompt =
    of(
      title = "Start in Safe Mode?",
      message = List(
        s"Serenity did not finish starting the last $unfinishedStarts times.",
        "Safe mode starts with default settings, without your session, language servers or project tasks. " +
          "Your files and settings are left as they are."
      ),
      choices = List(
        ConfirmChoice("Start in Safe Mode", ConfirmAction.Run(SafeModeCommands.restart), ButtonEmphasis.Primary),
        ConfirmChoice("Continue normally", ConfirmAction.Dismiss)
      ),
      blocking = true
    )

  /** "Save changes before closing?" for the buffer a close is waiting on. Escape cancels the close, as Cancel does. */
  def closeUnsaved(bufferLabel: String): ConfirmPrompt =
    val cancel = ConfirmAction.Run(CloseCommands.resolve(CloseWorkflowChoice.Cancel))
    of(
      title = "unsaved changes",
      message = List(bufferLabel),
      choices = List(
        ConfirmChoice(
          "Save",
          ConfirmAction.Run(CloseCommands.resolve(CloseWorkflowChoice.Save)),
          ButtonEmphasis.Primary
        ),
        ConfirmChoice(
          "Close Anyway",
          ConfirmAction.Run(CloseCommands.resolve(CloseWorkflowChoice.Discard)),
          ButtonEmphasis.Danger
        ),
        ConfirmChoice("Cancel", cancel)
      ),
      blocking = true
    ).copy(onDismiss = cancel)

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

  /** A session brought back unsaved text that differs from its file (#1904). Keeping it is the safe answer, so it is
    * both the first choice and what Escape does.
    */
  def recoverUnsaved(bufferId: BufferId, bufferLabel: String, fileChangedSince: Boolean): ConfirmPrompt =
    of(
      title = "Recover unsaved changes",
      message = List(
        bufferLabel,
        if fileChangedSince then "The file has also changed on disk since these changes were made."
        else "Unsaved changes from your last session are newer than the file on disk."
      ),
      choices = List(
        ConfirmChoice("Keep recovered changes", ConfirmAction.Dismiss, ButtonEmphasis.Primary),
        ConfirmChoice(
          "Open the file from disk",
          ConfirmAction.Run(ExternalChangeCommands.reloadFromDisk(bufferId)),
          ButtonEmphasis.Danger
        )
      ),
      blocking = true
    )

  /** Reopening in another encoding reads the file again, which loses unsaved edits (#1627). */
  def reopenDiscardingEdits(bufferId: BufferId, bufferLabel: String, encoding: TextEncoding): ConfirmPrompt =
    of(
      title = "Discard unsaved changes?",
      message = List(bufferLabel, s"Reopening as ${encoding.configKey} reads the file again from disk."),
      choices = List(
        ConfirmChoice(
          s"Reopen as ${encoding.configKey}",
          ConfirmAction.Run(ReopenWithEncodingCommands.reopen(bufferId, encoding, discardEdits = true)),
          ButtonEmphasis.Danger
        ),
        ConfirmChoice("Cancel", ConfirmAction.Dismiss)
      ),
      blocking = true
    )

  /** The file's bytes don't fit the encoding picked to reopen it in (#1627). Nothing changed, so it doesn't block. */
  def reopenFailed(bufferLabel: String, encoding: TextEncoding): ConfirmPrompt =
    of(
      title = "Can't reopen",
      message = List(s"$bufferLabel isn't valid ${encoding.configKey}."),
      choices = List(
        ConfirmChoice(
          "Choose another encoding",
          ConfirmAction.Run(ReopenWithEncodingCommands.chooseEncoding),
          ButtonEmphasis.Primary
        ),
        ConfirmChoice("Cancel", ConfirmAction.Dismiss)
      ),
      blocking = false
    )

  /** A formatting command on a file whose format can't store formatting. Not blocking: nothing is lost by ignoring it.
    */
  def convertToRichText(bufferLabel: String, requested: RichTextIntent): ConfirmPrompt =
    of(
      title = "Plain-text file",
      message = List(s"$bufferLabel is plain text and can't store formatting."),
      choices = List(
        ConfirmChoice(
          "Save as rich document…",
          ConfirmAction.Run(RichTextCommands.saveAsRichDocument),
          ButtonEmphasis.Primary
        ),
        ConfirmChoice("Format anyway", ConfirmAction.Run(RichTextCommands.convertToRichText(Some(requested)))),
        ConfirmChoice("Cancel", ConfirmAction.Dismiss)
      ),
      blocking = false
    )

  /** A plain Save of a formatted buffer whose file can't store formatting. Not blocking: ignoring it saves nothing. */
  def formattingWouldBeLost(bufferId: BufferId, bufferLabel: String): ConfirmPrompt =
    of(
      title = "Plain-text file",
      message = List(s"$bufferLabel can't store formatting."),
      choices = List(
        ConfirmChoice(
          "Save as rich document…",
          ConfirmAction.Run(RichTextCommands.saveAsRichDocument),
          ButtonEmphasis.Primary
        ),
        ConfirmChoice("Save without formatting", ConfirmAction.Run(RichTextCommands.saveWithoutFormatting(bufferId))),
        ConfirmChoice("Cancel", ConfirmAction.Dismiss)
      ),
      blocking = false
    )
