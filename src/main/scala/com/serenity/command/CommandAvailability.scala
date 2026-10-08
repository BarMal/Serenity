package com.serenity.command

/** Where one command stands in the current context (#1884). */
enum Availability:

  /** Outside this mode or frontend ([[CommandScope]]): not offered at all, search included. */
  case Hidden

  /** Offered greyed out with `reason`, so it can still be found, but refused if run. */
  case Disabled(reason: String)

  case Enabled

  /** Runnable and ranked ahead of its peers: the context holds what it acts on. */
  case Boosted

  def isOffered: Boolean =
    this match
      case Hidden                          => false
      case Disabled(_) | Enabled | Boosted => true

  def isRunnable: Boolean =
    this match
      case Enabled | Boosted    => true
      case Hidden | Disabled(_) => false

  def isBoosted: Boolean =
    this match
      case Boosted                        => true
      case Hidden | Disabled(_) | Enabled => false

  def disabledReason: Option[String] =
    this match
      case Disabled(reason)           => Some(reason)
      case Hidden | Enabled | Boosted => None

/** What a command does with the text selection. */
enum SelectionUse:
  case Ignores

  /** Acts on the selection when there is one, and on the line or at the caret otherwise. */
  case Prefers

  /** Does nothing without one. */
  case Requires

/** The one per-command predicate the palette's listing, greying out and ranking, and a bound key's dispatch
  * ([[CommandKeyBindings.runnable]]) all read, so no surface can offer or run a command the others would not.
  */
object CommandAvailability:

  val NeedsSelection: String = "Needs a text selection."

  def of(command: Command, context: CommandRunnerContext): Availability =
    if !CommandRelevance.isAvailable(command, context.editingContext) || !hasDialogFor(command, context) then
      Availability.Hidden
    else
      val hasSelection = context.editingContext.map(_.hasSelection)
      CommandPrerequisites.unmetReason(command, context) match
        case Some(reason) => Availability.Disabled(reason)
        case None =>
          (selectionUseOf(command.intent), hasSelection) match
            case (SelectionUse.Requires, Some(false))                       => Availability.Disabled(NeedsSelection)
            case (SelectionUse.Requires | SelectionUse.Prefers, Some(true)) => Availability.Boosted
            case _                                                          => Availability.Enabled

  /** Open... exists only where the platform's dialog takes a file or a folder; Open File... and Open Folder... stay
    * everywhere so a keyboard user can always be explicit.
    */
  private def hasDialogFor(command: Command, context: CommandRunnerContext): Boolean =
    command.intent != CommandIntent.File(FileIntent.OpenFileOrFolder) || context.opensFileOrFolder

  /** Formatting marks stay [[SelectionUse.Prefers]]: without a selection they set the mark for what is typed next. */
  def selectionUseOf(intent: CommandIntent): SelectionUse =
    intent match
      case CommandIntent.Darlings(DarlingIntent.CutToDarlings)          => SelectionUse.Requires
      case CommandIntent.Edit(EditIntent.Copy | EditIntent.Cut)         => SelectionUse.Prefers
      case CommandIntent.RichText(RichTextIntent.ToggleRichTextMark(_)) => SelectionUse.Prefers
      case _                                                            => SelectionUse.Ignores
