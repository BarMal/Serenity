package com.serenity

import com.serenity.command.CommandRunner
import com.serenity.state.models.{AppState, ListPicker, Modal, PickerSource, SurfaceContent}

/** What specs that drive Ctrl+P read back (#2577): Ctrl+P opens the command picker, a [[ListPicker]] over the palette's
  * commands, so the highlighted row is the picker's selected choice.
  */
object CommandPaletteProbe:

  def picker(state: AppState): Option[ListPicker] =
    state.commandPickerSurface.map(_.content).collect {
      case SurfaceContent.ModalWorkflow(Modal.ListPicker(open)) =>
        open
    }

  def isOpen(state: AppState): Boolean = picker(state).isDefined

  /** The command Enter would run: the highlighted choice's, when it is not refused. */
  def selectedCommandName(state: AppState): Option[String] =
    picker(state).flatMap(_.selectedChoice).filter(_.disabledReason.isEmpty).map(_.action.name)

  def queryText(state: AppState): Option[String] = picker(state).map(_.queryText)

  /** The runner the picker searches with: activated when it opened, seeded with the recorded command usage. */
  def runner(state: AppState): Option[CommandRunner] =
    picker(state).flatMap(_.source).collect { case PickerSource.Commands(runner, _) => runner }
