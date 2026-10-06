package com.serenity.command.menu

import com.serenity.command.{Command, CommandAvailability}
import com.serenity.state.models.AppState

final case class MenuItemState(enabled: Boolean, checked: Option[Boolean], disabledReason: Option[String])

object MenuItemState:

  val NothingToUndo: String = "Nothing to undo."
  val NothingToRedo: String = "Nothing to redo."
  val NoOpenBuffer: String  = "No document is open."

  private val needsActiveBuffer = Set("close", "close-pane", "save")

  /** What the palette says about the command, plus what only a menu knows: whether undo and redo have anything to
    * replay, and whether the commands that act on the open document have one.
    */
  def of(command: Command, app: AppState, canUndo: Boolean, canRedo: Boolean): MenuItemState =
    val availability = CommandAvailability.of(command, app.commandRunnerContext)
    val reason       = availability.disabledReason.orElse(menuOnlyReason(command, app, canUndo, canRedo))
    MenuItemState(
      enabled = availability.isRunnable && reason.isEmpty,
      checked = CommandToggleState.of(command.intent, app),
      disabledReason = reason
    )

  private def menuOnlyReason(command: Command, app: AppState, canUndo: Boolean, canRedo: Boolean): Option[String] =
    command.name match
      case "undo" if !canUndo                                                   => Some(NothingToUndo)
      case "redo" if !canRedo                                                   => Some(NothingToRedo)
      case name if needsActiveBuffer.contains(name) && app.activeBuffer.isEmpty => Some(NoOpenBuffer)
      case _                                                                    => None
