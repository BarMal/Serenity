package com.serenity.state.components

import cats.effect.IO
import com.serenity.command.Command
import com.serenity.io.FileManager
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.AppState

class FileComponent(fileManager: FileManager)(using balance: Balance) extends FocusedComponent:

  def processEvent(event: Event, currentState: AppState): ComponentResult =
    event match
      case SaveFile =>
        handleSaveFile(currentState)

      case OpenFile =>
        // For now, just log - in future could open file dialog
        println("[FILE] Open file requested")
        ComponentResult.noChange

      case _ => ComponentResult.noChange

  private def handleSaveFile(currentState: AppState): ComponentResult =
    // Get the currently active buffer
    val activeBuffer = getCurrentActiveBuffer(currentState)

    activeBuffer match
      case Some(buffer) if buffer.filePath.isDefined =>
        // Create a command to save the file
        val saveCommand = Command(
          name = s"Save ${buffer.filePath.get}",
          description = s"Save buffer ${buffer.id} to ${buffer.filePath.get}",
          action = _ =>
            fileManager
              .saveBuffer(buffer)
              .flatMap { savedBuffer =>
                // Update the state with the saved buffer
                IO.blocking {
                  println(s"[FILE] Successfully saved ${buffer.filePath.get}")
                }
              }
              .handleError(ex => println(s"[FILE] Error saving file: ${ex.getMessage}"))
        )
        ComponentResult.executeCommand(saveCommand)

      case Some(buffer) =>
        // Buffer has no file path - would need Save As dialog
        println("[FILE] Buffer has no file path - Save As not implemented yet")
        ComponentResult.noChange

      case None =>
        println("[FILE] No active buffer to save")
        ComponentResult.noChange

  private def getCurrentActiveBuffer(state: AppState) =
    state.focus match
      case com.serenity.state.models.Focus.EditorPane(paneId) =>
        for
          pane     <- state.layout.editorPanes.get(paneId)
          bufferId <- pane.bufferId
          buffer   <- state.buffers.get(bufferId)
        yield buffer
      case _ => None
