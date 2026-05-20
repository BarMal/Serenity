package com.serenity.state.components

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.state.models.AppState
import com.serenity.io.{FileManager, FileType}
import com.serenity.rope.Balance

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
        // Save to existing file
        try
          val savedBuffer = fileManager.saveBuffer(buffer).unsafeRunSync()
          ComponentResult.updateState { state =>
            state.copy(buffers = state.buffers + (buffer.id -> savedBuffer))
          }
        catch
          case ex: Exception =>
            println(s"[FILE] Error saving file: ${ex.getMessage}")
            ComponentResult.noChange
            
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
          pane <- state.layout.editorPanes.get(paneId)
          bufferId <- pane.bufferId
          buffer <- state.buffers.get(bufferId)
        yield buffer
      case _ => None