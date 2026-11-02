package com.serenity.ui.terminal

import java.awt.dnd.{DnDConstants, DropTargetAdapter, DropTargetDragEvent, DropTargetDropEvent}

import com.serenity.input.FileDropHandler

/** Answers AWT's drag events from [[FileDropHandler]]; it decides nothing itself. */
final class FileDropTarget(handler: FileDropHandler) extends DropTargetAdapter:

  override def dragEnter(event: DropTargetDragEvent): Unit =
    respond(event, handler.entered(event.getCurrentDataFlavors))

  override def dragOver(event: DropTargetDragEvent): Unit =
    respond(event, handler.accepts(event.getCurrentDataFlavors))

  override def drop(event: DropTargetDropEvent): Unit =
    if handler.accepts(event.getCurrentDataFlavors) then
      event.acceptDrop(DnDConstants.ACTION_COPY)
      event.dropComplete(handler.handle(event.getTransferable))
    else event.rejectDrop()

  private def respond(event: DropTargetDragEvent, welcome: Boolean): Unit =
    if welcome then event.acceptDrag(DnDConstants.ACTION_COPY) else event.rejectDrag()
