package com.serenity.state.manager

import com.serenity.state.models.{AppState, Buffer, BufferId}

/** Which buffers auto-save may write, and which commits make one of them due (#1992).
  *
  * A buffer is only ever written to the file it was opened from: an untitled buffer would need a Save As dialog, and a
  * buffer whose rich text would lose its formatting in that file needs the user's say-so, so neither is ever saved
  * unasked.
  */
private[manager] object AutoSave:

  def savable(buffer: Buffer): Boolean =
    !buffer.hidden && buffer.document.isDirty && buffer.document.filePath.isDefined && !buffer.formattingLostOnSave

  /** Whether `bufferId` may be written now: a blocking modal (a conflict or close prompt among them) owns the user's
    * attention, and a save already under way will report its own outcome.
    */
  def savableIn(state: AppState, bufferId: BufferId): Boolean =
    !state.hasBlockingModal && state.persisted.buffers.get(bufferId).exists(savable)

  def savableBuffers(state: AppState): List[BufferId] =
    if state.hasBlockingModal then Nil
    else state.persisted.buffers.valuesIterator.filter(savable).map(_.id).toList

  /** The savable buffers whose text this commit changed. */
  def edited(before: AppState, after: AppState): List[BufferId] =
    val previous = before.persisted.buffers
    if previous eq after.persisted.buffers then Nil
    else
      after.persisted.buffers.valuesIterator
        .filter(buffer =>
          savable(buffer) && previous.get(buffer.id).forall(old => old.document.content ne buffer.document.content)
        )
        .map(_.id)
        .toList

  /** The buffer the user has just stopped working in, if this commit moved them to another one. */
  def leftBehind(before: AppState, after: AppState): Option[BufferId] =
    before.currentEditorBufferId.filter(previous => !after.currentEditorBufferId.contains(previous))
