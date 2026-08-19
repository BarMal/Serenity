package com.serenity.state.reducers

import com.serenity.state.models.{AppState, Buffer, BufferId, EditorPane, PaneId}

/** Reaching the buffer under the cursor forks: the `BufferId` is read out of `layout` and used to index `buffers`, a
  * sibling field. That is why these are hand-written rather than composed optics -- no fixed path expresses it.
  *
  * A missing pane or buffer is absorbed here, so callers compose instead of repeating a `case None` arm.
  */
object Focused:

  def paneOf(state: AppState): Option[EditorPane] =
    state.layout.activeEditorPaneId.flatMap(state.layout.editorPanes.get)

  def bufferOf(state: AppState): Option[Buffer] =
    paneOf(state).flatMap(_.bufferId).flatMap(state.buffers.get)

  def bufferOf(state: AppState, paneId: PaneId): Option[Buffer] =
    state.layout.editorPanes.get(paneId).flatMap(_.bufferId).flatMap(state.buffers.get)

  def replaceBuffer(state: AppState, buffer: Buffer): AppState =
    state.copy(buffers = state.buffers + (buffer.id -> buffer))

  def replacePane(state: AppState, paneId: PaneId, pane: EditorPane): AppState =
    state.copy(layout = state.layout.copy(editorPanes = state.layout.editorPanes + (paneId -> pane)))

  val pane: Transition[Option[EditorPane]] = Transition.inspect(paneOf)

  val buffer: Transition[Option[Buffer]] = Transition.inspect(bufferOf)

  val paneId: Transition[Option[PaneId]] = Transition.inspect(_.layout.activeEditorPaneId)

  val bufferId: Transition[Option[BufferId]] = Transition.inspect(paneOf(_).flatMap(_.bufferId))

  def bufferWithId(id: BufferId): Transition[Option[Buffer]] = Transition.inspect(_.buffers.get(id))

  def paneWithId(id: PaneId): Transition[Option[EditorPane]] = Transition.inspect(_.layout.editorPanes.get(id))

  def modifyBuffer(f: Buffer => Buffer): Transition[Unit] =
    Transition.modify(state => bufferOf(state).fold(state)(buffer => replaceBuffer(state, f(buffer))))

  def modifyBufferWithId(id: BufferId)(f: Buffer => Buffer): Transition[Unit] =
    Transition.modify(state => state.buffers.get(id).fold(state)(buffer => replaceBuffer(state, f(buffer))))

  def modifyPane(f: EditorPane => EditorPane): Transition[Unit] =
    Transition.modify { state =>
      (state.layout.activeEditorPaneId, paneOf(state)) match
        case (Some(id), Some(pane)) => replacePane(state, id, f(pane))
        case _                      => state
    }

  def modifyPaneWithId(id: PaneId)(f: EditorPane => EditorPane): Transition[Unit] =
    Transition.modify(state => state.layout.editorPanes.get(id).fold(state)(pane => replacePane(state, id, f(pane))))
