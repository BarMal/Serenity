package com.serenity.state.undo

import com.serenity.rope.Rope
import com.serenity.state.models.*
import com.serenity.ui.layout.{Layout, WorkspaceNodeId, WorkspaceTree}

final case class BufferSnapshot(
    content: Rope,
    cursors: List[CursorPosition],
    selection: Option[Selection],
    selections: List[Selection],
    preferredColumn: Option[Int],
    preferredXPx: Option[Float],
    viewport: Viewport,
    findState: Option[FindState],
    isNewEmpty: Boolean
):

  def restoreInto(buffer: Buffer): Buffer =
    buffer.copy(
      document = buffer.document.copy(
        content = content,
        isDirty = true,
        isNewEmpty = isNewEmpty
      ),
      editing = buffer.editing.copy(
        cursors = cursors,
        selection = selection,
        selections = selections,
        preferredColumn = preferredColumn,
        preferredXPx = preferredXPx,
        multiCursorVerticalStates = Nil
      ),
      viewport = viewport,
      findState = findState
    )

object BufferSnapshot:

  def fromBuffer(buffer: Buffer): BufferSnapshot =
    BufferSnapshot(
      content = buffer.document.content,
      cursors = buffer.editing.cursors,
      selection = buffer.editing.selection,
      selections = buffer.editing.selections,
      preferredColumn = buffer.editing.preferredColumn,
      preferredXPx = buffer.editing.preferredXPx,
      viewport = buffer.viewport,
      findState = buffer.findState,
      isNewEmpty = buffer.document.isNewEmpty
    )

/** One undoable/redoable change (#1016), self-contained: restoring it needs nothing but the entry and the current
  * `AppState`. Each case owns exactly the state its declaring site (a reducer, or a direct call site like the replace
  * workflow) had in scope when it made the change -- never a whole-`AppState` snapshot, which would retain every open
  * buffer per entry.
  */
sealed trait HistoryEntry:

  /** Restores this entry into `state`, returning the updated state and the entry capturing what was there immediately
    * before restoring -- the inverse, to push onto the opposite stack. `None` if this entry's target no longer exists
    * (e.g. its buffer was since closed) -- the entry is then left in place rather than dropped, matching how a stale
    * redo/undo step already behaved before this type existed.
    */
  def restore(state: AppState): Option[(AppState, HistoryEntry)]

object HistoryEntry:

  /** A buffer's content, as it stood at some point -- the only kind of undo entry before #1016 widened this type. */
  final case class BufferEdit(bufferId: BufferId, paneId: PaneId, snapshot: BufferSnapshot) extends HistoryEntry:

    def restore(state: AppState): Option[(AppState, HistoryEntry)] =
      state.persisted.buffers.get(bufferId).map { current =>
        val inverse        = BufferEdit(bufferId, paneId, BufferSnapshot.fromBuffer(current))
        val restoredBuffer = snapshot.restoreInto(current)
        val snappedState   = snapFocusToPane(state, paneId)
        val restoredState = snappedState.copy(persisted =
          snappedState.persisted.copy(buffers = snappedState.persisted.buffers + (bufferId -> restoredBuffer))
        )
        (restoredState, inverse)
      }

  /** A pane's removal (#1016): captures the whole pre-removal `Layout` rather than just the one pane, since removing a
    * pane can also collapse its parent split and reassign `activeEditorPaneId`/focus -- restoring needs the topology
    * that produces, not just the leaf. Panes reference buffers by id, not content, so this is a topology snapshot, not
    * a buffer snapshot: cheap regardless of how many buffers are open, unlike a full `AppState` snapshot would be.
    */
  final case class PaneClose(layout: Layout, focus: Focus) extends HistoryEntry:

    def restore(state: AppState): Option[(AppState, HistoryEntry)] =
      val inverse = PaneClose(state.persisted.layout, state.persisted.focus)
      Some(state.copy(persisted = state.persisted.copy(layout = layout, focus = focus)) -> inverse)

  /** A pinned-panel layout change (#1016 PR4): pin, unpin, or any other change to which panels are pinned and how
    * they're docked. Captures the whole pre-change `uiSurfaces`/`workspaceTree`/`maximizedWorkspaceNodeId`/`focus`
    * rather than a diff of just the one panel touched, mirroring `PaneClose` -- these fields reference surface content
    * by id/kind, not buffer content, so this stays a topology snapshot regardless of how many panels are pinned.
    */
  final case class PanelChange(
      uiSurfaces: List[UiSurface],
      workspaceTree: Option[WorkspaceTree],
      maximizedWorkspaceNodeId: Option[WorkspaceNodeId],
      focus: Focus
  ) extends HistoryEntry:

    def restore(state: AppState): Option[(AppState, HistoryEntry)] =
      val inverse = PanelChange.capture(state)
      val restoredState = state.copy(
        persisted = state.persisted.copy(
          layout = state.persisted.layout.copy(
            workspaceTree = workspaceTree,
            maximizedWorkspaceNodeId = maximizedWorkspaceNodeId
          ),
          focus = focus
        ),
        runtime = state.runtime.copy(uiSurfaces = uiSurfaces)
      )
      Some(restoredState -> inverse)

  object PanelChange:

    def capture(state: AppState): PanelChange =
      PanelChange(
        state.runtime.uiSurfaces,
        state.persisted.layout.workspaceTree,
        state.persisted.layout.maximizedWorkspaceNodeId,
        state.persisted.focus
      )

  private def snapFocusToPane(state: AppState, paneId: PaneId): AppState =
    if state.persisted.focus == Focus.EditorPane(paneId) then state
    else
      state.copy(persisted =
        state.persisted.copy(
          focus = Focus.EditorPane(paneId),
          layout = state.persisted.layout.copy(activeEditorPaneId = Some(paneId))
        )
      )

/** Full undo/redo state, held separately from AppState in StateManager. Never persisted to disk — always starts fresh.
  */
final case class UndoState(
    undoStack: List[HistoryEntry] = Nil,
    redoStack: List[HistoryEntry] = Nil,
    pendingGroup: Option[HistoryEntry.BufferEdit] = None,
    maxUndoDepth: Int = UndoState.DefaultMaxUndoDepth
):

  def flushPendingGroup: UndoState =
    pendingGroup match
      case None        => this
      case Some(entry) => pushUndo(entry, clearRedo = false).copy(pendingGroup = None)

  def clearRedo: UndoState = copy(redoStack = Nil)

  def pushUndo(entry: HistoryEntry, clearRedo: Boolean = true): UndoState =
    copy(
      undoStack = boundedPush(entry, undoStack),
      redoStack = if clearRedo then Nil else redoStack
    )

  def pushRedo(entry: HistoryEntry): UndoState =
    copy(redoStack = boundedPush(entry, redoStack))

  // Only reallocates the tail of `stack` when it has actually reached the cap -- the common case (well under
  // `maxUndoDepth`, whose default is 1000) is a plain O(1) cons instead of a `take` that copies the whole stack
  // on every push regardless of how far below the cap it is.
  private def boundedPush(entry: HistoryEntry, stack: List[HistoryEntry]): List[HistoryEntry] =
    if stack.lengthIs < effectiveMaxUndoDepth then entry :: stack
    else entry :: stack.take(effectiveMaxUndoDepth - 1)

  private def effectiveMaxUndoDepth: Int =
    math.max(1, maxUndoDepth)

object UndoState:
  val DefaultMaxUndoDepth: Int = 1000
