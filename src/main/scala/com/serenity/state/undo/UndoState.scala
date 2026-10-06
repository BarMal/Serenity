package com.serenity.state.undo

import com.serenity.io.DocumentRevision
import com.serenity.rope.Rope
import com.serenity.state.models.*
import com.serenity.state.reducers.EditorEditSupport
import com.serenity.text.{LineEnding, LineEndingCounts}
import com.serenity.ui.layout.{Layout, WorkspaceNodeId, WorkspaceTree}

/** A buffer as it stood before an undoable change. `richText` travels with `content` because a rich-text document only
  * means anything against the text it was built for (#1935); `richTextInSync` records whether it did match that text.
  */
final case class BufferSnapshot(
    content: Rope,
    richText: RichTextState,
    richTextInSync: Boolean,
    editing: EditingState,
    viewport: Viewport,
    findState: Option[FindState],
    isNewEmpty: Boolean
):

  def restoreInto(buffer: Buffer): Buffer =
    // Bumped even when `content` is the current text: whatever was stamped against the current version (the rich
    // text, an outline) was stamped against the state being undone, not this one.
    val restoredDocument = buffer.document.withContent(content).copy(isNewEmpty = isNewEmpty)
    buffer.copy(
      document = restoredDocument,
      editing = editing,
      viewport = viewport,
      findState = findState,
      annotations =
        EditorEditSupport.adjustAnnotationsAcrossReplacement(buffer.annotations, buffer.document.content, content),
      // Fidelity describes the file as last read or written, not this edit state: undoing past a save must not bring
      // back a lossy-import warning that save already settled.
      richText = richText.copy(
        richTextFidelity = buffer.richText.richTextFidelity,
        richTextSyncedVersion = Option.when(richTextInSync)(restoredDocument.contentVersion)
      )
    )

object BufferSnapshot:

  def fromBuffer(buffer: Buffer): BufferSnapshot =
    BufferSnapshot(
      content = buffer.document.content,
      richText = buffer.richText,
      richTextInSync = buffer.richTextInSync,
      editing = buffer.editing,
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

  /** A buffer's line ending as it stood before it was changed (#1964). Only the document's save settings travel, never
    * its text, so it composes with [[BufferEdit]] entries in any order. `revision` tells a restore whether a save has
    * happened since: the file on disk then has the other ending, and the restored buffer differs from it whatever it
    * was before.
    */
  final case class LineEndingChange(
      bufferId: BufferId,
      lineEnding: LineEnding,
      mixedLineEndings: Option[LineEndingCounts],
      isDirty: Boolean,
      revision: Option[DocumentRevision]
  ) extends HistoryEntry:

    def restore(state: AppState): Option[(AppState, HistoryEntry)] =
      state.persisted.buffers.get(bufferId).map { current =>
        val document = current.document.copy(
          lineEnding = lineEnding,
          mixedLineEndings = mixedLineEndings,
          isDirty = isDirty || current.document.revision != revision
        )
        val restored = state.copy(persisted =
          state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> current.copy(document = document)))
        )
        (restored, LineEndingChange.capture(current))
      }

  object LineEndingChange:

    def capture(buffer: Buffer): LineEndingChange =
      LineEndingChange(
        buffer.id,
        buffer.document.lineEnding,
        buffer.document.mixedLineEndings,
        buffer.document.isDirty,
        buffer.document.revision
      )

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
    undoStack: Vector[HistoryEntry] = Vector.empty,
    redoStack: Vector[HistoryEntry] = Vector.empty,
    pendingGroup: Option[HistoryEntry.BufferEdit] = None,
    maxUndoDepth: Int = UndoState.DefaultMaxUndoDepth
):

  def flushPendingGroup: UndoState =
    pendingGroup match
      case None        => this
      case Some(entry) => pushUndo(entry, clearRedo = false).copy(pendingGroup = None)

  def clearRedo: UndoState = copy(redoStack = Vector.empty)

  /** Forgets every buffer edit whose buffer is no longer `live`. A closed buffer's entries can never be replayed -- a
    * reopened file is a new buffer under a fresh id -- yet each holds a rope, and the one at the head of a stack would
    * stop undo and redo cold, since restoring it finds no buffer. The same instance comes back when nothing is lost.
    */
  def retainingBuffers(live: BufferId => Boolean): UndoState =
    val retainedUndo    = UndoState.retained(undoStack, live)
    val retainedRedo    = UndoState.retained(redoStack, live)
    val retainedPending = pendingGroup.filter(group => live(group.bufferId))
    if (retainedUndo eq undoStack) && (retainedRedo eq redoStack) && (retainedPending eq pendingGroup) then this
    else copy(undoStack = retainedUndo, redoStack = retainedRedo, pendingGroup = retainedPending)

  def pushUndo(entry: HistoryEntry, clearRedo: Boolean = true): UndoState =
    copy(
      undoStack = boundedPush(entry, undoStack),
      redoStack = if clearRedo then Vector.empty else redoStack
    )

  def pushRedo(entry: HistoryEntry): UndoState =
    copy(redoStack = boundedPush(entry, redoStack))

  // `Vector` keeps this O(1) amortized on every push, not just below the cap: prepending (`+:`) and dropping the
  // oldest entry off the far end (`dropRight(1)`) are both effectively-constant-time operations on a `Vector`, unlike a
  // `List`, which has no cheap way to drop its last element. The prior fix only avoided the O(maxUndoDepth) `take`
  // copy while under the cap; every push once the stack reached the cap -- the steady state for any session longer
  // than `maxUndoDepth` edits -- still paid it in full (#1455).
  private def boundedPush(entry: HistoryEntry, stack: Vector[HistoryEntry]): Vector[HistoryEntry] =
    val pushed = entry +: stack
    if pushed.lengthIs <= effectiveMaxUndoDepth then pushed else pushed.dropRight(1)

  private def effectiveMaxUndoDepth: Int =
    math.max(1, maxUndoDepth)

object UndoState:
  val DefaultMaxUndoDepth: Int = 1000

  private def retained(stack: Vector[HistoryEntry], live: BufferId => Boolean): Vector[HistoryEntry] =
    if stack.forall(isLive(_, live)) then stack else stack.filter(isLive(_, live))

  private def isLive(entry: HistoryEntry, live: BufferId => Boolean): Boolean =
    entry match
      case edit: HistoryEntry.BufferEdit         => live(edit.bufferId)
      case change: HistoryEntry.LineEndingChange => live(change.bufferId)
      case _                                     => true
