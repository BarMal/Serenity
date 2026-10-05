package com.serenity.state.undo

import com.serenity.rope.Rope
import com.serenity.state.models.*
import com.serenity.state.reducers.EditorEditSupport
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
    isNewEmpty: Boolean,
    // The `Document.savedGeneration` the buffer was clean in, if it was: restoring this snapshot within that same
    // generation brings back the saved text, so the buffer is clean again (#1930).
    cleanIn: Option[Long]
):

  def restoreInto(buffer: Buffer): Buffer =
    // Bumped even when `content` is the current text: whatever was stamped against the current version (the rich
    // text, an outline) was stamped against the state being undone, not this one.
    val restoredDocument = buffer.document
      .withContent(content)
      .copy(isNewEmpty = isNewEmpty, isDirty = !cleanIn.contains(buffer.document.savedGeneration))
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
      isNewEmpty = buffer.document.isNewEmpty,
      cleanIn = Option.when(!buffer.document.isDirty)(buffer.document.savedGeneration)
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

/** A recorded change, numbered in the order changes were recorded across every history, so undo can tell which of two
  * histories changed last.
  */
final case class HistoryStep(number: Long, entry: HistoryEntry)

/** One undo stack and its redo stack, newest first, each holding at most `maxDepth` steps once pushed to. */
final case class HistoryStacks(undo: Vector[HistoryStep] = Vector.empty, redo: Vector[HistoryStep] = Vector.empty):

  def pushedUndo(step: HistoryStep, maxDepth: Int): HistoryStacks =
    copy(undo = HistoryStacks.boundedPush(step, undo, maxDepth))

  def pushedRedo(step: HistoryStep, maxDepth: Int): HistoryStacks =
    copy(redo = HistoryStacks.boundedPush(step, redo, maxDepth))

  def withoutRedo: HistoryStacks = if redo.isEmpty then this else copy(redo = Vector.empty)

object HistoryStacks:

  // `Vector` keeps this O(1) amortized on every push, not just below the cap: prepending (`+:`) and dropping the
  // oldest entry off the far end (`dropRight(1)`) are both effectively-constant-time operations on a `Vector`, unlike a
  // `List`, which has no cheap way to drop its last element. The prior fix only avoided the O(maxUndoDepth) `take`
  // copy while under the cap; every push once the stack reached the cap -- the steady state for any session longer
  // than `maxUndoDepth` edits -- still paid it in full (#1455).
  private def boundedPush(step: HistoryStep, stack: Vector[HistoryStep], maxDepth: Int): Vector[HistoryStep] =
    val pushed = step +: stack
    if pushed.lengthIs <= math.max(1, maxDepth) then pushed else pushed.dropRight(1)

/** Where a buffer's cursors and selections stood: what must be unchanged for the next edit to continue a run. */
final case class CaretMarks(marks: List[(CursorPosition, Option[CursorPosition])])

object CaretMarks:

  def of(editing: EditingState): CaretMarks =
    CaretMarks(editing.cursors.toList.map(cursor => cursor.position -> cursor.selectionAnchor))

/** Edits still coalescing into the undo step numbered `step`: all of `kind`, the latest leaving the cursors at
  * `carets`.
  */
final case class EditRun(paneId: PaneId, step: Long, kind: EditKind, carets: CaretMarks)

final case class BufferHistory(stacks: HistoryStacks = HistoryStacks(), openRun: Option[EditRun] = None)

/** The history an undo or redo acts on: the working buffer's, shown in `paneId`, or the layout's. */
enum HistoryOwner:
  case OfBuffer(bufferId: BufferId, paneId: PaneId)
  case OfLayout

/** Undo/redo history, held separately from AppState in StateManager. Never persisted to disk — always starts fresh.
  *
  * Each buffer keeps its own history (#1930), so undo only ever touches the buffer being worked in, and closing a
  * buffer drops its history with it. Pane and panel changes share a separate layout history, so toggling panels never
  * pushes text edits out of a buffer's bounded stack.
  */
final case class UndoState(
    buffers: Map[BufferId, BufferHistory] = Map.empty,
    layout: HistoryStacks = HistoryStacks(),
    recordedSteps: Long = 0L,
    maxUndoDepth: Int = UndoState.DefaultMaxUndoDepth
):

  /** Every undoable step in every history, most recently recorded first. */
  def undoStack: Vector[HistoryEntry] = merged(_.undo)

  def redoStack: Vector[HistoryEntry] = merged(_.redo)

  def pushUndo(entry: HistoryEntry, clearRedo: Boolean = true): UndoState =
    val step = HistoryStep(recordedSteps, entry)
    withStacksFor(entry)(stacks => (if clearRedo then stacks.withoutRedo else stacks).pushedUndo(step, maxUndoDepth))
      .copy(recordedSteps = recordedSteps + 1)

  def pushRedo(entry: HistoryEntry): UndoState =
    val step = HistoryStep(recordedSteps, entry)
    withStacksFor(entry)(_.pushedRedo(step, maxUndoDepth)).copy(recordedSteps = recordedSteps + 1)

  /** Records `entry` as a new undo step, or -- for a [[EditGrouping.Coalescing]] edit that picks up exactly where its
    * buffer's open run left off -- folds it into that run's step. `carets` is where the edit left the buffer's cursors
    * and `paused` whether the key making the edit came over [[EditClock.Pause]] after the one before. A run stays open
    * only while nothing else has been recorded since it, in any history; it closes at a cursor jump, a pause
    * (`paused`), a change of edit kind, whitespace typed after a word, and any clean buffer, so undo can stop at the
    * saved text.
    */
  def recorded(entry: HistoryEntry, grouping: EditGrouping, carets: CaretMarks, paused: Boolean): UndoState =
    (entry, grouping) match
      case (edit: HistoryEntry.BufferEdit, EditGrouping.Coalescing(kind, afterWord)) =>
        if continuesRun(edit, kind, afterWord, paused) then
          withRun(edit.bufferId, EditRun(edit.paneId, recordedSteps - 1, kind, carets))(
            _.withoutRedo
          )
        else
          val pushed = pushUndo(edit)
          pushed.withRun(edit.bufferId, EditRun(edit.paneId, recordedSteps, kind, carets))(identity)
      case _ => pushUndo(entry)

  /** What undo acts on in `state`: the newest step of the working buffer's history or of the layout history, whichever
    * was recorded last. Another buffer's history is never a candidate.
    */
  def nextUndo(state: AppState): Option[(HistoryOwner, HistoryEntry)] = newest(state, _.undo)

  def nextRedo(state: AppState): Option[(HistoryOwner, HistoryEntry)] = newest(state, _.redo)

  /** `owner`'s newest undo step moved to its redo stack as `inverse`, the state that step's restore replaced. */
  def undone(owner: HistoryOwner, inverse: HistoryEntry): UndoState =
    withStacksOf(owner)(stacks => stacks.copy(undo = stacks.undo.drop(1))).pushRedo(inverse)

  def redone(owner: HistoryOwner, inverse: HistoryEntry): UndoState =
    withStacksOf(owner)(stacks => stacks.copy(redo = stacks.redo.drop(1))).pushUndo(inverse, clearRedo = false)

  /** Forgets the history of every buffer that is no longer `live`: a closed buffer's steps can never be replayed -- a
    * reopened file is a new buffer under a fresh id -- yet each holds a rope. The same instance comes back when nothing
    * is lost.
    */
  def retainingBuffers(live: BufferId => Boolean): UndoState =
    if buffers.keysIterator.forall(live) then this
    else copy(buffers = buffers.filter((bufferId, _) => live(bufferId)))

  def forOpenBuffers(open: Map[BufferId, Buffer]): UndoState = retainingBuffers(open.contains)

  private def continuesRun(
    edit: HistoryEntry.BufferEdit,
    kind: EditKind,
    afterWord: Boolean,
    paused: Boolean
  ): Boolean =
    !afterWord && !paused && edit.snapshot.cleanIn.isEmpty && buffers
      .get(edit.bufferId)
      .flatMap(_.openRun)
      .exists(run =>
        run.paneId == edit.paneId &&
          run.step == recordedSteps - 1 &&
          run.kind == kind &&
          run.carets == CaretMarks.of(edit.snapshot.editing)
      )

  private def withRun(bufferId: BufferId, run: EditRun)(update: HistoryStacks => HistoryStacks): UndoState =
    copy(buffers =
      buffers.updatedWith(bufferId)(
        _.map(history => history.copy(stacks = update(history.stacks), openRun = Some(run)))
      )
    )

  private def newest(
    state: AppState,
    stack: HistoryStacks => Vector[HistoryStep]
  ): Option[(HistoryOwner, HistoryEntry)] =
    val bufferStep = UndoState.workingPane(state).flatMap { (paneId, bufferId) =>
      buffers
        .get(bufferId)
        .flatMap(history => stack(history.stacks).headOption)
        .map(step => (HistoryOwner.OfBuffer(bufferId, paneId), step))
    }
    val layoutStep = stack(layout).headOption.map(step => (HistoryOwner.OfLayout, step))
    (bufferStep.toList ++ layoutStep).maxByOption((_, step) => step.number).map((owner, step) => (owner, step.entry))

  private def withStacksFor(entry: HistoryEntry)(update: HistoryStacks => HistoryStacks): UndoState =
    entry match
      case edit: HistoryEntry.BufferEdit => withStacksOf(HistoryOwner.OfBuffer(edit.bufferId, edit.paneId))(update)
      case _                             => withStacksOf(HistoryOwner.OfLayout)(update)

  // Any change to a buffer's stacks ends its open run: the run's step is no longer the one an edit would fold into.
  private def withStacksOf(owner: HistoryOwner)(update: HistoryStacks => HistoryStacks): UndoState =
    owner match
      case HistoryOwner.OfBuffer(bufferId, _) =>
        val stacks = buffers.get(bufferId).fold(HistoryStacks())(_.stacks)
        copy(buffers = buffers.updated(bufferId, BufferHistory(update(stacks))))
      case HistoryOwner.OfLayout => copy(layout = update(layout))

  private def merged(stack: HistoryStacks => Vector[HistoryStep]): Vector[HistoryEntry] =
    if buffers.isEmpty then stack(layout).map(_.entry)
    else
      (buffers.values.toVector.flatMap(history => stack(history.stacks)) ++ stack(layout))
        .sortBy(-_.number)
        .map(_.entry)

object UndoState:
  val DefaultMaxUndoDepth: Int = 1000

  /** The editor pane undo works in: the focused one, or -- while a panel has focus -- the active one. */
  private def workingPane(state: AppState): Option[(PaneId, BufferId)] =
    val paneId = state.persisted.focus match
      case Focus.EditorPane(focused) => Some(focused)
      case _                         => state.persisted.layout.activeEditorPaneId
    paneId.flatMap(pane => state.persisted.layout.editorPanes.get(pane).flatMap(_.bufferId).map(pane -> _))
