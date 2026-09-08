package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.*
import com.serenity.state.models.*

/** Top-level dispatch for every editor text-entry and navigation event. The event-handling logic itself is split by
  * family into sibling objects in this package -- [[EditorTextEditReducer]] (insertion/deletion),
  * [[EditorNavigationEventReducer]] (caret movement/paging), [[EditorVerticalNavigationReducer]] (Up/Down, which alone
  * needs measured geometry), [[EditorClipboardEventReducer]] (copy/cut/paste) and [[EditorFindEventReducer]]
  * (goto-line/find/replace) -- with [[EditorCursorMovement]] and [[EditorEditSupport]] holding the primitives more than
  * one family shares, and [[EditorCursorSupport]] holding the per-event context bundle plus the geometry-free
  * home/end/line-counting helpers those families use. This file keeps only the entry points and the per-buffer routing
  * that decides which family handles an event.
  */
object EditorEventReducer:
  import EditorCursorSupport.{countLines, endTarget, homeTarget, CursorEventContext, TabInsertion}

  def reducer(paneId: PaneId)(using balance: com.serenity.rope.Balance): Reducer[TextEntryEvent] =
    Reducer.instance((event, state) => reduce(event, paneId, state))

  def reduce(
    event: TextEntryEvent,
    paneId: PaneId,
    currentState: AppState
  )(using balance: com.serenity.rope.Balance): ReducerResult =
    currentState.persisted.layout.editorPanes.get(paneId) match
      case Some(pane) => reduceForPane(event, paneId, pane, currentState)
      case None       => ReducerResult.noEffects(currentState)

  /** Vertical movement is the one editor reduction whose result depends on measured text geometry, so it lives in its
    * own [[EditorVerticalNavigationReducer]], geometry-free `reduce` and takes the geometry the effect boundary
    * produced for this pane.
    */
  def reduceVerticalNavigation(
    event: VerticalNavigationEvent,
    paneId: PaneId,
    currentState: AppState,
    geometry: EditorGeometry
  ): ReducerResult =
    EditorVerticalNavigationReducer.reduce(event, paneId, currentState, geometry)

  private def reduceForPane(
    event: TextEntryEvent,
    paneId: PaneId,
    pane: EditorPane,
    currentState: AppState
  )(using balance: com.serenity.rope.Balance): ReducerResult =
    event match
      case ScrollDown(lines) =>
        pane.bufferId.flatMap(currentState.persisted.buffers.get) match
          case Some(buffer) =>
            val totalLines    = countLines(buffer.document.content)
            val maxTopLine    = math.max(0, totalLines - buffer.viewport.visibleLines)
            val newTopLine    = math.min(buffer.viewport.topLine + lines, maxTopLine)
            val newViewport   = buffer.viewport.copy(topLine = newTopLine, topVisualLine = 0)
            val updatedBuffer = buffer.copy(viewport = newViewport)
            ReducerResult.noEffects(
              currentState.copy(persisted =
                currentState.persisted.copy(buffers = currentState.persisted.buffers + (buffer.id -> updatedBuffer))
              )
            )
          case None => ReducerResult.noEffects(currentState)

      case ScrollUp(lines) =>
        pane.bufferId.flatMap(currentState.persisted.buffers.get) match
          case Some(buffer) =>
            val newTopLine    = math.max(0, buffer.viewport.topLine - lines)
            val newViewport   = buffer.viewport.copy(topLine = newTopLine, topVisualLine = 0)
            val updatedBuffer = buffer.copy(viewport = newViewport)
            ReducerResult.noEffects(
              currentState.copy(persisted =
                currentState.persisted.copy(buffers = currentState.persisted.buffers + (buffer.id -> updatedBuffer))
              )
            )
          case None => ReducerResult.noEffects(currentState)

      case textEvent: TextEntryEvent =>
        reduceTextEvent(textEvent, paneId, pane, currentState)

  private def reduceTextEvent(
    event: TextEntryEvent,
    paneId: PaneId,
    pane: EditorPane,
    currentState: AppState
  )(using balance: com.serenity.rope.Balance): ReducerResult =
    pane.bufferId match
      case Some(bufferId) =>
        currentState.persisted.buffers.get(bufferId) match
          case Some(buffer) => reduceTextEventForBuffer(event, buffer, paneId, currentState)
          case None         => ReducerResult.noEffects(currentState)
      case None =>
        handleEventWithoutBuffer(event, paneId, pane, currentState)

  private def reduceTextEventForBuffer(
    event: TextEntryEvent,
    buffer: Buffer,
    paneId: PaneId,
    currentState: AppState
  ): ReducerResult =
    val result = reduceCursorsTextEvent(event, buffer, paneId, currentState)

    if refreshesFindResults(event) then result.copy(state = invalidateFindState(result.state, buffer.id))
    else result

  private def refreshesFindResults(event: TextEntryEvent): Boolean =
    event match
      case InsertChar(_) | TabKey | ReverseTabKey | DeleteBackward | DeleteForward | DeleteWordBackward |
          DeleteWordForward | NewLine | Enter | Paste | Cut =>
        true
      case _ =>
        false

  private def isExtendSelectionEvent(event: TextEntryEvent): Boolean =
    event match
      case ExtendSelectionLeft | ExtendSelectionRight | ExtendSelectionWordLeft | ExtendSelectionWordRight |
          ExtendSelectionToLineStart | ExtendSelectionToLineEnd | ExtendSelectionPageUp | ExtendSelectionPageDown =>
        true
      case _ => false

  private def invalidateFindState(state: AppState, bufferId: BufferId): AppState =
    state.persisted.buffers.get(bufferId) match
      case Some(buffer) =>
        state.copy(persisted =
          state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer.copy(findState = None)))
        )
      case _ =>
        state

  /** One path over `buffer.cursorList` (see its doc comment) handles single cursor, multi-cursor and multi-selection
    * alike. `hasSelection`/`isMulti` name which of the three shapes this buffer is in; the family objects this
    * dispatches to still branch on them because the three shapes genuinely compute different results (an active
    * selection replaces its range, a bare cursor inserts at a point), not because the branches are copies of each other
    * -- the duplication `#994` set out to remove was the surrounding dispatch (three whole functions keyed on
    * cardinality), not these per-event differences.
    *
    * `ExtendSelectionLeft`/`ExtendSelectionRight`/`ExtendSelectionWordLeft`/`ExtendSelectionWordRight` stay
    * single-representative-only, exactly as before `#994`: extending a selection has only ever operated on the buffer's
    * first cursor regardless of how many cursors are live, via `isExtendSelectionEvent`'s pre-dispatch gate below.
    */
  private def reduceCursorsTextEvent(
    event: TextEntryEvent,
    rawBuffer: Buffer,
    paneId: PaneId,
    incomingState: AppState
  ): ReducerResult =
    import EditorCursorMovement.*

    rawBuffer.editing.cursors.headOption match
      case None =>
        val currentState  = Focused.replaceBuffer(incomingState, rawBuffer)
        val defaultCursor = CursorPosition(0, 0)
        val updatedPane   = currentState.persisted.layout.editorPanes(paneId).copy(cursors = List(defaultCursor))
        ReducerResult.noEffects(
          currentState.copy(persisted =
            currentState.persisted.copy(
              layout = currentState.persisted.layout.copy(
                editorPanes = currentState.persisted.layout.editorPanes + (paneId -> updatedPane)
              )
            )
          )
        )

      case Some(head) if isExtendSelectionEvent(event) =>
        val buffer       = clearInFlightMultiCursorVerticalState(rawBuffer)
        val currentState = Focused.replaceBuffer(incomingState, buffer)
        event match
          case ExtendSelectionLeft      => reduceSelectionExtension(buffer, head, currentState)(leftTarget)
          case ExtendSelectionRight     => reduceSelectionExtension(buffer, head, currentState)(rightTarget)
          case ExtendSelectionWordLeft  => reduceSelectionExtension(buffer, head, currentState)(wordLeftTarget)
          case ExtendSelectionWordRight => reduceSelectionExtension(buffer, head, currentState)(wordRightTarget)
          // The same landing places Home and End move to, rather than the logical line's own bounds: a shifted key
          // selects to where its unshifted form goes, and under word wrap that is the cursor's own visual row. Sharing
          // `homeTarget`/`endTarget` also carries their row affinity, so Shift+End stops at the row's end instead of
          // reading as the start of the row below (#1292).
          case ExtendSelectionToLineStart =>
            reduceSelectionExtension(buffer, head, currentState)((_, from) =>
              horizontalTarget(homeTarget(currentState, paneId, from))
            )
          case ExtendSelectionToLineEnd =>
            reduceSelectionExtension(buffer, head, currentState)((target, from) =>
              horizontalTarget(endTarget(currentState, paneId, target, from))
            )
          case ExtendSelectionPageUp =>
            reduceSelectionExtension(buffer, head, currentState)((target, from) =>
              horizontalTarget(pageTarget(target, currentState, paneId, direction = -1)(from))
            )
          case ExtendSelectionPageDown =>
            reduceSelectionExtension(buffer, head, currentState)((target, from) =>
              horizontalTarget(pageTarget(target, currentState, paneId, direction = 1)(from))
            )
          case _ => ReducerResult.noEffects(currentState)

      case Some(head) =>
        val rawCursors   = rawBuffer.cursorList
        val hasSelection = rawCursors.head.selectionAnchor.isDefined
        val isMulti      = rawCursors.tail.nonEmpty
        // A single bare cursor is the only shape whose event bodies below don't already clear in-flight multi-cursor
        // vertical state themselves (`applyMultiCursor*`/`applyLine*` all do); clear it here so a later event that
        // becomes genuinely multi-cursor again doesn't inherit vertical state pinned to stale cursor positions.
        val buffer = if !hasSelection && !isMulti then clearInFlightMultiCursorVerticalState(rawBuffer) else rawBuffer
        val currentState = Focused.replaceBuffer(incomingState, buffer)
        val ctx          = CursorEventContext(buffer, head, hasSelection, isMulti, currentState, paneId)

        event match
          case InsertChar(_) | TabKey | NewLine | Enter | ReverseTabKey | DeleteBackward | DeleteForward |
              DeleteWordBackward | DeleteWordForward =>
            EditorTextEditReducer.reduce(event, ctx)

          case MoveLeft | MoveRight | MoveWordLeft | MoveWordRight | MoveToStart | MoveToEnd | MoveToStartOfFile |
              PageUp | PageDown | MoveToEndOfFile | SelectAll =>
            EditorNavigationEventReducer.reduce(event, ctx)

          case OpenGotoLine | OpenFind | OpenReplace | FindNext =>
            EditorFindEventReducer.reduce(event, ctx)

          case Copy | Cut | Paste =>
            EditorClipboardEventReducer.reduce(event, ctx)

          case _ =>
            ReducerResult.noEffects(currentState)

  private def handleEventWithoutBuffer(
    event: TextEntryEvent,
    paneId: PaneId,
    pane: EditorPane,
    currentState: AppState
  )(using balance: com.serenity.rope.Balance): ReducerResult =
    event match
      case InsertChar(char) =>
        val bufferId    = currentState.runtime.nextBufferId
        val fresh       = Buffer.fromString(bufferId, char.toString)
        val buffer      = fresh.copy(document = fresh.document.copy(isDirty = true, isNewEmpty = false))
        val newCursor   = CursorPosition(0, 1)
        val updatedPane = pane.copy(bufferId = Some(bufferId), cursors = List(newCursor))
        val (bufferWithAnimation, delta) = EditorEditSupport.addInsertionAnimations(
          buffer,
          currentState,
          List(EditorEditSupport.MultiCursorEdit(0, 0, 0, char.toString))
        )
        ReducerResult(
          currentState.copy(
            persisted = currentState.persisted.copy(
              buffers = currentState.persisted.buffers + (bufferId -> bufferWithAnimation),
              layout = currentState.persisted.layout.copy(
                editorPanes = currentState.persisted.layout.editorPanes + (paneId -> updatedPane)
              )
            ),
            runtime = currentState.runtime.copy(nextBufferId = BufferId(bufferId.value + 1))
          ),
          EditorEditSupport.animationMergeEffects(bufferId, delta)
        )

      case TabKey =>
        TabInsertion.foldLeft(ReducerResult.noEffects(currentState)) { (result, char) =>
          handleEventWithoutBuffer(InsertChar(char), paneId, pane, result.state)
        }

      case _ =>
        ReducerResult.noEffects(currentState)
