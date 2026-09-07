package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.*
import com.serenity.state.models.*

/** Top-level dispatch for every editor text-entry and navigation event. The event-handling logic itself is split by
  * family into sibling objects in this package -- [[EditorTextEditReducer]] (insertion/deletion),
  * [[EditorNavigationEventReducer]] (caret movement/paging), [[EditorVerticalNavigationReducer]] (Up/Down, which alone
  * needs measured geometry), [[EditorClipboardEventReducer]] (copy/cut/paste) and [[EditorFindEventReducer]]
  * (goto-line/find/replace) -- with [[EditorCursorMovement]] and [[EditorEditSupport]] holding the primitives more than
  * one family shares. This file keeps the entry points, the per-buffer routing that decides which family handles an
  * event, and the handful of geometry-free helpers (`homeTarget`/`endTarget`/line counting) too small to be their own
  * family.
  */
object EditorEventReducer:
  private[reducers] val TabInsertion = "    "

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

  /** Bundles the per-event state `reduceCursorsTextEvent` computes once (the seeded buffer/state and the cardinality
    * flags every family function below branches on) so each family function takes one parameter instead of five.
    */
  final private[reducers] case class CursorEventContext(
      buffer: Buffer,
      head: CursorPosition,
      hasSelection: Boolean,
      isMulti: Boolean,
      currentState: AppState,
      paneId: PaneId
  )

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

  private[reducers] def lineColumnToOffset(rope: Rope, line: Int, column: Int): Int =
    rope.lineColumnToOffset(line, column)

  private[reducers] def findLineEnd(content: Rope, line: Int): Int =
    val lineStart = lineColumnToOffset(content, line, 0)
    val lineEnd   = lineColumnToOffset(content, line, Int.MaxValue)
    lineEnd - lineStart

  /** Whether Home/End (and Up/Down, via `EditorVerticalNavigationReducer`) should move by visual row rather than
    * logical line -- word wrap has to be on for "visual row" to mean anything different from the logical line at all,
    * and the setting is independently toggleable on top of it.
    */
  private[reducers] def useVisualLineNavigation(state: AppState): Boolean =
    state.persisted.config.surfaceConfig.wordWrapEnabled &&
      state.persisted.config.surfaceConfig.visualLineCursorNavigation

  /** Home's landing column: the start of the cursor's current *visual* row when visual-line navigation applies,
    * otherwise column 0 of the logical line (`MoveToStartOfFile`-style callers that want the true buffer start
    * regardless of wrapping use `MoveToStartOfFile`, not this). Falls back to the logical start if no geometry is
    * available for this pane (e.g. no buffer content yet) even when the setting is on, matching `verticalTarget`'s own
    * `fallbackVerticalMove` fallback pattern for the same situation.
    */
  private[reducers] def homeTarget(state: AppState, paneId: PaneId, cursor: CursorPosition): CursorPosition =
    if useVisualLineNavigation(state) then
      com.serenity.state.manager.EditorGeometryProducer
        .forPane(state, paneId)
        // The cursor's own affinity, not a normalised one: Home after End has to find the row End left it on, or it
        // reads as already being at the start of the row below and does nothing.
        .flatMap(_.navigation.visualLineFor(cursor))
        .map(line => CursorPosition(cursor.line, line.startColumn))
        .getOrElse(CursorPosition(cursor.line, 0))
    else CursorPosition(cursor.line, 0)

  /** End's landing column: the end of the cursor's current *visual* row when visual-line navigation applies, otherwise
    * the logical line's own end (`findLineEnd`). Same geometry-missing fallback as [[homeTarget]].
    */
  private[reducers] def endTarget(
    state: AppState,
    paneId: PaneId,
    buffer: Buffer,
    cursor: CursorPosition
  ): CursorPosition =
    if useVisualLineNavigation(state) then
      com.serenity.state.manager.EditorGeometryProducer
        .forPane(state, paneId)
        // The cursor's own affinity, so a second End is idempotent rather than walking down a row at a time.
        .flatMap(_.navigation.visualLineFor(cursor))
        // A wrapped row's end is the next row's start, so the landing column alone would put the caret at the far left
        // of the row below; upstream affinity is what says the cursor stayed on the row End was pressed on. The
        // logical line's own end is never shared with another row, so marking it there is harmless.
        .map(line => CursorPosition(cursor.line, line.endColumn).upstream)
        .getOrElse(CursorPosition(cursor.line, findLineEnd(buffer.document.content, cursor.line)))
    else CursorPosition(cursor.line, findLineEnd(buffer.document.content, cursor.line))

  private[reducers] def countLines(rope: Rope): Int =
    rope.lineCount
