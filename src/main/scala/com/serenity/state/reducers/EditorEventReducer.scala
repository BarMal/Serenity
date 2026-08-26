package com.serenity.state.reducers

import com.serenity.animation.*
import com.serenity.keystroke.events.*
import com.serenity.richtext.{RichTextDocument, RichTextPosition, RichTextRange}
import com.serenity.rope.Rope
import com.serenity.state.models.*
import com.serenity.text.TextEditing

object EditorEventReducer:
  private val TabInsertion = "    "
  private val OriginCursor = CursorPosition(0, 0)

  def reducer(paneId: PaneId)(using balance: com.serenity.rope.Balance): Reducer[TextEntryEvent] =
    Reducer.instance((event, state) => reduce(event, paneId, state))

  def reduce(
    event: TextEntryEvent,
    paneId: PaneId,
    currentState: AppState
  )(using balance: com.serenity.rope.Balance): ReducerResult =
    currentState.layout.editorPanes.get(paneId) match
      case Some(pane) => reduceForPane(event, paneId, pane, currentState)
      case None       => ReducerResult.noEffects(currentState)

  /** Vertical movement is the one editor reduction whose result depends on measured text geometry, so it is separated
    * from the geometry-free `reduce` and takes the geometry the effect boundary produced for this pane. Routing mirrors
    * `reduceTextEventForBuffer`: extend-selection stays single-cursor, multi-selection collapses to its focuses, and a
    * multi-cursor buffer (or one carrying in-flight vertical state) moves every cursor.
    */
  def reduceVerticalNavigation(
    event: VerticalNavigationEvent,
    paneId: PaneId,
    currentState: AppState,
    geometry: EditorGeometry
  ): ReducerResult =
    currentState.layout.editorPanes.get(paneId).flatMap(_.bufferId).flatMap(currentState.buffers.get) match
      case Some(buffer) =>
        val direction = event match
          case MoveUp | ExtendSelectionUp     => -1
          case MoveDown | ExtendSelectionDown => 1
        event match
          case ExtendSelectionUp | ExtendSelectionDown =>
            extendVertical(clearInFlightMultiCursorVerticalState(buffer), currentState, geometry, direction)
          case MoveUp | MoveDown =>
            moveVerticalForBuffer(buffer, currentState, geometry, direction)
      case None => ReducerResult.noEffects(currentState)

  private def extendVertical(
    buffer: Buffer,
    incomingState: AppState,
    geometry: EditorGeometry,
    direction: Int
  ): ReducerResult =
    val currentState = Focused.replaceBuffer(incomingState, buffer)
    buffer.editing.cursors.headOption match
      case Some(cursor) =>
        reduceSelectionExtension(buffer, cursor, currentState)(verticalTarget(currentState, geometry, direction))
      case None => ReducerResult.noEffects(currentState)

  private def moveVerticalForBuffer(
    buffer: Buffer,
    incomingState: AppState,
    geometry: EditorGeometry,
    direction: Int
  ): ReducerResult =
    if buffer.allSelections.nonEmpty then
      val seeded    = Focused.replaceBuffer(incomingState, buffer)
      val collapsed = collapseSelectionsToFocus(buffer)
      multiVertical(collapsed, seeded, geometry, direction)
    else if buffer.editing.multiCursorVerticalStates.size > 1 || buffer.editing.cursors.size > 1 then
      multiVertical(buffer, incomingState, geometry, direction)
    else singleVertical(clearInFlightMultiCursorVerticalState(buffer), incomingState, geometry, direction)

  private def singleVertical(
    buffer: Buffer,
    incomingState: AppState,
    geometry: EditorGeometry,
    direction: Int
  ): ReducerResult =
    val currentState = Focused.replaceBuffer(incomingState, buffer)
    buffer.editing.cursors.headOption match
      case Some(cursor) =>
        reduceMovement(buffer, selectionFocusOrCursor(buffer, cursor), currentState)(
          verticalTarget(currentState, geometry, direction)
        )
      case None => ReducerResult.noEffects(currentState)

  private def multiVertical(
    buffer: Buffer,
    incomingState: AppState,
    geometry: EditorGeometry,
    direction: Int
  ): ReducerResult =
    val currentState = Focused.replaceBuffer(incomingState, buffer)
    ReducerResult.noEffects(
      Focused.replaceBuffer(currentState, applyMultiCursorVerticalNavigation(buffer, currentState, geometry, direction))
    )

  private def reduceForPane(
    event: TextEntryEvent,
    paneId: PaneId,
    pane: EditorPane,
    currentState: AppState
  )(using balance: com.serenity.rope.Balance): ReducerResult =
    event match
      case ScrollDown(lines) =>
        pane.bufferId.flatMap(currentState.buffers.get) match
          case Some(buffer) =>
            val totalLines    = countLines(buffer.document.content)
            val maxTopLine    = math.max(0, totalLines - buffer.viewport.visibleLines)
            val newTopLine    = math.min(buffer.viewport.topLine + lines, maxTopLine)
            val newViewport   = buffer.viewport.copy(topLine = newTopLine, topVisualLine = 0)
            val updatedBuffer = buffer.copy(viewport = newViewport)
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))
          case None => ReducerResult.noEffects(currentState)

      case ScrollUp(lines) =>
        pane.bufferId.flatMap(currentState.buffers.get) match
          case Some(buffer) =>
            val newTopLine    = math.max(0, buffer.viewport.topLine - lines)
            val newViewport   = buffer.viewport.copy(topLine = newTopLine, topVisualLine = 0)
            val updatedBuffer = buffer.copy(viewport = newViewport)
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))
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
        currentState.buffers.get(bufferId) match
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
      case ExtendSelectionLeft | ExtendSelectionRight => true
      case _                                          => false

  private def invalidateFindState(state: AppState, bufferId: BufferId): AppState =
    state.buffers.get(bufferId) match
      case Some(buffer) =>
        state.copy(buffers = state.buffers + (bufferId -> buffer.copy(findState = None)))
      case _ =>
        state

  private def clearInFlightMultiCursorVerticalState(buffer: Buffer): Buffer =
    if buffer.editing.multiCursorVerticalStates.isEmpty then buffer
    else buffer.copy(editing = buffer.editing.copy(multiCursorVerticalStates = Nil))

  /** All four deletions share a selection arm and differ only in the range they delete when there is none. */
  private def reduceDeletion(
    buffer: Buffer,
    currentState: AppState,
    withoutSelection: Buffer => Option[(Buffer, MultiCursorEdit)]
  ): ReducerResult =
    ReducerResult.fromTransition(
      currentState,
      Focused.modifyBufferWithIdAndEmit(buffer.id) { current =>
        val result = current.primarySelection match
          case Some(selection) => Some(deleteSelectedRange(current, selection))
          case None            => withoutSelection(current)
        result match
          case Some((updated, edit)) =>
            (updated, animationRemapEffects(buffer.id, current.document.content, updated.document.content, List(edit)))
          case None => (current, Nil)
      }
    )

  private def graphemeBackwardDeletion(
    buffer: Buffer,
    cursor: CursorPosition
  ): Option[(Buffer, MultiCursorEdit)] =
    val offset = lineColumnToOffset(buffer.document.content, cursor.line, cursor.column)
    backwardGraphemeDeletionRange(buffer.document.content, offset).map {
      case (start, end) =>
        val newContent = buffer.document.content.delete(start, end)
        val newCursor  = offsetToCursorPosition(newContent, start)
        val updated = buffer.copy(
          document = buffer.document.copy(content = newContent, isDirty = true, isNewEmpty = false),
          editing = buffer.editing.copy(
            cursors = newCursor :: buffer.editing.cursors.tail,
            selection = None,
            preferredColumn = Some(newCursor.column),
            preferredXPx = None
          ),
          annotations = buffer.annotations.copy(
            documentComments = adjustDocumentComments(
              buffer.annotations.documentComments,
              buffer.document.content,
              newContent,
              List(MultiCursorEdit(0, start, end, ""))
            )
          ),
          richText = buffer.richText.copy(richTextDocument = richTextDocumentAfterEdit(buffer, start, end, ""))
        )
        (updated, MultiCursorEdit(0, start, end, ""))
    }

  /** Forward deletion leaves the cursor where it is, so unlike the backward case it does not adjust the viewport. */
  private def graphemeForwardDeletion(buffer: Buffer, cursor: CursorPosition): Option[(Buffer, MultiCursorEdit)] =
    val offset = lineColumnToOffset(buffer.document.content, cursor.line, cursor.column)
    forwardGraphemeDeletionRange(buffer.document.content, offset).map {
      case (start, end) =>
        val newContent = buffer.document.content.delete(start, end)
        val newCursor  = offsetToCursorPosition(newContent, start)
        val updated = buffer.copy(
          document = buffer.document.copy(content = newContent, isDirty = true, isNewEmpty = false),
          editing = buffer.editing.copy(
            cursors = newCursor :: buffer.editing.cursors.tail,
            preferredColumn = Some(newCursor.column),
            preferredXPx = None
          ),
          annotations = buffer.annotations.copy(
            documentComments = adjustDocumentComments(
              buffer.annotations.documentComments,
              buffer.document.content,
              newContent,
              List(MultiCursorEdit(0, start, end, ""))
            )
          ),
          richText = buffer.richText.copy(richTextDocument = richTextDocumentAfterEdit(buffer, start, end, ""))
        )
        (updated, MultiCursorEdit(0, start, end, ""))
    }

  private def wordBackwardDeletion(buffer: Buffer, cursor: CursorPosition): Option[(Buffer, MultiCursorEdit)] =
    val offset = lineColumnToOffset(buffer.document.content, cursor.line, cursor.column)
    val start  = previousWordBoundary(buffer.document.content, offset)
    Option.when(start < offset)(deleteOffsetRange(buffer, start, offset, start))

  private def wordForwardDeletion(buffer: Buffer, cursor: CursorPosition): Option[(Buffer, MultiCursorEdit)] =
    val offset = lineColumnToOffset(buffer.document.content, cursor.line, cursor.column)
    val end    = nextWordBoundary(buffer.document.content, offset)
    Option.when(offset < end)(deleteOffsetRange(buffer, offset, end, offset))

  private def insertAtCursor(
    buffer: Buffer,
    cursor: CursorPosition,
    text: String,
    currentState: AppState
  ): ReducerResult =
    ReducerResult.fromTransition(
      currentState,
      Focused.modifyBufferWithIdAndEmit(buffer.id) { current =>
        val (replaced, edit)  = replaceSelectionOrInsert(current, cursor, text)
        val (animated, delta) = addInsertionAnimations(replaced, currentState, List(edit))
        val effects =
          animationRemapEffects(buffer.id, current.document.content, animated.document.content, List(edit)) ++
            animationMergeEffects(buffer.id, delta)
        (animated, effects)
      }
    )

  private def animationRemapEffects(
    bufferId: BufferId,
    before: Rope,
    after: Rope,
    edits: List[MultiCursorEdit]
  ): List[AppEffect] =
    if edits.isEmpty then Nil
    else
      List(
        AppEffect.Animation(
          AnimationEffect.RemapThroughEdits(bufferId, before, after, edits.map(toTextEdit))
        )
      )

  private def animationMergeEffects(bufferId: BufferId, delta: Map[CharacterKey, AnimatedCell]): List[AppEffect] =
    if delta.isEmpty then Nil else List(AppEffect.Animation(AnimationEffect.Merge(bufferId, delta)))

  private def toTextEdit(edit: MultiCursorEdit): TextEdit =
    TextEdit(edit.start, edit.end, edit.insertedText)

  /** One path over `buffer.cursorList` (see its doc comment) handles single cursor, multi-cursor and multi-selection
    * alike. `hasSelection`/`isMulti` name which of the three shapes this buffer is in; most event bodies below still
    * branch on them because the three shapes genuinely compute different results (an active selection replaces its
    * range, a bare cursor inserts at a point), not because the branches are copies of each other -- the duplication
    * `#994` set out to remove was the surrounding dispatch (three whole functions keyed on cardinality), not these
    * per-event differences.
    *
    * Movement (`MoveLeft`/`MoveRight`/`MoveWordLeft`/`MoveWordRight`/`MoveToStart`/`MoveToEnd`/`MoveToStartOfFile`/
    * `PageUp`/`PageDown`) turned out to have no genuine per-cardinality difference at all: the old single-cursor bodies
    * and `applyMultiCursorNavigation`/`applyMultiCursorPageNavigation` compute identically-shaped results for one
    * cursor, so all of them now share the one multi-cursor-shaped implementation (selections collapse to their focus
    * first, exactly as the old multi-selection path already did for the subset of these events it covered). That subset
    * excluded `MoveWordLeft`/`MoveWordRight`: unhandled by both multi-cursor and multi-selection, they fell through to
    * the single-cursor body operating on `buffer.cursors.head` alone, silently leaving every other cursor behind.
    * Folding them into the shared movement handling fixes that rather than replicating it -- see
    * `MultiCursorEditingSpec`'s new word-movement coverage. `MoveToEndOfFile` keeps its own single-cursor body: it
    * scrolls the viewport to keep the new cursor visible where the multi-cursor form does not, and untangling whether
    * that asymmetry is deliberate is outside this change's scope.
    *
    * `ExtendSelectionLeft`/`ExtendSelectionRight` stay single-representative-only, exactly as before `#994`: extending
    * a selection has only ever operated on the buffer's first cursor regardless of how many cursors are live, via
    * `isExtendSelectionEvent`'s pre-dispatch gate in `reduceTextEventForBuffer`.
    */
  private def reduceCursorsTextEvent(
    event: TextEntryEvent,
    rawBuffer: Buffer,
    paneId: PaneId,
    incomingState: AppState
  ): ReducerResult =
    rawBuffer.editing.cursors.headOption match
      case None =>
        val currentState  = Focused.replaceBuffer(incomingState, rawBuffer)
        val defaultCursor = CursorPosition(0, 0)
        val updatedPane   = currentState.layout.editorPanes(paneId).copy(cursors = List(defaultCursor))
        ReducerResult.noEffects(
          currentState.copy(
            layout = currentState.layout.copy(
              editorPanes = currentState.layout.editorPanes + (paneId -> updatedPane)
            )
          )
        )

      case Some(head) if isExtendSelectionEvent(event) =>
        val buffer       = clearInFlightMultiCursorVerticalState(rawBuffer)
        val currentState = Focused.replaceBuffer(incomingState, buffer)
        event match
          case ExtendSelectionLeft  => reduceSelectionExtension(buffer, head, currentState)(leftTarget)
          case ExtendSelectionRight => reduceSelectionExtension(buffer, head, currentState)(rightTarget)
          case _                    => ReducerResult.noEffects(currentState)

      case Some(head) =>
        val rawCursors   = rawBuffer.cursorList
        val hasSelection = rawCursors.head.selectionAnchor.isDefined
        val isMulti      = rawCursors.tail.nonEmpty
        // A single bare cursor is the only shape whose event bodies below don't already clear in-flight multi-cursor
        // vertical state themselves (`applyMultiCursor*`/`applyLine*` all do); clear it here so a later event that
        // becomes genuinely multi-cursor again doesn't inherit vertical state pinned to stale cursor positions.
        val buffer = if !hasSelection && !isMulti then clearInFlightMultiCursorVerticalState(rawBuffer) else rawBuffer
        val currentState = Focused.replaceBuffer(incomingState, buffer)

        def applyBuffer(f: Buffer => Buffer): ReducerResult =
          ReducerResult.noEffects(Focused.replaceBuffer(currentState, f(buffer)))

        /** Like `applyBuffer`, but `f` also reports the edits it made, so their animations can be remapped in the
          * presentation layer (`#1001`) instead of inside `Buffer` itself.
          */
        def applyEditedBuffer(f: Buffer => (Buffer, List[MultiCursorEdit])): ReducerResult =
          val (updated, edits) = f(buffer)
          ReducerResult(
            Focused.replaceBuffer(currentState, updated),
            animationRemapEffects(buffer.id, buffer.document.content, updated.document.content, edits)
          )

        def navigate(moveFn: CursorPosition => CursorPosition): ReducerResult =
          applyBuffer(target =>
            applyMultiCursorNavigation(if hasSelection then collapseSelectionsToFocus(target) else target)(moveFn)
          )

        event match
          case InsertChar(char) =>
            if hasSelection then applyEditedBuffer(applyMultiSelectionReplacement(_, char.toString))
            else if isMulti then applyEditedBuffer(applyMultiCursorInsertion(_, char.toString))
            else insertAtCursor(buffer, head, char.toString, currentState)

          case TabKey =>
            if hasSelection then
              val (updated, edits, delta) = applyLineIndent(buffer, currentState, selectionLines(buffer))
              val effects =
                animationRemapEffects(buffer.id, buffer.document.content, updated.document.content, edits) ++
                  animationMergeEffects(buffer.id, delta)
              ReducerResult(Focused.replaceBuffer(currentState, updated), effects)
            else if isMulti then applyEditedBuffer(applyMultiCursorInsertion(_, TabInsertion))
            else insertAtCursor(buffer, head, TabInsertion, currentState)

          case NewLine | Enter =>
            if hasSelection then applyEditedBuffer(applyMultiSelectionReplacement(_, "\n"))
            else if isMulti then applyEditedBuffer(applyMultiCursorInsertion(_, "\n"))
            else insertAtCursor(buffer, head, "\n", currentState)

          case ReverseTabKey =>
            val targetLines =
              if hasSelection then selectionLines(buffer)
              else if isMulti then distinctCursorLines(buffer)
              else List(head.line)
            applyEditedBuffer(applyLineUnindent(_, targetLines))

          case DeleteBackward =>
            if hasSelection then applyEditedBuffer(deleteSelectedRanges)
            else if isMulti then applyEditedBuffer(applyMultiCursorDeletion(_, backward = true))
            else reduceDeletion(buffer, currentState, graphemeBackwardDeletion(_, head))

          case DeleteForward =>
            if hasSelection then applyEditedBuffer(deleteSelectedRanges)
            else if isMulti then applyEditedBuffer(applyMultiCursorDeletion(_, backward = false))
            else reduceDeletion(buffer, currentState, graphemeForwardDeletion(_, head))

          case DeleteWordBackward =>
            if hasSelection then applyEditedBuffer(deleteSelectedRanges)
            else if isMulti then applyEditedBuffer(applyMultiCursorWordDeletion(_, backward = true))
            else reduceDeletion(buffer, currentState, wordBackwardDeletion(_, head))

          case DeleteWordForward =>
            if hasSelection then applyEditedBuffer(deleteSelectedRanges)
            else if isMulti then applyEditedBuffer(applyMultiCursorWordDeletion(_, backward = false))
            else reduceDeletion(buffer, currentState, wordForwardDeletion(_, head))

          case MoveLeft      => navigate(cursor => moveCursorLeft(cursor, buffer.document.content))
          case MoveRight     => navigate(cursor => moveCursorRight(cursor, buffer.document.content))
          case MoveWordLeft  => navigate(cursor => wordBoundaryFrom(buffer, cursor, previousWordBoundary))
          case MoveWordRight => navigate(cursor => wordBoundaryFrom(buffer, cursor, nextWordBoundary))
          case MoveToStart   => navigate(_.copy(column = 0))
          case MoveToEnd => navigate(cursor => cursor.copy(column = findLineEnd(buffer.document.content, cursor.line)))
          case MoveToStartOfFile => navigate(_ => OriginCursor)

          case PageUp =>
            applyBuffer(target =>
              applyMultiCursorPageNavigation(
                if hasSelection then collapseSelectionsToFocus(target) else target,
                direction = -1
              )
            )
          case PageDown =>
            applyBuffer(target =>
              applyMultiCursorPageNavigation(
                if hasSelection then collapseSelectionsToFocus(target) else target,
                direction = 1
              )
            )

          case MoveToEndOfFile if !hasSelection && !isMulti =>
            val totalLines  = countLines(buffer.document.content)
            val lastLine    = totalLines - 1
            val lastLineEnd = findLineEnd(buffer.document.content, lastLine)
            val newCursor   = CursorPosition(lastLine, lastLineEnd)
            val newTopLine  = math.max(0, lastLine - buffer.viewport.visibleLines + 1)
            val newViewport = buffer.viewport.copy(topLine = newTopLine, topVisualLine = 0)
            applyBuffer(
              _.copy(
                editing = buffer.editing.copy(
                  cursors = List(newCursor),
                  preferredColumn = Some(newCursor.column),
                  preferredXPx = None
                ),
                viewport = newViewport
              )
            )

          case MoveToEndOfFile =>
            val totalLines = countLines(buffer.document.content)
            val lastLine   = totalLines - 1
            val target     = CursorPosition(lastLine, findLineEnd(buffer.document.content, lastLine))
            navigate(_ => target)

          case SelectAll =>
            ReducerResult.fromTransition(
              currentState,
              Focused.modifyBufferWithId(buffer.id) { current =>
                val lastLine  = math.max(0, countLines(current.document.content) - 1)
                val endCursor = CursorPosition(lastLine, findLineEnd(current.document.content, lastLine))
                current.copy(
                  editing = current.editing.copy(
                    cursors = List(endCursor),
                    selection = Some(Selection(CursorPosition(0, 0), endCursor)),
                    selections = Nil,
                    preferredColumn = Some(endCursor.column),
                    preferredXPx = None
                  )
                )
              }
            )

          case OpenGotoLine =>
            ModalStateReducer.show(Modal.GotoLine(""), currentState)

          case OpenFind =>
            ModalStateReducer.show(findModalForBuffer(buffer), currentState)

          case OpenReplace =>
            ModalStateReducer.show(Modal.ReplaceWorkflow(ReplaceWorkflowState()), currentState)

          case FindNext =>
            buffer.findState match
              case Some(FindState(query, storedResults, currentIndex)) if storedResults.nonEmpty =>
                val validResults = storedResults.filter { result =>
                  isWholeGraphemeMatch(
                    buffer.document.content,
                    lineColumnToOffset(buffer.document.content, result.line, result.column),
                    query.length
                  )
                }
                val resultSet = FindResultSet.normalized(query, validResults, currentIndex + 1)
                if resultSet.results.isEmpty then applyBuffer(_.copy(findState = None))
                else
                  val selected = resultSet.results(resultSet.currentIndex)
                  val target   = CursorPosition(selected.line, selected.column)
                  applyBuffer(
                    _.copy(
                      editing = buffer.editing.copy(
                        cursors = List(target),
                        selection = None,
                        selections = Nil,
                        preferredColumn = Some(target.column),
                        preferredXPx = None
                      ),
                      findState = Some(FindState.fromResultSet(resultSet))
                    )
                  )
              case _ =>
                ReducerResult.noEffects(currentState)

          case Copy =>
            if hasSelection then
              ReducerResult.noEffects(currentState.copy(clipboard = Some(selectedTexts(buffer).mkString("\n"))))
            else
              val clipboardText =
                distinctCursorLines(buffer)
                  .map(line => buffer.document.content.getLine(line).getOrElse(""))
                  .mkString("\n")
              ReducerResult.noEffects(currentState.copy(clipboard = Some(clipboardText)))

          case Cut =>
            if hasSelection then
              val (updated, edits) = deleteSelectedRanges(buffer)
              ReducerResult(
                currentState.copy(
                  buffers = currentState.buffers + (buffer.id -> updated),
                  clipboard = Some(selectedTexts(buffer).mkString("\n"))
                ),
                animationRemapEffects(buffer.id, buffer.document.content, updated.document.content, edits)
              )
            else
              val targetLines = distinctCursorLines(buffer)
              val clipboardText =
                targetLines.map(line => buffer.document.content.getLine(line).getOrElse("")).mkString("\n")
              val (updated, edits) = applyMultiCursorLineCut(buffer, targetLines)
              ReducerResult(
                currentState.copy(
                  buffers = currentState.buffers + (buffer.id -> updated),
                  clipboard = Some(clipboardText)
                ),
                animationRemapEffects(buffer.id, buffer.document.content, updated.document.content, edits)
              )

          case Paste =>
            currentState.clipboard.filter(_.nonEmpty) match
              case None => ReducerResult.noEffects(currentState)
              case Some(text) if hasSelection =>
                applyEditedBuffer(applyMultiSelectionReplacement(_, text))
              case Some(text) if isMulti =>
                applyEditedBuffer(applyMultiCursorInsertion(_, text))
              case Some(text) =>
                val (replacedBuffer, replacementEdit) = replaceSelectionOrInsert(buffer, head, text)
                val newCursor                         = replacedBuffer.editing.cursors.headOption.getOrElse(head)
                val withoutAnimations = buffer.copy(
                  document = buffer.document.copy(
                    content = replacedBuffer.document.content,
                    isDirty = replacedBuffer.document.isDirty,
                    isNewEmpty = replacedBuffer.document.isNewEmpty
                  ),
                  editing = buffer.editing.copy(
                    cursors = replacedBuffer.editing.cursors,
                    selection = replacedBuffer.editing.selection,
                    preferredColumn = Some(newCursor.column),
                    preferredXPx = None
                  )
                )
                val (updatedBuffer, delta) =
                  addInsertionAnimations(withoutAnimations, currentState, List(replacementEdit))
                val effects =
                  animationRemapEffects(
                    buffer.id,
                    buffer.document.content,
                    updatedBuffer.document.content,
                    List(replacementEdit)
                  ) ++
                    animationMergeEffects(buffer.id, delta)
                ReducerResult(
                  currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)),
                  effects
                )

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
        val bufferId    = currentState.nextBufferId
        val fresh       = Buffer.fromString(bufferId, char.toString)
        val buffer      = fresh.copy(document = fresh.document.copy(isDirty = true, isNewEmpty = false))
        val newCursor   = CursorPosition(0, 1)
        val updatedPane = pane.copy(bufferId = Some(bufferId), cursors = List(newCursor))
        val (bufferWithAnimation, delta) = addInsertionAnimations(
          buffer,
          currentState,
          List(MultiCursorEdit(0, 0, 0, char.toString))
        )
        ReducerResult(
          currentState.copy(
            buffers = currentState.buffers + (bufferId -> bufferWithAnimation),
            layout = currentState.layout.copy(
              editorPanes = currentState.layout.editorPanes + (paneId -> updatedPane)
            ),
            nextBufferId = BufferId(bufferId.value + 1)
          ),
          animationMergeEffects(bufferId, delta)
        )

      case TabKey =>
        TabInsertion.foldLeft(ReducerResult.noEffects(currentState)) { (result, char) =>
          handleEventWithoutBuffer(InsertChar(char), paneId, pane, result.state)
        }

      case _ =>
        ReducerResult.noEffects(currentState)

  private[reducers] def lineColumnToOffset(rope: Rope, line: Int, column: Int): Int =
    rope.lineColumnToOffset(line, column)

  private def findLineEnd(content: Rope, line: Int): Int =
    val lineStart = content.lineColumnToOffset(line, 0)
    val lineEnd   = content.lineColumnToOffset(line, Int.MaxValue)
    lineEnd - lineStart

  private def countLines(rope: Rope): Int =
    rope.lineCount

  final private case class CursorEntry(cursor: CursorPosition, offset: Int)
  final private case class MultiCursorEdit(ownerIndex: Int, start: Int, end: Int, insertedText: String)
  final private case class MultiCursorVerticalState(cursor: CursorPosition, preferredColumn: Int, preferredXPx: Float)

  private def applyMultiCursorInsertion(
    buffer: Buffer,
    insertedText: String
  ): (Buffer, List[MultiCursorEdit]) =
    val insertionOffsets =
      multiCursorEntries(buffer).map(entry => graphemeBoundaryAfterOrAt(buffer.document.content, entry.offset))
    val edits = insertionOffsets.zipWithIndex.map {
      case (offset, index) =>
        MultiCursorEdit(index, offset, offset, insertedText)
    }
    applyTrackedEdits(buffer, insertionOffsets, edits)

  private def applyMultiCursorDeletion(
    buffer: Buffer,
    backward: Boolean
  ): (Buffer, List[MultiCursorEdit]) =
    val entries = multiCursorEntries(buffer)
    val edits = entries.zipWithIndex.flatMap {
      case (entry, index) =>
        val range =
          if backward then backwardGraphemeDeletionRange(buffer.document.content, entry.offset)
          else forwardGraphemeDeletionRange(buffer.document.content, entry.offset)
        range.map { case (start, end) => MultiCursorEdit(index, start, end, "") }
    }
    applyTrackedEdits(buffer, entries.map(_.offset), edits)

  private def applyMultiCursorWordDeletion(
    buffer: Buffer,
    backward: Boolean
  ): (Buffer, List[MultiCursorEdit]) =
    val entries = multiCursorEntries(buffer)
    val edits = entries.zipWithIndex.flatMap {
      case (entry, index) =>
        if backward then
          val start = previousWordBoundary(buffer.document.content, entry.offset)
          Option.when(start < entry.offset)(MultiCursorEdit(index, start, entry.offset, ""))
        else
          val end = nextWordBoundary(buffer.document.content, entry.offset)
          Option.when(entry.offset < end)(MultiCursorEdit(index, entry.offset, end, ""))
    }
    applyMergedDeletionEdits(buffer, entries.map(_.offset), edits)

  /** Returns the buffer with the indent applied and its own edits (for the caller's animation remap), plus the
    * insertion-animation delta from `addInsertionAnimations` (for the caller's animation merge) -- two independent
    * animation effects, since one shifts existing animations and the other adds new ones.
    */
  private def applyLineIndent(
    buffer: Buffer,
    currentState: AppState,
    targetLines: List[Int]
  ): (Buffer, List[MultiCursorEdit], Map[CharacterKey, AnimatedCell]) =
    val targetSet = targetLines.filter(line => line >= 0 && line < countLines(buffer.document.content)).toSet

    if targetSet.isEmpty then (buffer, Nil, Map.empty)
    else
      val edits = targetSet.toList.sorted.zipWithIndex.map {
        case (line, index) =>
          val offset = lineColumnToOffset(buffer.document.content, line, 0)
          MultiCursorEdit(index, offset, offset, TabInsertion)
      }
      val updatedContent = edits
        .sortBy(edit => (-edit.start, -edit.end))
        .foldLeft(buffer.document.content)((content, edit) => content.insert(edit.start, edit.insertedText))
      val finalCursors = buffer.editing.cursors.map { cursor =>
        if targetSet.contains(cursor.line) then cursor.copy(column = cursor.column + TabInsertion.length)
        else cursor
      }.distinct
      val primaryCursor = finalCursors.headOption.getOrElse(CursorPosition(0, 0))
      val baseBuffer = buffer.copy(
        document = buffer.document.copy(content = updatedContent, isDirty = true, isNewEmpty = false),
        editing = buffer.editing.copy(
          cursors = finalCursors,
          selection = None,
          selections = Nil,
          preferredColumn = Some(primaryCursor.column),
          preferredXPx = None,
          multiCursorVerticalStates = Nil
        ),
        annotations = buffer.annotations.copy(
          documentComments =
            adjustDocumentComments(buffer.annotations.documentComments, buffer.document.content, updatedContent, edits)
        )
      )
      val (animatedBuffer, delta) = addInsertionAnimations(baseBuffer, currentState, edits)
      (animatedBuffer, edits, delta)

  private def applyLineUnindent(
    buffer: Buffer,
    targetLines: List[Int]
  ): (Buffer, List[MultiCursorEdit]) =
    val targetSet = targetLines.filter(line => line >= 0 && line < countLines(buffer.document.content)).toSet
    val removals = targetSet.toList.sorted.map { line =>
      val (_, removed) = unindentLine(buffer.document.content.getLine(line).getOrElse(""))
      line -> removed
    }.toMap

    if removals.values.forall(_ == 0) then (buffer, Nil)
    else
      val edits = removals.toList.sortBy(_._1).zipWithIndex.collect {
        case ((line, removed), index) if removed > 0 =>
          val start = lineColumnToOffset(buffer.document.content, line, 0)
          MultiCursorEdit(index, start, start + removed, "")
      }
      val updatedContent = edits
        .sortBy(edit => (-edit.start, -edit.end))
        .foldLeft(buffer.document.content)((content, edit) => content.delete(edit.start, edit.end))
      val finalCursors = buffer.editing.cursors
        .map(cursor => cursor.copy(column = math.max(0, cursor.column - removals.getOrElse(cursor.line, 0))))
        .distinct
      val primaryCursor = finalCursors.headOption.getOrElse(CursorPosition(0, 0))
      val baseBuffer = buffer.copy(
        document = buffer.document.copy(content = updatedContent, isDirty = true, isNewEmpty = false),
        editing = buffer.editing.copy(
          cursors = finalCursors,
          selection = None,
          selections = Nil,
          preferredColumn = Some(primaryCursor.column),
          preferredXPx = None,
          multiCursorVerticalStates = Nil
        ),
        annotations = buffer.annotations.copy(
          documentComments =
            adjustDocumentComments(buffer.annotations.documentComments, buffer.document.content, updatedContent, edits)
        )
      )
      (baseBuffer, edits)

  private def unindentLine(lineText: String): (String, Int) =
    if lineText.startsWith("\t") then (lineText.drop(1), 1)
    else
      val spacesToRemove = lineText.take(TabInsertion.length).takeWhile(_ == ' ').length
      if spacesToRemove == 0 then (lineText, 0)
      else (lineText.drop(spacesToRemove), spacesToRemove)

  private def applyMultiCursorLineCut(
    buffer: Buffer,
    targetLines: List[Int]
  ): (Buffer, List[MultiCursorEdit]) =
    if targetLines.isEmpty then (buffer, Nil)
    else
      val totalLines = countLines(buffer.document.content)
      val lineEdits = targetLines.distinct.sorted.map { line =>
        val lineText  = buffer.document.content.getLine(line).getOrElse("")
        val lineStart = lineColumnToOffset(buffer.document.content, line, 0)
        val lineEnd   = lineColumnToOffset(buffer.document.content, line, lineText.length)
        val (deleteStart, deleteEnd) =
          if line == 0 && totalLines == 1 then (0, lineEnd)
          else if line < totalLines - 1 then (lineStart, lineEnd + 1)
          else (math.max(0, lineStart - 1), lineEnd)
        (line, deleteStart, deleteEnd)
      }
      val updatedContent = lineEdits
        .sortBy { case (_, start, end) => (-start, -end) }
        .foldLeft(buffer.document.content) {
          case (content, (_, start, end)) =>
            content.delete(start, end)
        }
      val edits = lineEdits.zipWithIndex.map {
        case ((_, start, end), index) =>
          MultiCursorEdit(index, start, end, "")
      }
      val maxFinalLine = math.max(0, countLines(updatedContent) - 1)
      val finalCursors = targetLines.distinct.sorted.map { line =>
        val deletedBefore = targetLines.count(_ < line)
        val targetLine =
          if totalLines == 1 then 0
          else if line < totalLines - 1 then line - deletedBefore
          else line - targetLines.count(_ <= line)
        val clampedLine = math.max(0, math.min(targetLine, maxFinalLine))
        offsetToCursorPosition(updatedContent, lineColumnToOffset(updatedContent, clampedLine, 0))
      }.distinct
      val primaryCursor = finalCursors.headOption.getOrElse(CursorPosition(0, 0))
      val baseBuffer = buffer.copy(
        document = buffer.document.copy(content = updatedContent, isDirty = true, isNewEmpty = false),
        editing = buffer.editing.copy(
          cursors = finalCursors,
          selection = None,
          selections = Nil,
          preferredColumn = Some(primaryCursor.column),
          preferredXPx = None,
          multiCursorVerticalStates = Nil
        ),
        annotations = buffer.annotations.copy(
          documentComments =
            adjustDocumentComments(buffer.annotations.documentComments, buffer.document.content, updatedContent, edits)
        )
      )
      (baseBuffer, edits)

  private def applyTrackedEdits(
    buffer: Buffer,
    initialOffsets: List[Int],
    edits: List[MultiCursorEdit]
  ): (Buffer, List[MultiCursorEdit]) =
    if edits.isEmpty then (buffer, Nil)
    else
      val trackedOffsets = initialOffsets.toArray
      val sortedEdits    = edits.sortBy(edit => (-edit.start, -edit.end))
      val (updatedContent, updatedRichTextDocument) =
        sortedEdits.foldLeft((buffer.document.content, buffer.richText.richTextDocument)) {
          case ((content, document), edit) =>
            val deleted     = content.delete(edit.start, edit.end)
            val nextContent = deleted.insert(edit.start, edit.insertedText)
            val nextDocument = richTextDocumentAfterEdit(
              buffer.copy(
                document = buffer.document.copy(content = content),
                richText = buffer.richText.copy(richTextDocument = document)
              ),
              edit.start,
              edit.end,
              edit.insertedText
            )
            (nextContent, nextDocument)
        }
      val finalOffsets = sortedEdits.foldLeft(trackedOffsets) { (offsets, edit) =>
        val delta = edit.insertedText.length - (edit.end - edit.start)
        offsets.indices.foreach { i =>
          val offset = offsets(i)
          offsets(i) =
            if offset < edit.start then offset
            else if offset > edit.end then offset + delta
            else if i == edit.ownerIndex then edit.start + edit.insertedText.length
            else edit.start + edit.insertedText.length
        }
        offsets
      }
      val finalCursors = finalOffsets.toList
        .map(offset => offsetToCursorPosition(updatedContent, offset))
        .distinct
      val primaryCursor = finalCursors.headOption.getOrElse(CursorPosition(0, 0))
      val baseBuffer = buffer.copy(
        document = buffer.document.copy(content = updatedContent, isDirty = true, isNewEmpty = false),
        editing = buffer.editing.copy(
          cursors = finalCursors,
          selection = None,
          selections = Nil,
          preferredColumn = Some(primaryCursor.column),
          preferredXPx = None,
          multiCursorVerticalStates = Nil
        ),
        annotations = buffer.annotations.copy(
          documentComments =
            adjustDocumentComments(buffer.annotations.documentComments, buffer.document.content, updatedContent, edits)
        ),
        richText = buffer.richText.copy(richTextDocument = updatedRichTextDocument)
      )
      (baseBuffer, edits)

  private def applyMergedDeletionEdits(
    buffer: Buffer,
    initialOffsets: List[Int],
    edits: List[MultiCursorEdit]
  ): (Buffer, List[MultiCursorEdit]) =
    if edits.isEmpty then (buffer, Nil)
    else
      val mergedRanges = mergeOverlappingDeletionRanges(edits.map(edit => (edit.start, edit.end)))
      val updatedContent = mergedRanges
        .sortBy { case (start, end) => (-start, -end) }
        .foldLeft(buffer.document.content) {
          case (content, (start, end)) =>
            content.delete(start, end)
        }
      val mergedEdits = mergedRanges.zipWithIndex.map {
        case ((start, end), index) =>
          MultiCursorEdit(index, start, end, "")
      }
      val finalOffsets = initialOffsets.map(offset => remapOffsetAfterDeletions(offset, mergedRanges))
      val finalCursors = finalOffsets
        .map(offset => offsetToCursorPosition(updatedContent, offset))
        .distinct
      val primaryCursor = finalCursors.headOption.getOrElse(CursorPosition(0, 0))
      val baseBuffer = buffer.copy(
        document = buffer.document.copy(content = updatedContent, isDirty = true, isNewEmpty = false),
        editing = buffer.editing.copy(
          cursors = finalCursors,
          selection = None,
          selections = Nil,
          preferredColumn = Some(primaryCursor.column),
          preferredXPx = None
        ),
        annotations = buffer.annotations.copy(
          documentComments = adjustDocumentComments(
            buffer.annotations.documentComments,
            buffer.document.content,
            updatedContent,
            mergedEdits
          )
        )
      )
      (baseBuffer, mergedEdits)

  private def mergeOverlappingDeletionRanges(
    ranges: List[(Int, Int)]
  ): List[(Int, Int)] =
    ranges
      .sortBy { case (start, end) => (start, end) }
      .foldLeft(List.empty[(Int, Int)]) {
        case (Nil, range) => range :: Nil
        case ((currentStart, currentEnd) :: rest, (nextStart, nextEnd)) =>
          if nextStart < currentEnd then (currentStart, math.max(currentEnd, nextEnd)) :: rest
          else (nextStart, nextEnd) :: (currentStart, currentEnd) :: rest
      }
      .reverse

  private def remapOffsetAfterDeletions(
    offset: Int,
    deletions: List[(Int, Int)]
  ): Int =
    deletions.foldLeft(offset) {
      case (currentOffset, (start, end)) =>
        if currentOffset < start then currentOffset
        else if currentOffset > end then currentOffset - (end - start)
        else start
    }

  private def adjustDocumentComments(
    comments: List[DocumentComment],
    initialContent: Rope,
    updatedContent: Rope,
    edits: List[MultiCursorEdit]
  ): List[DocumentComment] =
    if comments.isEmpty || edits.isEmpty then comments
    else
      val sortedEdits = edits.sortBy(edit => (edit.start, edit.end))
      comments.map { comment =>
        val startOffset              = initialContent.lineColumnToOffset(comment.start.line, comment.start.column)
        val endOffset                = initialContent.lineColumnToOffset(comment.end.line, comment.end.column)
        val nextStart                = remapCommentStart(startOffset, sortedEdits)
        val nextEnd                  = remapCommentEnd(endOffset, sortedEdits).max(nextStart)
        val (startLine, startColumn) = updatedContent.offsetToLineColumn(nextStart)
        val (endLine, endColumn)     = updatedContent.offsetToLineColumn(nextEnd)

        DocumentComment(
          CursorPosition(startLine, startColumn),
          CursorPosition(endLine, endColumn),
          comment.text
        )
      }

  private def remapCommentStart(offset: Int, edits: List[MultiCursorEdit]): Int =
    remapEditBoundary(offset, edits, insertionAtBoundaryMoves = true)

  private def remapCommentEnd(offset: Int, edits: List[MultiCursorEdit]): Int =
    remapEditBoundary(offset, edits, insertionAtBoundaryMoves = false)

  private def remapEditBoundary(
    offset: Int,
    edits: List[MultiCursorEdit],
    insertionAtBoundaryMoves: Boolean
  ): Int =
    val (_, remappedOffset) = edits.foldLeft((0, offset)) {
      case ((deltaSoFar, currentOffset), edit) =>
        val removedLength     = edit.end - edit.start
        val insertedLength    = edit.insertedText.length
        val editDelta         = insertedLength - removedLength
        val remappedEditStart = edit.start + deltaSoFar
        val isInsertion       = edit.start == edit.end
        val nextOffset =
          if isInsertion then
            if offset > edit.start || (offset == edit.start && insertionAtBoundaryMoves) then
              currentOffset + insertedLength
            else currentOffset
          else if offset < edit.start then currentOffset
          else if offset > edit.end || (offset == edit.end && !insertionAtBoundaryMoves) then currentOffset + editDelta
          else remappedEditStart

        (deltaSoFar + editDelta, nextOffset)
    }

    remappedOffset.max(0)

  private def applyMultiSelectionReplacement(
    buffer: Buffer,
    insertedText: String
  ): (Buffer, List[MultiCursorEdit]) =
    val ranges  = mergedActiveSelectionRanges(buffer, buffer.document.content)
    val offsets = ranges.map(_._1)
    val edits = ranges.zipWithIndex.map {
      case ((start, end), index) =>
        MultiCursorEdit(index, start, end, insertedText)
    }
    applyTrackedEdits(buffer, offsets, edits)

  private def deleteSelectedRanges(
    buffer: Buffer
  ): (Buffer, List[MultiCursorEdit]) =
    val ranges  = mergedActiveSelectionRanges(buffer, buffer.document.content)
    val offsets = ranges.map(_._1)
    val edits = ranges.zipWithIndex.map {
      case ((start, end), index) =>
        MultiCursorEdit(index, start, end, "")
    }
    applyTrackedEdits(buffer, offsets, edits)

  private def applyMultiCursorNavigation(
    buffer: Buffer
  )(move: CursorPosition => CursorPosition): Buffer =
    val finalCursors = buffer.editing.cursors
      .map(move)
      .distinct
      .sortBy(cursor => (cursor.line, cursor.column))
    val primaryCursor = finalCursors.headOption.getOrElse(CursorPosition(0, 0))
    val baseBuffer = buffer.copy(
      editing = buffer.editing.copy(
        cursors = finalCursors,
        selection = None,
        selections = Nil,
        preferredColumn = Some(primaryCursor.column),
        preferredXPx = None,
        multiCursorVerticalStates = Nil
      )
    )
    baseBuffer

  private def applyMultiCursorVerticalNavigation(
    buffer: Buffer,
    currentState: AppState,
    geometry: EditorGeometry,
    direction: Int
  ): Buffer =
    val cursorStates = multiCursorVerticalStates(buffer, geometry)
    val movedStates = cursorStates.map { cursorState =>
      cursorState.copy(
        cursor = moveMultiCursorVertical(
          cursorState.cursor,
          buffer,
          currentState,
          geometry,
          cursorState.preferredColumn,
          cursorState.preferredXPx,
          direction
        )
      )
    }
    val sortedStates = movedStates.sortBy(cursorState =>
      (cursorState.cursor.line, cursorState.cursor.column, cursorState.preferredColumn, cursorState.preferredXPx)
    )
    val visibleCursors = sortedStates
      .map(_.cursor)
      .distinct
    val primaryCursor = visibleCursors.headOption.getOrElse(CursorPosition(0, 0))
    val baseBuffer = buffer.copy(
      editing = buffer.editing.copy(
        cursors = visibleCursors,
        selection = None,
        selections = Nil,
        preferredColumn = Some(primaryCursor.column),
        preferredXPx = None,
        multiCursorVerticalStates = sortedStates.map(cursorState =>
          VerticalCursorState(cursorState.cursor, cursorState.preferredColumn, cursorState.preferredXPx)
        )
      )
    )
    baseBuffer

  private def applyMultiCursorPageNavigation(
    buffer: Buffer,
    direction: Int
  ): Buffer =
    val totalLines = countLines(buffer.document.content)
    val visLines   = buffer.viewport.visibleLines
    val finalCursors = buffer.editing.cursors
      .map { cursor =>
        val targetLine =
          if direction < 0 then math.max(0, cursor.line - visLines)
          else math.min(cursor.line + visLines, totalLines - 1)
        cursor.copy(line = targetLine, column = 0)
      }
      .distinct
      .sortBy(cursor => (cursor.line, cursor.column))
    val primaryCursor = finalCursors.headOption.getOrElse(CursorPosition(0, 0))
    val newTopLine =
      if direction < 0 then math.max(0, buffer.viewport.topLine - visLines)
      else math.min(buffer.viewport.topLine + visLines, math.max(0, totalLines - visLines))
    buffer.copy(
      editing = buffer.editing.copy(
        cursors = finalCursors,
        selection = None,
        selections = Nil,
        preferredColumn = Some(primaryCursor.column),
        preferredXPx = None,
        multiCursorVerticalStates = Nil
      ),
      viewport = buffer.viewport.copy(topLine = newTopLine, topVisualLine = 0)
    )

  private def multiCursorEntries(buffer: Buffer): List[CursorEntry] =
    buffer.editing.cursors.distinct
      .map(cursor => CursorEntry(cursor, lineColumnToOffset(buffer.document.content, cursor.line, cursor.column)))
      .sortBy(_.offset)

  private def distinctCursorLines(buffer: Buffer): List[Int] =
    buffer.editing.cursors.distinct
      .sortBy(cursor => (cursor.line, cursor.column))
      .map(_.line)
      .distinct

  private def selectionLines(buffer: Buffer): List[Int] =
    activeSelections(buffer)
      .flatMap(selection => selection.start.line to selection.end.line)
      .distinct
      .sorted

  private def deleteOffsetRange(
    buffer: Buffer,
    startOffset: Int,
    endOffset: Int,
    cursorOffset: Int
  ): (Buffer, MultiCursorEdit) =
    val newContent = buffer.document.content.delete(startOffset, endOffset)
    val newCursor  = offsetToCursorPosition(newContent, cursorOffset)
    val baseBuffer = buffer.copy(
      document = buffer.document.copy(content = newContent, isDirty = true, isNewEmpty = false),
      editing = buffer.editing.copy(
        cursors = newCursor :: buffer.editing.cursors.tail,
        selection = None,
        selections = Nil,
        preferredColumn = Some(newCursor.column),
        preferredXPx = None
      ),
      annotations = buffer.annotations.copy(
        documentComments = adjustDocumentComments(
          buffer.annotations.documentComments,
          buffer.document.content,
          newContent,
          List(MultiCursorEdit(0, startOffset, endOffset, ""))
        )
      ),
      richText = buffer.richText.copy(richTextDocument = richTextDocumentAfterEdit(buffer, startOffset, endOffset, ""))
    )
    (baseBuffer, MultiCursorEdit(0, startOffset, endOffset, ""))

  private def offsetToCursorPosition(content: Rope, offset: Int): CursorPosition =
    val (line, column) = content.offsetToLineColumn(offset)
    CursorPosition(line, column)

  private def previousWordBoundary(content: Rope, offset: Int): Int =
    TextEditing.previousWordBoundary(RopeCharacterSource(content), offset)

  private def nextWordBoundary(content: Rope, offset: Int): Int =
    TextEditing.nextWordBoundary(RopeCharacterSource(content), offset)

  private def previousGraphemeBoundary(content: Rope, offset: Int): Int =
    TextEditing.previousGraphemeBoundary(RopeCharacterSource(content), offset)

  private def nextGraphemeBoundary(content: Rope, offset: Int): Int =
    TextEditing.nextGraphemeBoundary(RopeCharacterSource(content), offset)

  private def graphemeBoundaryBeforeOrAt(content: Rope, offset: Int): Int =
    TextEditing.graphemeBoundaryBeforeOrAt(RopeCharacterSource(content), offset)

  private def graphemeBoundaryAfterOrAt(content: Rope, offset: Int): Int =
    TextEditing.graphemeBoundaryAfterOrAt(RopeCharacterSource(content), offset)

  private def backwardGraphemeDeletionRange(content: Rope, offset: Int): Option[(Int, Int)] =
    val beforeOrAt = graphemeBoundaryBeforeOrAt(content, offset)
    val afterOrAt  = graphemeBoundaryAfterOrAt(content, offset)
    if beforeOrAt < offset && offset < afterOrAt then Some(beforeOrAt -> afterOrAt)
    else
      val start = previousGraphemeBoundary(content, offset)
      Option.when(start < offset)(start -> offset)

  private def forwardGraphemeDeletionRange(content: Rope, offset: Int): Option[(Int, Int)] =
    val beforeOrAt = graphemeBoundaryBeforeOrAt(content, offset)
    val afterOrAt  = graphemeBoundaryAfterOrAt(content, offset)
    if beforeOrAt < offset && offset < afterOrAt then Some(beforeOrAt -> afterOrAt)
    else
      val end = nextGraphemeBoundary(content, offset)
      Option.when(offset < end)(offset -> end)

  final private case class RopeCharacterSource(content: Rope) extends TextEditing.CharacterSource:
    override def length: Int =
      content.weight

    override def charAt(index: Int): Char =
      content.index(index).getOrElse('\u0000')

  private def isWholeGraphemeMatch(content: Rope, offset: Int, length: Int): Boolean =
    TextEditing.isWholeGraphemeRange(RopeCharacterSource(content), offset, offset + length)

  private def findModalForBuffer(buffer: Buffer): Modal =
    buffer.findState match
      case Some(FindState(query, results, currentIndex)) if query.nonEmpty =>
        val resultSet = FindResultSet.normalized(query, results, currentIndex)
        Modal.Find(resultSet.query, resultSet.results, resultSet.currentIndex)
      case _ =>
        Modal.Find("", Nil, 0)

  private def moveCursorLeft(cursor: CursorPosition, content: Rope): CursorPosition =
    val offset = lineColumnToOffset(content, cursor.line, cursor.column)
    val target = previousGraphemeBoundary(content, offset)
    if target < offset then offsetToCursorPosition(content, target)
    else cursor

  private def moveCursorRight(cursor: CursorPosition, content: Rope): CursorPosition =
    val offset = lineColumnToOffset(content, cursor.line, cursor.column)
    val target = nextGraphemeBoundary(content, offset)
    if offset < target then offsetToCursorPosition(content, target)
    else cursor

  private def moveMultiCursorVertical(
    cursor: CursorPosition,
    buffer: Buffer,
    currentState: AppState,
    geometry: EditorGeometry,
    preferredColumn: Int,
    preferredXPx: Float,
    direction: Int
  ): CursorPosition =
    measuredVerticalMoveBySnapshot(
      currentState.config.wordWrapEnabled,
      cursor,
      geometry.navigation,
      preferredXPx,
      direction
    )
      .getOrElse(
        fallbackVerticalMove(cursor, buffer, geometry, currentState.config.wordWrapEnabled, preferredColumn, direction)
      )

  private def multiCursorVerticalStates(
    buffer: Buffer,
    geometry: EditorGeometry
  ): List[MultiCursorVerticalState] =
    val visibleCursors = buffer.editing.cursors.distinct
      .sortBy(cursor => (cursor.line, cursor.column))
    val storedVisibleCursors = buffer.editing.multiCursorVerticalStates
      .map(_.cursor)
      .distinct
      .sortBy(cursor => (cursor.line, cursor.column))

    if buffer.editing.multiCursorVerticalStates.nonEmpty && storedVisibleCursors == visibleCursors then
      buffer.editing.multiCursorVerticalStates.map(cursorState =>
        MultiCursorVerticalState(cursorState.cursor, cursorState.preferredColumn, cursorState.preferredXPx)
      )
    else
      visibleCursors.map(cursor =>
        MultiCursorVerticalState(cursor, cursor.column, measuredCursorXPxFrom(geometry, cursor))
      )

  private def moveUpVisualLine(
    cursor: CursorPosition,
    rope: Rope,
    panelWidth: Int,
    preferredColumn: Int
  ): CursorPosition =
    if cursor.line == 0 && cursor.column < panelWidth then cursor.copy(column = 0)
    else
      val currentVisualLineInBuffer = cursor.column / panelWidth

      if currentVisualLineInBuffer > 0 then
        val newColumn = currentVisualLineInBuffer * panelWidth - panelWidth + (preferredColumn % panelWidth)
        cursor.copy(column = math.max(0, newColumn))
      else if cursor.line > 0 then
        val prevLineContent = rope.getLine(cursor.line - 1).getOrElse("")
        if prevLineContent.length <= panelWidth then
          val newColumn = math.min(preferredColumn, prevLineContent.length)
          cursor.copy(line = cursor.line - 1, column = newColumn)
        else
          val lastVisualLineInPrev   = (prevLineContent.length - 1) / panelWidth
          val baseColumnInLastVisual = lastVisualLineInPrev * panelWidth
          val newColumn = math.min(baseColumnInLastVisual + (preferredColumn % panelWidth), prevLineContent.length)
          cursor.copy(line = cursor.line - 1, column = newColumn)
      else cursor

  private def moveDownVisualLine(
    cursor: CursorPosition,
    rope: Rope,
    panelWidth: Int,
    preferredColumn: Int
  ): CursorPosition =
    val currentLineContent        = rope.getLine(cursor.line).getOrElse("")
    val currentVisualLineInBuffer = cursor.column / panelWidth
    val totalVisualLinesInCurrent = math.max(1, (currentLineContent.length + panelWidth - 1) / panelWidth)

    if currentVisualLineInBuffer < totalVisualLinesInCurrent - 1 then
      val newColumn = currentVisualLineInBuffer * panelWidth + panelWidth + (preferredColumn % panelWidth)
      cursor.copy(column = math.min(newColumn, currentLineContent.length))
    else if cursor.line < rope.lineCount - 1 then
      val nextLineContent      = rope.getLine(cursor.line + 1).getOrElse("")
      val targetColumnInVisual = preferredColumn % panelWidth
      val newColumn            = math.min(targetColumnInVisual, nextLineContent.length)
      cursor.copy(line = cursor.line + 1, column = newColumn)
    else cursor

  private def fallbackVerticalMove(
    cursor: CursorPosition,
    buffer: Buffer,
    geometry: EditorGeometry,
    wordWrapEnabled: Boolean,
    preferredColumn: Int,
    direction: Int
  ): CursorPosition =
    if wordWrapEnabled then
      if direction < 0 then
        moveUpVisualLine(cursor, buffer.document.content, geometry.panelWidthColumns, preferredColumn)
      else moveDownVisualLine(cursor, buffer.document.content, geometry.panelWidthColumns, preferredColumn)
    else if direction < 0 then moveUpLogicalLine(cursor, buffer.document.content, preferredColumn)
    else moveDownLogicalLine(cursor, buffer.document.content, preferredColumn)

  private def moveUpLogicalLine(cursor: CursorPosition, rope: Rope, preferredColumn: Int): CursorPosition =
    if cursor.line <= 0 then cursor
    else
      val previousLineLength = rope.getLine(cursor.line - 1).map(_.length).getOrElse(0)
      cursor.copy(line = cursor.line - 1, column = math.min(preferredColumn, previousLineLength))

  private def moveDownLogicalLine(cursor: CursorPosition, rope: Rope, preferredColumn: Int): CursorPosition =
    if cursor.line >= rope.lineCount - 1 then cursor
    else
      val nextLineLength = rope.getLine(cursor.line + 1).map(_.length).getOrElse(0)
      cursor.copy(line = cursor.line + 1, column = math.min(preferredColumn, nextLineLength))

  /** Returns the buffer with content/comments/etc. applied but animations untouched, plus the delta of newly animated
    * cells for the caller to hand to the presentation layer (`#1001`) -- this function never had access to the buffer's
    * *current* animations beyond merging into them, so it never needed to read them; only the merge itself moves to the
    * caller.
    */
  private def addInsertionAnimations(
    buffer: Buffer,
    state: AppState,
    edits: List[MultiCursorEdit]
  ): (Buffer, Map[CharacterKey, AnimatedCell]) =
    val sortedEdits = edits
      .filter(_.insertedText.nonEmpty)
      .sortBy(edit => (edit.start, edit.end))

    if sortedEdits.isEmpty then (buffer, Map.empty)
    else
      val insertedCells = insertedTransitionCells(buffer.document.content, sortedEdits, state)
      if insertedCells.isEmpty then (buffer, Map.empty)
      else
        val plan = ElementTransitionPlanner.plan(
          ElementTransitionRequest(TransitionScope.EditorInsertion),
          state.config.editorInsertionTransitionSettings
        )
        if plan.kind == TransitionKind.Disabled then (buffer, Map.empty)
        else if plan.kind == TransitionKind.Fade then
          state.config.scaledCharacterAnimation match
            case Some(animConfig) =>
              if insertedCells.size == 1 then
                val (key, cell) = insertedCells.head
                val delta = Map(
                  key -> AnimatedCell.parametricForeground(cell.char, cell.startColor, cell.endColor, animConfig.steps)
                )
                (buffer, delta)
              else
                val staggeredCells = insertedCells
                  .groupBy { case (key, _) => key.line }
                  .valuesIterator
                  .flatMap(lineCells =>
                    FlowAnimationBuilder.build(
                      cells = lineCells,
                      direction = FlowDirection.ByColumn,
                      sweep = SweepDirection.Forward,
                      steps = animConfig.steps,
                      staggerFrames = 1
                    )
                  )
                  .toMap
                (buffer, staggeredCells)
            case None =>
              (buffer, Map.empty)
        else
          val animationState = ElementTransitionLowerer.lower(
            plan,
            ElementTransitionCells(content = insertedCells),
            tickRateMs = 16
          )
          (buffer, animationState.animations)

  private def insertedTransitionCells(
    content: Rope,
    edits: List[MultiCursorEdit],
    state: AppState,
    maxAnimatedCells: Int = com.serenity.state.manager.VisibleBufferAnimationCells.DefaultMaxAnimatedCells
  ): Map[CharacterKey, CellAnimation] =
    edits.foldLeft(Map.empty[CharacterKey, CellAnimation]) { (cells, edit) =>
      val remainingBudget = maxAnimatedCells - cells.size
      if remainingBudget <= 0 then cells
      else
        val finalStartOffset = remapEditBoundary(edit.start, edits, insertionAtBoundaryMoves = false)
        cells ++ insertedCellsFromText(
          content,
          finalStartOffset,
          edit.insertedText.take(remainingBudget),
          state.theme.backgroundColor,
          state.theme.foregroundColor
        )
    }

  private def insertedCellsFromText(
    content: Rope,
    startOffset: Int,
    insertedText: String,
    startColor: java.awt.Color,
    endColor: java.awt.Color
  ): Map[CharacterKey, CellAnimation] =
    insertedText
      .foldLeft((Map.empty[CharacterKey, CellAnimation], startOffset)) {
        case ((cells, offset), char) if char == '\n' =>
          (cells, offset + 1)
        case ((cells, offset), char) =>
          val (line, column) = content.offsetToLineColumn(offset)
          (
            cells + (CharacterKey(column, line) -> CellAnimation(char, startColor, endColor)),
            offset + 1
          )
      }
      ._1

  /** Where a movement key lands, plus the column and measured x-offset a later vertical move should resume from.
    * Movement and shift-movement compute this identically and differ only in what they do with it.
    */
  final private case class CursorTarget(cursor: CursorPosition, preferredColumn: Int, preferredXPx: Option[Float])

  private def horizontalTarget(landed: CursorPosition): CursorTarget =
    CursorTarget(landed, landed.column, preferredXPx = None)

  private def leftTarget(buffer: Buffer, from: CursorPosition): CursorTarget =
    horizontalTarget(moveCursorLeft(from, buffer.document.content))

  private def rightTarget(buffer: Buffer, from: CursorPosition): CursorTarget =
    horizontalTarget(moveCursorRight(from, buffer.document.content))

  private def wordBoundaryFrom(buffer: Buffer, from: CursorPosition, boundary: (Rope, Int) => Int): CursorPosition =
    val offset = lineColumnToOffset(buffer.document.content, from.line, from.column)
    offsetToCursorPosition(buffer.document.content, boundary(buffer.document.content, offset))

  private def verticalTarget(currentState: AppState, geometry: EditorGeometry, direction: Int)(
    buffer: Buffer,
    from: CursorPosition
  ): CursorTarget =
    val preferredColumn = buffer.editing.preferredColumn.getOrElse(from.column)
    val preferredXPx    = buffer.editing.preferredXPx.getOrElse(measuredCursorXPxFrom(geometry, from))
    val landed =
      measuredVerticalMoveBySnapshot(
        currentState.config.wordWrapEnabled,
        from,
        geometry.navigation,
        preferredXPx,
        direction
      )
        .getOrElse(
          fallbackVerticalMove(from, buffer, geometry, currentState.config.wordWrapEnabled, preferredColumn, direction)
        )

    CursorTarget(landed, preferredColumn, Some(preferredXPx))

  /** `from` is passed rather than derived: arrow keys resume from the selection focus so a right-arrow off a selection
    * lands past its end, while Home and End resume from the head cursor.
    */
  private def reduceMovement(buffer: Buffer, from: CursorPosition, currentState: AppState)(
    target: (Buffer, CursorPosition) => CursorTarget
  ): ReducerResult =
    ReducerResult.fromTransition(
      currentState,
      Focused.modifyBufferWithId(buffer.id) { current =>
        val landed = target(current, from)
        current.copy(
          editing = current.editing.copy(
            cursors = landed.cursor :: current.editing.cursors.tail,
            selection = None,
            preferredColumn = Some(landed.preferredColumn),
            preferredXPx = landed.preferredXPx
          )
        )
      }
    )

  private def reduceSelectionExtension(buffer: Buffer, cursor: CursorPosition, currentState: AppState)(
    target: (Buffer, CursorPosition) => CursorTarget
  ): ReducerResult =
    ReducerResult.fromTransition(
      currentState,
      Focused.modifyBufferWithId(buffer.id) { current =>
        val landed = target(current, cursor)
        extendSelection(current, cursor, landed.cursor, Some(landed.preferredColumn), landed.preferredXPx)
      }
    )

  private def selectionFocusOrCursor(buffer: Buffer, cursor: CursorPosition): CursorPosition =
    buffer.primarySelection.map(_.focus).getOrElse(cursor)

  private def extendSelection(
    buffer: Buffer,
    anchor: CursorPosition,
    focus: CursorPosition,
    preferredColumn: Option[Int],
    preferredXPx: Option[Float]
  ): Buffer =
    val selectionAnchor = buffer.primarySelection.map(_.anchor).getOrElse(anchor)
    buffer.copy(
      editing = buffer.editing.copy(
        cursors = focus :: buffer.editing.cursors.tail,
        selection = Some(Selection(selectionAnchor, focus)),
        selections = Nil,
        preferredColumn = preferredColumn,
        preferredXPx = preferredXPx
      )
    )

  private def measuredCursorXPxFrom(geometry: EditorGeometry, cursor: CursorPosition): Float =
    geometry.navigation.xPxForCursor(cursor).getOrElse(cursor.column.toFloat * geometry.charWidthPx.toFloat)

  private def measuredVerticalMoveBySnapshot(
    wordWrapEnabled: Boolean,
    cursor: CursorPosition,
    navigation: NavigationGeometry,
    preferredXPx: Float,
    direction: Int
  ): Option[CursorPosition] =
    Option.when(wordWrapEnabled)(navigation.moveVertical(cursor, direction, preferredXPx)).flatten

  private def replaceSelectionOrInsert(
    buffer: Buffer,
    cursor: CursorPosition,
    insertedText: String
  ): (Buffer, MultiCursorEdit) =
    val (baseContent, insertionStart, startOffset, endOffset) = buffer.primarySelection match
      case Some(selection) =>
        val startOffset = selectionStartOffset(selection, buffer.document.content)
        val endOffset   = selectionEndOffset(selection, buffer.document.content)
        (
          buffer.document.content.delete(startOffset, endOffset),
          offsetToCursorPosition(buffer.document.content, startOffset),
          startOffset,
          endOffset
        )
      case None =>
        val startOffset =
          graphemeBoundaryAfterOrAt(
            buffer.document.content,
            lineColumnToOffset(buffer.document.content, cursor.line, cursor.column)
          )
        (
          buffer.document.content,
          offsetToCursorPosition(buffer.document.content, startOffset),
          startOffset,
          startOffset
        )

    val newContent      = baseContent.insert(startOffset, insertedText)
    val newCursor       = cursorAfterInsertion(insertionStart, insertedText)
    val replacementEdit = MultiCursorEdit(0, startOffset, endOffset, insertedText)

    (
      buffer.copy(
        document = buffer.document.copy(content = newContent, isDirty = true, isNewEmpty = false),
        editing = buffer.editing.copy(
          cursors = newCursor :: buffer.editing.cursors.tail,
          selection = None,
          selections = Nil,
          preferredColumn = Some(newCursor.column),
          preferredXPx = None
        ),
        annotations = buffer.annotations.copy(
          documentComments = adjustDocumentComments(
            buffer.annotations.documentComments,
            buffer.document.content,
            newContent,
            List(replacementEdit)
          )
        ),
        richText = buffer.richText.copy(
          richTextDocument = richTextDocumentAfterEdit(buffer, startOffset, endOffset, insertedText)
        )
      ),
      replacementEdit
    )

  private def deleteSelectedRange(
    buffer: Buffer,
    selection: Selection
  ): (Buffer, MultiCursorEdit) =
    val startOffset = selectionStartOffset(selection, buffer.document.content)
    val endOffset   = selectionEndOffset(selection, buffer.document.content)
    val newContent  = buffer.document.content.delete(startOffset, endOffset)
    val newCursor   = offsetToCursorPosition(newContent, startOffset)
    val baseBuffer = buffer.copy(
      document = buffer.document.copy(content = newContent, isDirty = true, isNewEmpty = false),
      editing = buffer.editing.copy(
        cursors = newCursor :: buffer.editing.cursors.tail,
        selection = None,
        selections = Nil,
        preferredColumn = Some(newCursor.column),
        preferredXPx = None
      ),
      annotations = buffer.annotations.copy(
        documentComments = adjustDocumentComments(
          buffer.annotations.documentComments,
          buffer.document.content,
          newContent,
          List(MultiCursorEdit(0, startOffset, endOffset, ""))
        )
      ),
      richText = buffer.richText.copy(richTextDocument = richTextDocumentAfterEdit(buffer, startOffset, endOffset, ""))
    )
    (baseBuffer, MultiCursorEdit(0, startOffset, endOffset, ""))

  private def activeSelections(buffer: Buffer): List[Selection] =
    buffer.allSelections.distinct
      .sortBy(selection =>
        (
          selection.start.line,
          selection.start.column,
          selection.end.line,
          selection.end.column
        )
      )

  private def selectedTexts(buffer: Buffer): List[String] =
    mergedActiveSelectionRanges(buffer, buffer.document.content).map {
      case (start, end) =>
        buffer.document.content.sliceString(start, end)
    }

  private def mergedActiveSelectionRanges(buffer: Buffer, content: Rope): List[(Int, Int)] =
    val ranges = activeSelections(buffer)
      .map(selection => (selectionStartOffset(selection, content), selectionEndOffset(selection, content)))
      .filter { case (start, end) => start < end }
    mergeOverlappingSelectionRanges(ranges)

  private def mergeOverlappingSelectionRanges(ranges: List[(Int, Int)]): List[(Int, Int)] =
    ranges
      .sortBy { case (start, end) => (start, end) }
      .foldLeft(List.empty[(Int, Int)]) {
        case (Nil, range) => range :: Nil
        case ((currentStart, currentEnd) :: rest, (nextStart, nextEnd)) =>
          if nextStart < currentEnd then (currentStart, math.max(currentEnd, nextEnd)) :: rest
          else (nextStart, nextEnd) :: (currentStart, currentEnd) :: rest
      }
      .reverse

  private def selectionStartOffset(selection: Selection, content: Rope): Int =
    graphemeBoundaryBeforeOrAt(content, lineColumnToOffset(content, selection.start.line, selection.start.column))

  private def selectionEndOffset(selection: Selection, content: Rope): Int =
    graphemeBoundaryAfterOrAt(content, lineColumnToOffset(content, selection.end.line, selection.end.column))

  private def richTextDocumentAfterEdit(
    buffer: Buffer,
    startOffset: Int,
    endOffset: Int,
    insertedText: String
  ): Option[RichTextDocument] =
    buffer.richText.richTextDocument.flatMap { document =>
      Option.when(document.matchesPlainText(buffer.document.content.collect())) {
        val updatedDocument = document
          .replaceRange(
            RichTextRange(
              richTextPositionForOffset(buffer.document.content, startOffset),
              richTextPositionForOffset(buffer.document.content, endOffset)
            ),
            insertedText
          )
          .normalized
        buffer.richText.insertionRichTextStyle
          .filter(_ => insertedText.nonEmpty)
          .map { style =>
            val updatedContent =
              buffer.document.content.delete(startOffset, endOffset).insert(startOffset, insertedText)
            updatedDocument
              .updateInlineStyle(
                RichTextRange(
                  richTextPositionForOffset(updatedContent, startOffset),
                  richTextPositionForOffset(updatedContent, startOffset + insertedText.length)
                )
              )(_ => style)
              .normalized
          }
          .getOrElse(updatedDocument)
      }
    }

  private def richTextPositionForOffset(content: Rope, offset: Int): RichTextPosition =
    val (line, column) = content.offsetToLineColumn(offset)
    RichTextPosition(line, column)

  private def collapseSelectionsToFocus(buffer: Buffer): Buffer =
    val cursors = activeSelections(buffer)
      .map(_.focus)
      .distinct
      .sortBy(cursor => (cursor.line, cursor.column))
    val primaryCursor = cursors.headOption.getOrElse(CursorPosition(0, 0))
    val baseBuffer = buffer.copy(
      editing = buffer.editing.copy(
        cursors = cursors,
        selection = None,
        selections = Nil,
        preferredColumn = Some(primaryCursor.column),
        preferredXPx = None
      )
    )
    baseBuffer

  private def cursorAfterInsertion(start: CursorPosition, insertedText: String): CursorPosition =
    val lines = insertedText.split("\n", -1)
    if lines.length == 1 then start.copy(column = start.column + insertedText.length)
    else CursorPosition(start.line + lines.length - 1, lines.last.length)
