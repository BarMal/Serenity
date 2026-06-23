package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Rope
import com.serenity.state.models.*
import com.serenity.text.TextEditing
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot}

object EditorEventReducer:
  private val TabInsertion = "    "

  def reducer(paneId: PaneId)(using balance: com.serenity.rope.Balance): Reducer[EditorEvent] =
    Reducer.instance((event, state) => reduce(event, paneId, state))

  def reduce(
    event: EditorEvent,
    paneId: PaneId,
    currentState: AppState
  )(using balance: com.serenity.rope.Balance): ReducerResult =
    currentState.layout.editorPanes.get(paneId) match
      case Some(pane) => reduceForPane(event, paneId, pane, currentState)
      case None       => ReducerResult.noEffects(currentState)

  private def reduceForPane(
    event: EditorEvent,
    paneId: PaneId,
    pane: EditorPane,
    currentState: AppState
  )(using balance: com.serenity.rope.Balance): ReducerResult =
    event match
      case ScrollDown(lines) =>
        pane.bufferId.flatMap(currentState.buffers.get) match
          case Some(buffer) =>
            val totalLines    = countLines(buffer.content)
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
  )(using balance: com.serenity.rope.Balance): ReducerResult =
    val result =
      if isExtendSelectionEvent(event) then
        reduceSingleCursorTextEvent(event, clearInFlightMultiCursorVerticalState(buffer), paneId, currentState)
      else if buffer.allSelections.nonEmpty then reduceMultiSelectionTextEvent(event, buffer, paneId, currentState)
      else if preservesInFlightMultiCursorVerticalState(event, buffer) then
        reduceMultiCursorTextEvent(event, buffer, paneId, currentState)
      else if buffer.cursors.size > 1 then reduceMultiCursorTextEvent(event, buffer, paneId, currentState)
      else reduceSingleCursorTextEvent(event, clearInFlightMultiCursorVerticalState(buffer), paneId, currentState)

    if refreshesFindResults(event) then result.copy(state = refreshFindState(result.state, buffer.id))
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
      case ExtendSelectionLeft | ExtendSelectionRight | ExtendSelectionUp | ExtendSelectionDown => true
      case _                                                                                    => false

  private def refreshFindState(state: AppState, bufferId: BufferId): AppState =
    state.buffers.get(bufferId) match
      case Some(buffer) =>
        buffer.findState match
          case Some(FindState(query, _, currentIndex)) if query.nonEmpty =>
            val resultSet = FindResultSet.normalized(query, findMatches(buffer, query).map(toFindResult), currentIndex)
            val updatedFindState =
              Option.when(resultSet.results.nonEmpty)(FindState.fromResultSet(resultSet))
            state.copy(buffers = state.buffers + (bufferId -> buffer.copy(findState = updatedFindState)))
          case _ =>
            state
      case _ =>
        state

  private def preservesInFlightMultiCursorVerticalState(event: TextEntryEvent, buffer: Buffer): Boolean =
    buffer.multiCursorVerticalStates.size > 1 && (event == MoveUp || event == MoveDown)

  private def clearInFlightMultiCursorVerticalState(buffer: Buffer): Buffer =
    if buffer.multiCursorVerticalStates.isEmpty then buffer
    else buffer.copy(multiCursorVerticalStates = Nil)

  private def reduceSingleCursorTextEvent(
    event: TextEntryEvent,
    buffer: Buffer,
    paneId: PaneId,
    currentState: AppState
  )(using balance: com.serenity.rope.Balance): ReducerResult =
    buffer.cursors.headOption match
      case Some(cursor) =>
        event match
          case InsertChar(char) =>
            val replacedBuffer  = replaceSelectionOrInsert(buffer, cursor, char.toString)
            val newCursor       = replacedBuffer.cursors.headOption.getOrElse(cursor)
            val updatedViewport = adjustViewportForCursor(replacedBuffer, currentState, newCursor)
            val updatedBuffer = addCharacterAnimationToBuffer(
              replacedBuffer.copy(viewport = updatedViewport),
              currentState,
              char,
              cursor.line,
              cursor.column
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case TabKey =>
            val replacedBuffer  = replaceSelectionOrInsert(buffer, cursor, TabInsertion)
            val newCursor       = replacedBuffer.cursors.headOption.getOrElse(cursor)
            val updatedViewport = adjustViewportForCursor(replacedBuffer, currentState, newCursor)
            ReducerResult.noEffects(
              currentState.copy(buffers =
                currentState.buffers + (buffer.id -> replacedBuffer.copy(viewport = updatedViewport))
              )
            )

          case ReverseTabKey =>
            ReducerResult.noEffects(
              updateBufferInState(currentState, applyLineUnindent(buffer, currentState, List(cursor.line)))
            )

          case DeleteBackward =>
            buffer.primarySelection match
              case Some(selection) =>
                val updatedBuffer = deleteSelectedRange(buffer, selection, currentState)
                ReducerResult.noEffects(
                  currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer))
                )
              case None =>
                val text   = buffer.content.collect()
                val offset = lineColumnToOffset(text, cursor.line, cursor.column)
                if offset > 0 then
                  val newContent = buffer.content.delete(offset - 1, offset)
                  val newCursor =
                    if cursor.column > 0 then cursor.copy(column = cursor.column - 1)
                    else if cursor.line > 0 then
                      val prevLineEnd = findLineEnd(text, cursor.line - 1)
                      cursor.copy(line = cursor.line - 1, column = prevLineEnd)
                    else cursor
                  val updatedViewport = adjustViewportForCursor(buffer, currentState, newCursor)
                  val updatedBuffer = buffer.copy(
                    content = newContent,
                    isDirty = true,
                    isNewEmpty = false,
                    cursors = newCursor :: buffer.cursors.tail,
                    selection = None,
                    preferredColumn = Some(newCursor.column),
                    preferredXPx = None,
                    viewport = updatedViewport,
                    documentComments = adjustDocumentComments(
                      buffer.documentComments,
                      text,
                      newContent,
                      List(MultiCursorEdit(0, offset - 1, offset, ""))
                    )
                  )
                  ReducerResult.noEffects(
                    currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer))
                  )
                else ReducerResult.noEffects(currentState)

          case DeleteForward =>
            buffer.primarySelection match
              case Some(selection) =>
                val updatedBuffer = deleteSelectedRange(buffer, selection, currentState)
                ReducerResult.noEffects(
                  currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer))
                )
              case None =>
                val text   = buffer.content.collect()
                val offset = lineColumnToOffset(text, cursor.line, cursor.column)
                if offset < buffer.content.weight then
                  val newContent = buffer.content.delete(offset, offset + 1)
                  val updatedBuffer = buffer.copy(
                    content = newContent,
                    isDirty = true,
                    isNewEmpty = false,
                    preferredColumn = Some(cursor.column),
                    preferredXPx = None,
                    documentComments = adjustDocumentComments(
                      buffer.documentComments,
                      text,
                      newContent,
                      List(MultiCursorEdit(0, offset, offset + 1, ""))
                    )
                  )
                  ReducerResult.noEffects(
                    currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer))
                  )
                else ReducerResult.noEffects(currentState)

          case DeleteWordBackward =>
            buffer.primarySelection match
              case Some(selection) =>
                val updatedBuffer = deleteSelectedRange(buffer, selection, currentState)
                ReducerResult.noEffects(
                  currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer))
                )
              case None =>
                val text   = buffer.content.collect()
                val offset = lineColumnToOffset(text, cursor.line, cursor.column)
                val start  = TextEditing.previousWordBoundary(text, offset)
                if start < offset then
                  val updatedBuffer = deleteOffsetRange(buffer, currentState, start, offset, start)
                  ReducerResult.noEffects(
                    currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer))
                  )
                else ReducerResult.noEffects(currentState)

          case DeleteWordForward =>
            buffer.primarySelection match
              case Some(selection) =>
                val updatedBuffer = deleteSelectedRange(buffer, selection, currentState)
                ReducerResult.noEffects(
                  currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer))
                )
              case None =>
                val text   = buffer.content.collect()
                val offset = lineColumnToOffset(text, cursor.line, cursor.column)
                val end    = TextEditing.nextWordBoundary(text, offset)
                if offset < end then
                  val updatedBuffer = deleteOffsetRange(buffer, currentState, offset, end, offset)
                  ReducerResult.noEffects(
                    currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer))
                  )
                else ReducerResult.noEffects(currentState)

          case MoveLeft =>
            val movementStart   = selectionFocusOrCursor(buffer, cursor)
            val newCursor       = moveCursorLeft(movementStart, buffer.content)
            val updatedViewport = adjustViewportForCursor(buffer, currentState, newCursor)
            val updatedBuffer = buffer.copy(
              cursors = newCursor :: buffer.cursors.tail,
              selection = None,
              preferredColumn = Some(newCursor.column),
              preferredXPx = None,
              viewport = updatedViewport
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case MoveRight =>
            val movementStart   = selectionFocusOrCursor(buffer, cursor)
            val newCursor       = moveCursorRight(movementStart, buffer.content)
            val updatedViewport = adjustViewportForCursor(buffer, currentState, newCursor)
            val updatedBuffer = buffer.copy(
              cursors = newCursor :: buffer.cursors.tail,
              selection = None,
              preferredColumn = Some(newCursor.column),
              preferredXPx = None,
              viewport = updatedViewport
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case MoveUp =>
            val movementStart         = selectionFocusOrCursor(buffer, cursor)
            val preferredColumn       = buffer.preferredColumn.getOrElse(movementStart.column)
            val (navSnap, navMetrics) = navigationSnapshot(buffer, currentState)
            val preferredXPx = buffer.preferredXPx.getOrElse(measuredCursorXPxFrom(navSnap, navMetrics, movementStart))
            val newCursor = measuredVerticalMoveBySnapshot(buffer, movementStart, navSnap, preferredXPx, direction = -1)
              .getOrElse(
                moveUpVisualLine(movementStart, buffer.content, effectivePanelWidth(currentState), preferredColumn)
              )
            val updatedViewport = adjustViewportForCursor(buffer, currentState, newCursor)
            val updatedBuffer = buffer.copy(
              cursors = newCursor :: buffer.cursors.tail,
              selection = None,
              preferredColumn = Some(preferredColumn),
              preferredXPx = Some(preferredXPx),
              viewport = updatedViewport
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case MoveDown =>
            val movementStart         = selectionFocusOrCursor(buffer, cursor)
            val preferredColumn       = buffer.preferredColumn.getOrElse(movementStart.column)
            val (navSnap, navMetrics) = navigationSnapshot(buffer, currentState)
            val preferredXPx = buffer.preferredXPx.getOrElse(measuredCursorXPxFrom(navSnap, navMetrics, movementStart))
            val newCursor = measuredVerticalMoveBySnapshot(buffer, movementStart, navSnap, preferredXPx, direction = 1)
              .getOrElse(
                moveDownVisualLine(movementStart, buffer.content, effectivePanelWidth(currentState), preferredColumn)
              )
            val updatedViewport = adjustViewportForCursor(buffer, currentState, newCursor)
            val updatedBuffer = buffer.copy(
              cursors = newCursor :: buffer.cursors.tail,
              selection = None,
              preferredColumn = Some(preferredColumn),
              preferredXPx = Some(preferredXPx),
              viewport = updatedViewport
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case ExtendSelectionLeft =>
            val newCursor = moveCursorLeft(cursor, buffer.content)
            val updatedBuffer =
              extendSelection(buffer, currentState, cursor, newCursor, preferredColumn = Some(newCursor.column))
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case ExtendSelectionRight =>
            val newCursor = moveCursorRight(cursor, buffer.content)
            val updatedBuffer =
              extendSelection(buffer, currentState, cursor, newCursor, preferredColumn = Some(newCursor.column))
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case ExtendSelectionUp =>
            val preferredColumn       = buffer.preferredColumn.getOrElse(cursor.column)
            val (navSnap, navMetrics) = navigationSnapshot(buffer, currentState)
            val preferredXPx = buffer.preferredXPx.getOrElse(measuredCursorXPxFrom(navSnap, navMetrics, cursor))
            val newCursor = measuredVerticalMoveBySnapshot(buffer, cursor, navSnap, preferredXPx, direction = -1)
              .getOrElse(moveUpVisualLine(cursor, buffer.content, effectivePanelWidth(currentState), preferredColumn))
            val updatedBuffer = extendSelection(
              buffer,
              currentState,
              cursor,
              newCursor,
              preferredColumn = Some(preferredColumn),
              preferredXPx = Some(preferredXPx)
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case ExtendSelectionDown =>
            val preferredColumn       = buffer.preferredColumn.getOrElse(cursor.column)
            val (navSnap, navMetrics) = navigationSnapshot(buffer, currentState)
            val preferredXPx = buffer.preferredXPx.getOrElse(measuredCursorXPxFrom(navSnap, navMetrics, cursor))
            val newCursor = measuredVerticalMoveBySnapshot(buffer, cursor, navSnap, preferredXPx, direction = 1)
              .getOrElse(moveDownVisualLine(cursor, buffer.content, effectivePanelWidth(currentState), preferredColumn))
            val updatedBuffer = extendSelection(
              buffer,
              currentState,
              cursor,
              newCursor,
              preferredColumn = Some(preferredColumn),
              preferredXPx = Some(preferredXPx)
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case NewLine | Enter =>
            val updatedBuffer             = replaceSelectionOrInsert(buffer, cursor, "\n")
            val newCursor                 = updatedBuffer.cursors.headOption.getOrElse(cursor)
            val updatedViewport           = adjustViewportForCursor(updatedBuffer, currentState, newCursor)
            val updatedBufferWithViewport = updatedBuffer.copy(viewport = updatedViewport)
            ReducerResult.noEffects(
              currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBufferWithViewport))
            )

          case MoveToStart =>
            val newCursor       = cursor.copy(column = 0)
            val updatedViewport = adjustViewportForCursor(buffer, currentState, newCursor)
            val updatedBuffer = buffer.copy(
              cursors = newCursor :: buffer.cursors.tail,
              selection = None,
              preferredColumn = Some(newCursor.column),
              preferredXPx = None,
              viewport = updatedViewport
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case MoveToEnd =>
            val lineEnd         = findLineEnd(buffer.content, cursor.line)
            val newCursor       = cursor.copy(column = lineEnd)
            val updatedViewport = adjustViewportForCursor(buffer, currentState, newCursor)
            val updatedBuffer = buffer.copy(
              cursors = newCursor :: buffer.cursors.tail,
              selection = None,
              preferredColumn = Some(newCursor.column),
              preferredXPx = None,
              viewport = updatedViewport
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case SelectAll =>
            val text        = buffer.content.collect()
            val lastLine    = math.max(0, countLines(text) - 1)
            val lastColumn  = findLineEnd(text, lastLine)
            val startCursor = CursorPosition(0, 0)
            val endCursor   = CursorPosition(lastLine, lastColumn)
            val updatedBuffer = buffer.copy(
              cursors = List(endCursor),
              selection = Some(Selection(startCursor, endCursor)),
              preferredColumn = Some(endCursor.column),
              preferredXPx = None,
              viewport = adjustViewportForCursor(buffer, currentState, endCursor)
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case PageDown =>
            val totalLines    = countLines(buffer.content)
            val visLines      = buffer.viewport.visibleLines
            val newTopLine    = math.min(buffer.viewport.topLine + visLines, math.max(0, totalLines - visLines))
            val newCursorLine = math.min(cursor.line + visLines, totalLines - 1)
            val newCursor     = cursor.copy(line = newCursorLine, column = 0)
            val newViewport   = buffer.viewport.copy(topLine = newTopLine, topVisualLine = 0)
            val updatedBuffer = buffer.copy(
              cursors = newCursor :: buffer.cursors.tail,
              preferredColumn = Some(newCursor.column),
              preferredXPx = None,
              viewport = newViewport
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case PageUp =>
            val visLines      = buffer.viewport.visibleLines
            val newTopLine    = math.max(0, buffer.viewport.topLine - visLines)
            val newCursorLine = math.max(0, cursor.line - visLines)
            val newCursor     = cursor.copy(line = newCursorLine, column = 0)
            val newViewport   = buffer.viewport.copy(topLine = newTopLine, topVisualLine = 0)
            val updatedBuffer = buffer.copy(
              cursors = newCursor :: buffer.cursors.tail,
              preferredColumn = Some(newCursor.column),
              preferredXPx = None,
              viewport = newViewport
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case MoveToEndOfFile =>
            val totalLines  = countLines(buffer.content)
            val lastLine    = totalLines - 1
            val lastLineEnd = findLineEnd(buffer.content, lastLine)
            val newCursor   = CursorPosition(lastLine, lastLineEnd)
            val newTopLine  = math.max(0, lastLine - buffer.viewport.visibleLines + 1)
            val newViewport = buffer.viewport.copy(topLine = newTopLine, topVisualLine = 0)
            val updatedBuffer = buffer.copy(
              cursors = newCursor :: buffer.cursors.tail,
              preferredColumn = Some(newCursor.column),
              preferredXPx = None,
              viewport = newViewport
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case MoveToStartOfFile =>
            val newCursor       = CursorPosition(0, 0)
            val updatedViewport = adjustViewportForCursor(buffer, currentState, newCursor)
            val updatedBuffer = buffer.copy(
              cursors = newCursor :: buffer.cursors.tail,
              selection = None,
              preferredColumn = Some(newCursor.column),
              preferredXPx = None,
              viewport = updatedViewport
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case OpenGotoLine =>
            ModalStateReducer.show(Modal.GotoLine(""), currentState)

          case OpenFind =>
            ModalStateReducer.show(findModalForBuffer(buffer), currentState)

          case FindNext =>
            buffer.findState match
              case Some(FindState(query, storedResults, currentIndex)) if storedResults.nonEmpty =>
                val resultSet =
                  FindResultSet.normalized(query, findMatches(buffer, query).map(toFindResult), currentIndex + 1)
                if resultSet.results.isEmpty then
                  val updatedBuffer = buffer.copy(findState = None)
                  ReducerResult.noEffects(
                    currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer))
                  )
                else
                  val selected = resultSet.results(resultSet.currentIndex)
                  val target   = CursorPosition(selected.line, selected.column)
                  val baseBuffer = buffer.copy(
                    cursors = List(target),
                    selection = None,
                    selections = Nil,
                    preferredColumn = Some(target.column),
                    preferredXPx = None,
                    findState = Some(FindState.fromResultSet(resultSet))
                  )
                  val updatedBuffer = baseBuffer.copy(
                    viewport = adjustViewportForCursor(baseBuffer, currentState, target)
                  )
                  ReducerResult.noEffects(
                    currentState.copy(
                      buffers = currentState.buffers + (buffer.id -> updatedBuffer)
                    )
                  )
              case _ =>
                ReducerResult.noEffects(currentState)

          case Copy if buffer.primarySelection.isDefined =>
            val selection = buffer.primarySelection.get
            ReducerResult.noEffects(
              currentState.copy(clipboard = Some(selectedText(buffer, selection)))
            )

          case Cut if buffer.primarySelection.isDefined =>
            val selection     = buffer.primarySelection.get
            val updatedBuffer = deleteSelectedRange(buffer, selection, currentState)
            ReducerResult.noEffects(
              currentState.copy(
                buffers = currentState.buffers + (buffer.id -> updatedBuffer),
                clipboard = Some(selectedText(buffer, selection))
              )
            )

          case Copy =>
            val text          = buffer.content.collect()
            val clipboardText = getLine(text, cursor.line).getOrElse("")
            ReducerResult.noEffects(currentState.copy(clipboard = Some(clipboardText)))

          case Cut =>
            val text      = buffer.content.collect()
            val lineText  = getLine(text, cursor.line).getOrElse("")
            val lineStart = lineColumnToOffset(text, cursor.line, 0)
            val lineEnd   = lineColumnToOffset(text, cursor.line, lineText.length)
            val lineCount = countLines(text)
            val (deleteStart, deleteEnd, newCursor) =
              if cursor.line == 0 && lineCount == 1 then (0, lineEnd, CursorPosition(0, 0))
              else if cursor.line < lineCount - 1 then
                // delete including the trailing newline
                (lineStart, lineEnd + 1, CursorPosition(cursor.line, 0))
              else
                // last line — delete preceding newline
                (lineStart - 1, lineEnd, CursorPosition(cursor.line - 1, 0))
            val newContent = buffer.content.delete(deleteStart, deleteEnd)
            val updatedBuffer = buffer.copy(
              content = newContent,
              isDirty = true,
              isNewEmpty = false,
              cursors = newCursor :: buffer.cursors.tail,
              preferredColumn = Some(newCursor.column),
              preferredXPx = None,
              viewport = adjustViewportForCursor(buffer, currentState, newCursor),
              documentComments = adjustDocumentComments(
                buffer.documentComments,
                text,
                newContent,
                List(MultiCursorEdit(0, deleteStart, deleteEnd, ""))
              )
            )
            ReducerResult.noEffects(
              currentState.copy(
                buffers = currentState.buffers + (buffer.id -> updatedBuffer),
                clipboard = Some(lineText)
              )
            )

          case Paste =>
            currentState.clipboard match
              case None                       => ReducerResult.noEffects(currentState)
              case Some(text) if text.isEmpty => ReducerResult.noEffects(currentState)
              case Some(text) =>
                val replacedBuffer = replaceSelectionOrInsert(buffer, cursor, text)
                val newCursor      = replacedBuffer.cursors.headOption.getOrElse(cursor)
                val updatedBuffer = buffer.copy(
                  content = replacedBuffer.content,
                  isDirty = replacedBuffer.isDirty,
                  isNewEmpty = replacedBuffer.isNewEmpty,
                  cursors = replacedBuffer.cursors,
                  selection = replacedBuffer.selection,
                  preferredColumn = Some(newCursor.column),
                  preferredXPx = None,
                  viewport = adjustViewportForCursor(buffer, currentState, newCursor)
                )
                ReducerResult.noEffects(
                  currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer))
                )

          case _ =>
            ReducerResult.noEffects(currentState)

      case None =>
        val defaultCursor = CursorPosition(0, 0)
        val updatedPane   = currentState.layout.editorPanes(paneId).copy(cursors = List(defaultCursor))
        ReducerResult.noEffects(
          currentState.copy(
            layout = currentState.layout.copy(
              editorPanes = currentState.layout.editorPanes + (paneId -> updatedPane)
            )
          )
        )

  private def reduceMultiSelectionTextEvent(
    event: TextEntryEvent,
    buffer: Buffer,
    paneId: PaneId,
    currentState: AppState
  )(using balance: com.serenity.rope.Balance): ReducerResult =
    event match
      case InsertChar(char) =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiSelectionReplacement(buffer, currentState, char.toString))
        )
      case TabKey =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyLineIndent(buffer, currentState, selectionLines(buffer)))
        )
      case NewLine | Enter =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiSelectionReplacement(buffer, currentState, "\n"))
        )
      case Paste =>
        currentState.clipboard.filter(_.nonEmpty) match
          case Some(text) =>
            ReducerResult.noEffects(
              updateBufferInState(currentState, applyMultiSelectionReplacement(buffer, currentState, text))
            )
          case None =>
            ReducerResult.noEffects(currentState)
      case ReverseTabKey =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyLineUnindent(buffer, currentState, selectionLines(buffer)))
        )
      case DeleteBackward | DeleteForward | DeleteWordBackward | DeleteWordForward =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, deleteSelectedRanges(buffer, currentState))
        )
      case Copy =>
        ReducerResult.noEffects(currentState.copy(clipboard = Some(selectedTexts(buffer).mkString("\n"))))
      case Cut =>
        ReducerResult.noEffects(
          currentState.copy(
            buffers = currentState.buffers + (buffer.id -> deleteSelectedRanges(buffer, currentState)),
            clipboard = Some(selectedTexts(buffer).mkString("\n"))
          )
        )
      case MoveLeft | MoveRight | MoveUp | MoveDown | MoveToStart | MoveToEnd | PageUp | PageDown | MoveToStartOfFile |
          MoveToEndOfFile =>
        reduceMultiCursorTextEvent(event, collapseSelectionsToFocus(buffer, currentState), paneId, currentState)
      case SelectAll | OpenGotoLine | OpenFind | FindNext | Escape =>
        reduceGlobalTextEvent(event, buffer, paneId, currentState)
      case _ =>
        reduceGlobalTextEvent(event, buffer, paneId, currentState)

  private def reduceMultiCursorTextEvent(
    event: TextEntryEvent,
    buffer: Buffer,
    paneId: PaneId,
    currentState: AppState
  )(using balance: com.serenity.rope.Balance): ReducerResult =
    event match
      case InsertChar(char) =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiCursorInsertion(buffer, currentState, char.toString))
        )
      case TabKey =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiCursorInsertion(buffer, currentState, TabInsertion))
        )
      case NewLine | Enter =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiCursorInsertion(buffer, currentState, "\n"))
        )
      case Paste =>
        currentState.clipboard.filter(_.nonEmpty) match
          case Some(text) =>
            ReducerResult.noEffects(
              updateBufferInState(currentState, applyMultiCursorInsertion(buffer, currentState, text))
            )
          case None =>
            ReducerResult.noEffects(currentState)
      case DeleteBackward =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiCursorDeletion(buffer, currentState, backward = true))
        )
      case DeleteForward =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiCursorDeletion(buffer, currentState, backward = false))
        )
      case DeleteWordBackward =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiCursorWordDeletion(buffer, currentState, backward = true))
        )
      case DeleteWordForward =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiCursorWordDeletion(buffer, currentState, backward = false))
        )
      case ReverseTabKey =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyLineUnindent(buffer, currentState, distinctCursorLines(buffer)))
        )
      case MoveLeft =>
        ReducerResult.noEffects(
          updateBufferInState(
            currentState,
            applyMultiCursorNavigation(buffer, currentState)(cursor => moveCursorLeft(cursor, buffer.content))
          )
        )
      case MoveRight =>
        ReducerResult.noEffects(
          updateBufferInState(
            currentState,
            applyMultiCursorNavigation(buffer, currentState)(cursor => moveCursorRight(cursor, buffer.content))
          )
        )
      case MoveToStart =>
        ReducerResult.noEffects(
          updateBufferInState(
            currentState,
            applyMultiCursorNavigation(buffer, currentState)(cursor => cursor.copy(column = 0))
          )
        )
      case MoveToEnd =>
        ReducerResult.noEffects(
          updateBufferInState(
            currentState,
            applyMultiCursorNavigation(buffer, currentState)(cursor =>
              cursor.copy(column = findLineEnd(buffer.content, cursor.line))
            )
          )
        )
      case MoveUp =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiCursorVerticalNavigation(buffer, currentState, direction = -1))
        )
      case MoveDown =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiCursorVerticalNavigation(buffer, currentState, direction = 1))
        )
      case PageUp =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiCursorPageNavigation(buffer, direction = -1))
        )
      case PageDown =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiCursorPageNavigation(buffer, direction = 1))
        )
      case MoveToStartOfFile =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiCursorNavigation(buffer, currentState)(_ => CursorPosition(0, 0)))
        )
      case MoveToEndOfFile =>
        val totalLines  = countLines(buffer.content)
        val lastLine    = totalLines - 1
        val lastLineEnd = findLineEnd(buffer.content, lastLine)
        val target      = CursorPosition(lastLine, lastLineEnd)
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiCursorNavigation(buffer, currentState)(_ => target))
        )
      case Copy =>
        val text = buffer.content.collect()
        val clipboardText = distinctCursorLines(buffer)
          .map(line => getLine(text, line).getOrElse(""))
          .mkString("\n")
        ReducerResult.noEffects(currentState.copy(clipboard = Some(clipboardText)))
      case Cut =>
        val text          = buffer.content.collect()
        val targetLines   = distinctCursorLines(buffer)
        val clipboardText = targetLines.map(line => getLine(text, line).getOrElse("")).mkString("\n")
        val updatedBuffer = applyMultiCursorLineCut(buffer, currentState, targetLines, text)
        ReducerResult.noEffects(
          currentState.copy(
            buffers = currentState.buffers + (buffer.id -> updatedBuffer),
            clipboard = Some(clipboardText)
          )
        )
      case SelectAll | OpenGotoLine | OpenFind | FindNext | Escape =>
        reduceGlobalTextEvent(event, buffer, paneId, currentState)
      case _ =>
        reduceSingleCursorTextEvent(event, buffer, paneId, currentState)

  private def handleEventWithoutBuffer(
    event: TextEntryEvent,
    paneId: PaneId,
    pane: EditorPane,
    currentState: AppState
  )(using balance: com.serenity.rope.Balance): ReducerResult =
    event match
      case InsertChar(char) =>
        val bufferId    = currentState.nextBufferId
        val buffer      = Buffer.fromString(bufferId, char.toString).copy(isDirty = true, isNewEmpty = false)
        val newCursor   = CursorPosition(0, 1)
        val updatedPane = pane.copy(bufferId = Some(bufferId), cursors = List(newCursor))
        val bufferWithAnimation = addCharacterAnimationToBuffer(
          buffer,
          currentState,
          char,
          0,
          0
        )
        ReducerResult.noEffects(
          currentState.copy(
            buffers = currentState.buffers + (bufferId -> bufferWithAnimation),
            layout = currentState.layout.copy(
              editorPanes = currentState.layout.editorPanes + (paneId -> updatedPane)
            ),
            nextBufferId = BufferId(bufferId.value + 1)
          )
        )

      case TabKey =>
        TabInsertion.foldLeft(ReducerResult.noEffects(currentState)) { (result, char) =>
          handleEventWithoutBuffer(InsertChar(char), paneId, pane, result.state)
        }

      case _ =>
        ReducerResult.noEffects(currentState)

  private def reduceGlobalTextEvent(
    event: TextEntryEvent,
    buffer: Buffer,
    paneId: PaneId,
    currentState: AppState
  )(using balance: com.serenity.rope.Balance): ReducerResult =
    reduceSingleCursorTextEvent(
      event,
      clearInFlightMultiCursorVerticalState(buffer.copy(selection = buffer.primarySelection, selections = Nil)),
      paneId,
      currentState
    )

  private[reducers] def lineColumnToOffset(rope: Rope, line: Int, column: Int): Int =
    rope.lineColumnToOffset(line, column)

  private[reducers] def lineColumnToOffset(content: String, line: Int, column: Int): Int =
    val targetLine = math.max(0, line)
    val targetCol  = math.max(0, column)

    @annotation.tailrec
    def findLineStart(currentLine: Int, lineStart: Int): Int =
      if currentLine >= targetLine then lineStart
      else
        val nextLineBreak = content.indexOf('\n', lineStart)
        if nextLineBreak == -1 then content.length
        else findLineStart(currentLine + 1, nextLineBreak + 1)

    val lineStart = findLineStart(currentLine = 0, lineStart = 0)
    val lineEnd = content.indexOf('\n', lineStart) match
      case -1    => content.length
      case index => index
    math.min(lineStart + targetCol, lineEnd)

  private def findLineEnd(content: String, line: Int): Int =
    getLine(content, line).fold(0)(_.length)

  private def findLineEnd(content: Rope, line: Int): Int =
    content.getLine(line).fold(0)(_.length)

  private def countLines(rope: Rope): Int =
    rope.lineCount

  private def countLines(content: String): Int =
    if content.isEmpty then 1 else content.count(_ == '\n') + 1

  private def getLine(content: String, lineIndex: Int): Option[String] =
    if lineIndex < 0 then None
    else
      @annotation.tailrec
      def findLineStart(currentLine: Int, lineStart: Int): Option[Int] =
        if currentLine == lineIndex then Some(lineStart)
        else
          val nextLineBreak = content.indexOf('\n', lineStart)
          if nextLineBreak == -1 then None
          else findLineStart(currentLine + 1, nextLineBreak + 1)

      findLineStart(currentLine = 0, lineStart = 0).map { lineStart =>
        val lineEnd = content.indexOf('\n', lineStart) match
          case -1    => content.length
          case index => index
        content.slice(lineStart, lineEnd)
      }

  private case class CursorEntry(cursor: CursorPosition, offset: Int)
  private case class MultiCursorEdit(ownerIndex: Int, start: Int, end: Int, insertedText: String)
  private case class MultiCursorVerticalState(cursor: CursorPosition, preferredColumn: Int, preferredXPx: Float)

  private def applyMultiCursorInsertion(
    buffer: Buffer,
    currentState: AppState,
    insertedText: String
  ): Buffer =
    val entries = multiCursorEntries(buffer)
    val edits = entries.zipWithIndex.map {
      case (entry, index) =>
        MultiCursorEdit(index, entry.offset, entry.offset, insertedText)
    }
    applyTrackedEdits(buffer, currentState, entries.map(_.offset), edits)

  private def applyMultiCursorDeletion(
    buffer: Buffer,
    currentState: AppState,
    backward: Boolean
  ): Buffer =
    val entries = multiCursorEntries(buffer)
    val edits = entries.zipWithIndex.flatMap {
      case (entry, index) =>
        if backward then Option.when(entry.offset > 0)(MultiCursorEdit(index, entry.offset - 1, entry.offset, ""))
        else
          Option.when(entry.offset < buffer.content.weight)(MultiCursorEdit(index, entry.offset, entry.offset + 1, ""))
    }
    applyTrackedEdits(buffer, currentState, entries.map(_.offset), edits)

  private def applyMultiCursorWordDeletion(
    buffer: Buffer,
    currentState: AppState,
    backward: Boolean
  ): Buffer =
    val text    = buffer.content.collect()
    val entries = multiCursorEntries(buffer, text)
    val edits = entries.zipWithIndex.flatMap {
      case (entry, index) =>
        if backward then
          val start = TextEditing.previousWordBoundary(text, entry.offset)
          Option.when(start < entry.offset)(MultiCursorEdit(index, start, entry.offset, ""))
        else
          val end = TextEditing.nextWordBoundary(text, entry.offset)
          Option.when(entry.offset < end)(MultiCursorEdit(index, entry.offset, end, ""))
    }
    applyMergedDeletionEdits(buffer, currentState, entries.map(_.offset), edits)

  private def applyLineIndent(
    buffer: Buffer,
    currentState: AppState,
    targetLines: List[Int]
  )(using balance: com.serenity.rope.Balance): Buffer =
    val text      = buffer.content.collect()
    val lines     = text.split("\n", -1).toList
    val targetSet = targetLines.filter(line => line >= 0 && line < lines.length).toSet

    if targetSet.isEmpty then buffer
    else
      val edits = targetSet.toList.sorted.zipWithIndex.map {
        case (line, index) =>
          val offset = lineColumnToOffset(text, line, 0)
          MultiCursorEdit(index, offset, offset, TabInsertion)
      }
      val updatedText = lines.zipWithIndex
        .map {
          case (lineText, lineIndex) =>
            if targetSet.contains(lineIndex) then TabInsertion + lineText else lineText
        }
        .mkString("\n")
      val updatedContent = Rope(updatedText)
      val finalCursors = buffer.cursors.map { cursor =>
        if targetSet.contains(cursor.line) then cursor.copy(column = cursor.column + TabInsertion.length)
        else cursor
      }.distinct
      val primaryCursor = finalCursors.headOption.getOrElse(CursorPosition(0, 0))
      val baseBuffer = buffer.copy(
        content = updatedContent,
        isDirty = true,
        isNewEmpty = false,
        cursors = finalCursors,
        selection = None,
        selections = Nil,
        preferredColumn = Some(primaryCursor.column),
        preferredXPx = None,
        multiCursorVerticalStates = Nil,
        documentComments = adjustDocumentComments(buffer.documentComments, text, updatedText, edits)
      )
      baseBuffer.copy(viewport = adjustViewportForCursor(baseBuffer, currentState, primaryCursor))

  private def applyLineUnindent(
    buffer: Buffer,
    currentState: AppState,
    targetLines: List[Int]
  )(using balance: com.serenity.rope.Balance): Buffer =
    val text      = buffer.content.collect()
    val lines     = text.split("\n", -1).toList
    val targetSet = targetLines.filter(line => line >= 0 && line < lines.length).toSet
    val updatedLines = lines.zipWithIndex.map {
      case (lineText, lineIndex) =>
        if targetSet.contains(lineIndex) then
          val (updatedLine, removed) = unindentLine(lineText)
          (updatedLine, lineIndex -> removed)
        else (lineText, lineIndex -> 0)
    }
    val updatedText = updatedLines.map(_._1).mkString("\n")
    val removals    = updatedLines.map(_._2).toMap

    if removals.values.forall(_ == 0) then buffer
    else
      val edits = removals.toList.sortBy(_._1).zipWithIndex.collect {
        case ((line, removed), index) if removed > 0 =>
          val start = lineColumnToOffset(text, line, 0)
          MultiCursorEdit(index, start, start + removed, "")
      }
      val updatedContent = Rope(updatedText)
      val finalCursors = buffer.cursors
        .map(cursor => cursor.copy(column = math.max(0, cursor.column - removals.getOrElse(cursor.line, 0))))
        .distinct
      val primaryCursor = finalCursors.headOption.getOrElse(CursorPosition(0, 0))
      val baseBuffer = buffer.copy(
        content = updatedContent,
        isDirty = true,
        isNewEmpty = false,
        cursors = finalCursors,
        selection = None,
        selections = Nil,
        preferredColumn = Some(primaryCursor.column),
        preferredXPx = None,
        multiCursorVerticalStates = Nil,
        documentComments = adjustDocumentComments(buffer.documentComments, text, updatedText, edits)
      )
      baseBuffer.copy(viewport = adjustViewportForCursor(baseBuffer, currentState, primaryCursor))

  private def unindentLine(lineText: String): (String, Int) =
    if lineText.startsWith("\t") then (lineText.drop(1), 1)
    else
      val spacesToRemove = lineText.take(TabInsertion.length).takeWhile(_ == ' ').length
      if spacesToRemove == 0 then (lineText, 0)
      else (lineText.drop(spacesToRemove), spacesToRemove)

  private def applyMultiCursorLineCut(
    buffer: Buffer,
    currentState: AppState,
    targetLines: List[Int],
    text: String
  ): Buffer =
    if targetLines.isEmpty then buffer
    else
      val totalLines = countLines(text)
      val lineEdits = targetLines.distinct.sorted.map { line =>
        val lineText  = getLine(text, line).getOrElse("")
        val lineStart = lineColumnToOffset(text, line, 0)
        val lineEnd   = lineColumnToOffset(text, line, lineText.length)
        val (deleteStart, deleteEnd) =
          if line == 0 && totalLines == 1 then (0, lineEnd)
          else if line < totalLines - 1 then (lineStart, lineEnd + 1)
          else (math.max(0, lineStart - 1), lineEnd)
        (line, deleteStart, deleteEnd)
      }
      val updatedContent = lineEdits
        .sortBy { case (_, start, end) => (-start, -end) }
        .foldLeft(buffer.content) {
          case (content, (_, start, end)) =>
            content.delete(start, end)
        }
      val edits = lineEdits.zipWithIndex.map {
        case ((_, start, end), index) =>
          MultiCursorEdit(index, start, end, "")
      }
      val updatedText  = updatedContent.collect()
      val maxFinalLine = math.max(0, countLines(updatedText) - 1)
      val finalCursors = targetLines.distinct.sorted.map { line =>
        val deletedBefore = targetLines.count(_ < line)
        val targetLine =
          if totalLines == 1 then 0
          else if line < totalLines - 1 then line - deletedBefore
          else line - targetLines.count(_ <= line)
        val clampedLine = math.max(0, math.min(targetLine, maxFinalLine))
        offsetToCursorPosition(updatedText, updatedContent.weight, lineColumnToOffset(updatedText, clampedLine, 0))
      }.distinct
      val primaryCursor = finalCursors.headOption.getOrElse(CursorPosition(0, 0))
      val baseBuffer = buffer.copy(
        content = updatedContent,
        isDirty = true,
        isNewEmpty = false,
        cursors = finalCursors,
        selection = None,
        selections = Nil,
        preferredColumn = Some(primaryCursor.column),
        preferredXPx = None,
        multiCursorVerticalStates = Nil,
        documentComments = adjustDocumentComments(buffer.documentComments, text, updatedText, edits)
      )
      baseBuffer.copy(viewport = adjustViewportForCursor(baseBuffer, currentState, primaryCursor))

  private def applyTrackedEdits(
    buffer: Buffer,
    currentState: AppState,
    initialOffsets: List[Int],
    edits: List[MultiCursorEdit]
  ): Buffer =
    if edits.isEmpty then buffer
    else
      val initialText    = Option.when(buffer.documentComments.nonEmpty)(buffer.content.collect())
      val trackedOffsets = initialOffsets.toArray
      val sortedEdits    = edits.sortBy(edit => (-edit.start, -edit.end))
      val updatedContent = sortedEdits.foldLeft(buffer.content) { (content, edit) =>
        val deleted = content.delete(edit.start, edit.end)
        deleted.insert(edit.start, edit.insertedText)
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
      val updatedText = updatedContent.collect()
      val finalCursors = finalOffsets.toList
        .map(offset => offsetToCursorPosition(updatedText, updatedContent.weight, offset))
        .distinct
      val primaryCursor = finalCursors.headOption.getOrElse(CursorPosition(0, 0))
      val baseBuffer = buffer.copy(
        content = updatedContent,
        isDirty = true,
        isNewEmpty = false,
        cursors = finalCursors,
        selection = None,
        selections = Nil,
        preferredColumn = Some(primaryCursor.column),
        preferredXPx = None,
        multiCursorVerticalStates = Nil,
        documentComments = initialText.fold(buffer.documentComments) { text =>
          adjustDocumentComments(buffer.documentComments, text, updatedText, edits)
        }
      )
      baseBuffer.copy(viewport = adjustViewportForCursor(baseBuffer, currentState, primaryCursor))

  private def applyMergedDeletionEdits(
    buffer: Buffer,
    currentState: AppState,
    initialOffsets: List[Int],
    edits: List[MultiCursorEdit]
  ): Buffer =
    if edits.isEmpty then buffer
    else
      val initialText  = Option.when(buffer.documentComments.nonEmpty)(buffer.content.collect())
      val mergedRanges = mergeOverlappingDeletionRanges(edits.map(edit => (edit.start, edit.end)))
      val updatedContent = mergedRanges
        .sortBy { case (start, end) => (-start, -end) }
        .foldLeft(buffer.content) {
          case (content, (start, end)) =>
            content.delete(start, end)
        }
      val mergedEdits = mergedRanges.zipWithIndex.map {
        case ((start, end), index) =>
          MultiCursorEdit(index, start, end, "")
      }
      val finalOffsets = initialOffsets.map(offset => remapOffsetAfterDeletions(offset, mergedRanges))
      val updatedText  = updatedContent.collect()
      val finalCursors = finalOffsets
        .map(offset => offsetToCursorPosition(updatedText, updatedContent.weight, offset))
        .distinct
      val primaryCursor = finalCursors.headOption.getOrElse(CursorPosition(0, 0))
      val baseBuffer = buffer.copy(
        content = updatedContent,
        isDirty = true,
        isNewEmpty = false,
        cursors = finalCursors,
        selection = None,
        selections = Nil,
        preferredColumn = Some(primaryCursor.column),
        preferredXPx = None,
        documentComments = initialText.fold(buffer.documentComments) { text =>
          adjustDocumentComments(buffer.documentComments, text, updatedText, mergedEdits)
        }
      )
      baseBuffer.copy(viewport = adjustViewportForCursor(baseBuffer, currentState, primaryCursor))

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
    initialText: String,
    updatedContent: Rope,
    edits: List[MultiCursorEdit]
  ): List[DocumentComment] =
    if comments.isEmpty || edits.isEmpty then comments
    else adjustDocumentComments(comments, initialText, updatedContent.collect(), edits)

  private def adjustDocumentComments(
    comments: List[DocumentComment],
    initialText: String,
    updatedText: String,
    edits: List[MultiCursorEdit]
  ): List[DocumentComment] =
    if comments.isEmpty || edits.isEmpty then comments
    else
      val sortedEdits = edits.sortBy(edit => (edit.start, edit.end))
      comments.map { comment =>
        val startOffset = lineColumnToOffset(initialText, comment.start.line, comment.start.column)
        val endOffset   = lineColumnToOffset(initialText, comment.end.line, comment.end.column)
        val nextStart   = remapCommentStart(startOffset, sortedEdits)
        val nextEnd     = remapCommentEnd(endOffset, sortedEdits).max(nextStart)

        DocumentComment(
          offsetToCursorPosition(updatedText, updatedText.length, nextStart),
          offsetToCursorPosition(updatedText, updatedText.length, nextEnd),
          comment.text
        )
      }

  private def remapCommentStart(offset: Int, edits: List[MultiCursorEdit]): Int =
    remapCommentBoundary(offset, edits, insertionAtBoundaryMoves = true)

  private def remapCommentEnd(offset: Int, edits: List[MultiCursorEdit]): Int =
    remapCommentBoundary(offset, edits, insertionAtBoundaryMoves = false)

  private def remapCommentBoundary(
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
    currentState: AppState,
    insertedText: String
  ): Buffer =
    val text    = buffer.content.collect()
    val ranges  = mergedActiveSelectionRanges(buffer, text)
    val offsets = ranges.map(_._1)
    val edits = ranges.zipWithIndex.map {
      case ((start, end), index) =>
        MultiCursorEdit(index, start, end, insertedText)
    }
    applyTrackedEdits(buffer, currentState, offsets, edits)

  private def deleteSelectedRanges(
    buffer: Buffer,
    currentState: AppState
  ): Buffer =
    val text    = buffer.content.collect()
    val ranges  = mergedActiveSelectionRanges(buffer, text)
    val offsets = ranges.map(_._1)
    val edits = ranges.zipWithIndex.map {
      case ((start, end), index) =>
        MultiCursorEdit(index, start, end, "")
    }
    applyTrackedEdits(buffer, currentState, offsets, edits)

  private def applyMultiCursorNavigation(
    buffer: Buffer,
    currentState: AppState
  )(move: CursorPosition => CursorPosition): Buffer =
    val finalCursors = buffer.cursors
      .map(move)
      .distinct
      .sortBy(cursor => (cursor.line, cursor.column))
    val primaryCursor = finalCursors.headOption.getOrElse(CursorPosition(0, 0))
    val baseBuffer = buffer.copy(
      cursors = finalCursors,
      selection = None,
      selections = Nil,
      preferredColumn = Some(primaryCursor.column),
      preferredXPx = None,
      multiCursorVerticalStates = Nil
    )
    baseBuffer.copy(viewport = adjustViewportForCursor(baseBuffer, currentState, primaryCursor))

  private def applyMultiCursorVerticalNavigation(
    buffer: Buffer,
    currentState: AppState,
    direction: Int
  ): Buffer =
    val cursorStates = multiCursorVerticalStates(buffer, currentState)
    val movedStates = cursorStates.map { cursorState =>
      cursorState.copy(
        cursor = moveMultiCursorVertical(
          cursorState.cursor,
          buffer,
          currentState,
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
      cursors = visibleCursors,
      selection = None,
      selections = Nil,
      preferredColumn = Some(primaryCursor.column),
      preferredXPx = None,
      multiCursorVerticalStates = sortedStates.map(cursorState =>
        VerticalCursorState(cursorState.cursor, cursorState.preferredColumn, cursorState.preferredXPx)
      )
    )
    baseBuffer.copy(viewport = adjustViewportForCursor(baseBuffer, currentState, primaryCursor))

  private def applyMultiCursorPageNavigation(
    buffer: Buffer,
    direction: Int
  ): Buffer =
    val totalLines = countLines(buffer.content)
    val visLines   = buffer.viewport.visibleLines
    val finalCursors = buffer.cursors
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
      cursors = finalCursors,
      selection = None,
      selections = Nil,
      preferredColumn = Some(primaryCursor.column),
      preferredXPx = None,
      multiCursorVerticalStates = Nil,
      viewport = buffer.viewport.copy(topLine = newTopLine, topVisualLine = 0)
    )

  private def multiCursorEntries(buffer: Buffer): List[CursorEntry] =
    multiCursorEntries(buffer, buffer.content.collect())

  private def multiCursorEntries(buffer: Buffer, text: String): List[CursorEntry] =
    buffer.cursors.distinct
      .map(cursor => CursorEntry(cursor, lineColumnToOffset(text, cursor.line, cursor.column)))
      .sortBy(_.offset)

  private def distinctCursorLines(buffer: Buffer): List[Int] =
    buffer.cursors.distinct
      .sortBy(cursor => (cursor.line, cursor.column))
      .map(_.line)
      .distinct

  private def selectionLines(buffer: Buffer): List[Int] =
    activeSelections(buffer)
      .flatMap(selection => selection.start.line to selection.end.line)
      .distinct
      .sorted

  private def updateBufferInState(state: AppState, buffer: Buffer): AppState =
    state.copy(buffers = state.buffers + (buffer.id -> buffer))

  private def deleteOffsetRange(
    buffer: Buffer,
    currentState: AppState,
    startOffset: Int,
    endOffset: Int,
    cursorOffset: Int
  ): Buffer =
    val initialText = Option.when(buffer.documentComments.nonEmpty)(buffer.content.collect())
    val newContent  = buffer.content.delete(startOffset, endOffset)
    val updatedText = Option.when(buffer.documentComments.nonEmpty)(newContent.collect())
    val newCursor = updatedText match
      case Some(text) => offsetToCursorPosition(text, newContent.weight, cursorOffset)
      case None       => offsetToCursorPosition(newContent, cursorOffset)
    val baseBuffer = buffer.copy(
      content = newContent,
      isDirty = true,
      isNewEmpty = false,
      cursors = newCursor :: buffer.cursors.tail,
      selection = None,
      selections = Nil,
      preferredColumn = Some(newCursor.column),
      preferredXPx = None,
      documentComments = initialText.zip(updatedText).fold(buffer.documentComments) {
        case (beforeText, afterText) =>
          adjustDocumentComments(
            buffer.documentComments,
            beforeText,
            afterText,
            List(MultiCursorEdit(0, startOffset, endOffset, ""))
          )
      }
    )
    val updatedViewport = adjustViewportForCursor(baseBuffer, currentState, newCursor)
    baseBuffer.copy(viewport = updatedViewport)

  private def offsetToCursorPosition(content: Rope, offset: Int): CursorPosition =
    offsetToCursorPosition(content.collect(), content.weight, offset)

  private def offsetToCursorPosition(text: String, contentLength: Int, offset: Int): CursorPosition =
    val clamped = math.max(0, math.min(offset, contentLength))
    val scanned = text.take(clamped).foldLeft(CursorPosition(0, 0)) { (cursor, char) =>
      if char == '\n' then CursorPosition(cursor.line + 1, 0)
      else cursor.copy(column = cursor.column + 1)
    }
    scanned

  private def findModalForBuffer(buffer: Buffer): Modal =
    buffer.findState match
      case Some(FindState(query, _, currentIndex)) if query.nonEmpty =>
        val resultSet = FindResultSet.normalized(query, findMatches(buffer, query).map(toFindResult), currentIndex)
        Modal.Find(resultSet.query, resultSet.results, resultSet.currentIndex)
      case _ =>
        Modal.Find("", Nil, 0)

  private def findMatches(buffer: Buffer, query: String): List[CursorPosition] =
    if query.isEmpty then Nil
    else
      val text = buffer.content.collect()
      findMatches(text, query).map(offset => offsetToCursorPosition(text, text.length, offset))

  private def findMatches(text: String, query: String): List[Int] =
    if query.isEmpty then Nil
    else
      @annotation.tailrec
      def loop(start: Int, acc: List[Int]): List[Int] =
        val index = text.indexOf(query, start)
        if index == -1 then acc.reverse
        else loop(index + query.length, index :: acc)

      loop(0, Nil)

  private def toFindResult(cursor: CursorPosition): FindResult =
    FindResult(cursor.line, cursor.column)

  private def moveCursorLeft(cursor: CursorPosition, content: Rope): CursorPosition =
    if cursor.column > 0 then cursor.moveLeft
    else if cursor.line > 0 then
      val prevLineEnd = findLineEnd(content, cursor.line - 1)
      cursor.copy(line = cursor.line - 1, column = prevLineEnd)
    else cursor

  private def moveCursorRight(cursor: CursorPosition, content: Rope): CursorPosition =
    val currentLineEnd = findLineEnd(content, cursor.line)
    if cursor.column < currentLineEnd then cursor.moveRight
    else
      val totalLines = countLines(content)
      if cursor.line < totalLines - 1 then cursor.copy(line = cursor.line + 1, column = 0)
      else cursor

  private def moveMultiCursorVertical(
    cursor: CursorPosition,
    buffer: Buffer,
    currentState: AppState,
    preferredColumn: Int,
    preferredXPx: Float,
    direction: Int
  ): CursorPosition =
    measuredVerticalMove(buffer, cursor, currentState, preferredXPx, direction).getOrElse {
      if direction < 0 then moveUpVisualLine(cursor, buffer.content, effectivePanelWidth(currentState), preferredColumn)
      else moveDownVisualLine(cursor, buffer.content, effectivePanelWidth(currentState), preferredColumn)
    }

  private def multiCursorVerticalStates(
    buffer: Buffer,
    currentState: AppState
  ): List[MultiCursorVerticalState] =
    val visibleCursors = buffer.cursors.distinct
      .sortBy(cursor => (cursor.line, cursor.column))
    val storedVisibleCursors = buffer.multiCursorVerticalStates
      .map(_.cursor)
      .distinct
      .sortBy(cursor => (cursor.line, cursor.column))

    if buffer.multiCursorVerticalStates.nonEmpty && storedVisibleCursors == visibleCursors then
      buffer.multiCursorVerticalStates.map(cursorState =>
        MultiCursorVerticalState(cursorState.cursor, cursorState.preferredColumn, cursorState.preferredXPx)
      )
    else
      visibleCursors.map(cursor =>
        MultiCursorVerticalState(cursor, cursor.column, measuredCursorXPx(buffer, currentState, cursor))
      )

  private def adjustViewportForCursor(
    buffer: Buffer,
    currentState: AppState,
    cursor: CursorPosition
  ): Viewport =
    CursorViewport.adjustForCursor(buffer, currentState, cursor)

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

  private def addCharacterAnimationToBuffer(
    buffer: Buffer,
    state: AppState,
    char: Char,
    cursorLine: Int,
    cursorColumn: Int
  ): Buffer =
    state.config.characterAnimation match
      case Some(animConfig) =>
        val updatedAnimations = buffer.animations.addCharacterAnimation(
          char,
          cursorColumn,
          cursorLine,
          state.theme.backgroundColor,
          state.theme.foregroundColor,
          animConfig.steps
        )
        buffer.copy(animations = updatedAnimations)
      case None =>
        buffer

  private def selectionFocusOrCursor(buffer: Buffer, cursor: CursorPosition): CursorPosition =
    buffer.primarySelection.map(_.focus).getOrElse(cursor)

  private def extendSelection(
    buffer: Buffer,
    currentState: AppState,
    anchor: CursorPosition,
    focus: CursorPosition,
    preferredColumn: Option[Int],
    preferredXPx: Option[Float] = None
  ): Buffer =
    val selectionAnchor = buffer.primarySelection.map(_.anchor).getOrElse(anchor)
    val baseBuffer = buffer.copy(
      cursors = focus :: buffer.cursors.tail,
      selection = Some(Selection(selectionAnchor, focus)),
      selections = Nil,
      preferredColumn = preferredColumn,
      preferredXPx = preferredXPx
    )
    baseBuffer.copy(viewport = adjustViewportForCursor(baseBuffer, currentState, focus))

  private def effectivePanelWidth(currentState: AppState): Int =
    val viewportSize = currentState.viewportSize.getOrElse(com.serenity.ui.layout.ViewportSize(80, 24))
    val layout       = com.serenity.ui.layout.LayoutEngine.calculateLayout(currentState, viewportSize)
    layout.editorPanelRect.width

  private def measuredCursorXPx(buffer: Buffer, currentState: AppState, cursor: CursorPosition): Float =
    val font         = previewFontForBuffer(buffer, currentState.config.fontConfig)
    val metrics      = CellMetrics.fromFont(font)
    val panelWidthPx = effectivePanelWidth(currentState) * metrics.charWidth
    val snapshot =
      TextLayoutSnapshot.fromBuffer(
        buffer.copy(viewport = buffer.viewport.copy(leftColumn = 0, topVisualLine = 0)),
        panelWidthPx,
        font,
        wordWrapEnabled = currentState.config.wordWrapEnabled
      )
    snapshot.xPxForCursor(cursor).getOrElse(cursor.column.toFloat * metrics.charWidth.toFloat)

  private def moveVerticalByLayout(
    cursor: CursorPosition,
    buffer: Buffer,
    currentState: AppState,
    preferredXPx: Float,
    direction: Int
  ): Option[CursorPosition] =
    val font         = previewFontForBuffer(buffer, currentState.config.fontConfig)
    val panelWidthPx = effectivePanelWidth(currentState) * CellMetrics.fromFont(font).charWidth
    val snapshot =
      TextLayoutSnapshot.fromBuffer(
        buffer.copy(viewport = buffer.viewport.copy(leftColumn = 0, topVisualLine = 0)),
        panelWidthPx,
        font,
        wordWrapEnabled = currentState.config.wordWrapEnabled
      )
    snapshot.moveVertical(cursor, direction, preferredXPx)

  private def measuredVerticalMove(
    buffer: Buffer,
    cursor: CursorPosition,
    currentState: AppState,
    preferredXPx: Float,
    direction: Int
  ): Option[CursorPosition] =
    Option
      .when(usesMeasuredVerticalNavigation(buffer)) {
        moveVerticalByLayout(cursor, buffer, currentState, preferredXPx, direction)
      }
      .flatten

  /** Compute the single shared snapshot + metrics for single-cursor vertical navigation. Both preferredXPx measurement
    * and vertical movement use the same snapshot.
    */
  private def navigationSnapshot(buffer: Buffer, state: AppState): (TextLayoutSnapshot, CellMetrics) =
    val font    = previewFontForBuffer(buffer, state.config.fontConfig)
    val metrics = CellMetrics.fromFont(font)
    val widthPx = effectivePanelWidth(state) * metrics.charWidth
    val snap =
      TextLayoutSnapshot.fromBuffer(
        buffer.copy(viewport = buffer.viewport.copy(leftColumn = 0, topVisualLine = 0)),
        widthPx,
        font,
        wordWrapEnabled = state.config.wordWrapEnabled
      )
    (snap, metrics)

  private def measuredCursorXPxFrom(snap: TextLayoutSnapshot, metrics: CellMetrics, cursor: CursorPosition): Float =
    snap.xPxForCursor(cursor).getOrElse(cursor.column.toFloat * metrics.charWidth.toFloat)

  private def moveVerticalBySnapshot(
    cursor: CursorPosition,
    snap: TextLayoutSnapshot,
    preferredXPx: Float,
    direction: Int
  ): Option[CursorPosition] =
    snap.moveVertical(cursor, direction, preferredXPx)

  private def measuredVerticalMoveBySnapshot(
    buffer: Buffer,
    cursor: CursorPosition,
    snap: TextLayoutSnapshot,
    preferredXPx: Float,
    direction: Int
  ): Option[CursorPosition] =
    Option
      .when(usesMeasuredVerticalNavigation(buffer)) {
        moveVerticalBySnapshot(cursor, snap, preferredXPx, direction)
      }
      .flatten

  private def usesMeasuredVerticalNavigation(buffer: Buffer): Boolean =
    buffer.usesTextFont

  private def previewFontForBuffer(
    buffer: Buffer,
    config: com.serenity.ui.fonts.FontLoader.FontConfig
  ): java.awt.Font =
    FontLoader.previewFontForRole(config, buffer.typographyRole)

  private def replaceSelectionOrInsert(buffer: Buffer, cursor: CursorPosition, insertedText: String): Buffer =
    val contentText = buffer.content.collect()
    val (baseContent, insertionStart, startOffset, endOffset) = buffer.primarySelection match
      case Some(selection) =>
        val startOffset = selectionStartOffset(selection, contentText)
        val endOffset   = selectionEndOffset(selection, contentText)
        (
          buffer.content.delete(startOffset, endOffset),
          selection.start,
          startOffset,
          endOffset
        )
      case None =>
        val startOffset = lineColumnToOffset(contentText, cursor.line, cursor.column)
        (
          buffer.content,
          cursor,
          startOffset,
          startOffset
        )

    val newContent = baseContent.insert(startOffset, insertedText)
    val newCursor  = cursorAfterInsertion(insertionStart, insertedText)

    buffer.copy(
      content = newContent,
      isDirty = true,
      isNewEmpty = false,
      cursors = newCursor :: buffer.cursors.tail,
      selection = None,
      selections = Nil,
      preferredColumn = Some(newCursor.column),
      preferredXPx = None,
      documentComments = adjustDocumentComments(
        buffer.documentComments,
        contentText,
        newContent,
        List(MultiCursorEdit(0, startOffset, endOffset, insertedText))
      )
    )

  private def deleteSelectedRange(
    buffer: Buffer,
    selection: Selection,
    currentState: AppState
  ): Buffer =
    val contentText = buffer.content.collect()
    val startOffset = selectionStartOffset(selection, contentText)
    val endOffset   = selectionEndOffset(selection, contentText)
    val newContent  = buffer.content.delete(startOffset, endOffset)
    val newCursor   = selection.start
    val baseBuffer = buffer.copy(
      content = newContent,
      isDirty = true,
      isNewEmpty = false,
      cursors = newCursor :: buffer.cursors.tail,
      selection = None,
      selections = Nil,
      preferredColumn = Some(newCursor.column),
      preferredXPx = None,
      documentComments = adjustDocumentComments(
        buffer.documentComments,
        contentText,
        newContent,
        List(MultiCursorEdit(0, startOffset, endOffset, ""))
      )
    )
    val updatedViewport = adjustViewportForCursor(baseBuffer, currentState, newCursor)

    baseBuffer.copy(viewport = updatedViewport)

  private def selectedText(buffer: Buffer, selection: Selection): String =
    val text        = buffer.content.collect()
    val startOffset = selectionStartOffset(selection, text)
    val endOffset   = selectionEndOffset(selection, text)
    text.slice(startOffset, endOffset)

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
    val text = buffer.content.collect()
    mergedActiveSelectionRanges(buffer, text).map { case (start, end) => text.slice(start, end) }

  private def mergedActiveSelectionRanges(buffer: Buffer, text: String): List[(Int, Int)] =
    val ranges = activeSelections(buffer)
      .map(selection => (selectionStartOffset(selection, text), selectionEndOffset(selection, text)))
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

  private def selectionStartOffset(selection: Selection, text: String): Int =
    lineColumnToOffset(text, selection.start.line, selection.start.column)

  private def selectionEndOffset(selection: Selection, text: String): Int =
    lineColumnToOffset(text, selection.end.line, selection.end.column)

  private def collapseSelectionsToFocus(buffer: Buffer, currentState: AppState): Buffer =
    val cursors = activeSelections(buffer)
      .map(_.focus)
      .distinct
      .sortBy(cursor => (cursor.line, cursor.column))
    val primaryCursor = cursors.headOption.getOrElse(CursorPosition(0, 0))
    val baseBuffer = buffer.copy(
      cursors = cursors,
      selection = None,
      selections = Nil,
      preferredColumn = Some(primaryCursor.column),
      preferredXPx = None
    )
    baseBuffer.copy(viewport = adjustViewportForCursor(baseBuffer, currentState, primaryCursor))

  private def cursorAfterInsertion(start: CursorPosition, insertedText: String): CursorPosition =
    val lines = insertedText.split("\n", -1)
    if lines.length == 1 then start.copy(column = start.column + insertedText.length)
    else CursorPosition(start.line + lines.length - 1, lines.last.length)
