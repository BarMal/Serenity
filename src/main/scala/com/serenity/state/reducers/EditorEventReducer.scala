package com.serenity.state.reducers

import com.serenity.animation.*
import com.serenity.keystroke.events.*
import com.serenity.richtext.{RichTextDocument, RichTextPosition, RichTextRange}
import com.serenity.rope.Rope
import com.serenity.state.models.*
import com.serenity.text.TextEditing
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot}

object EditorEventReducer:
  private val TabInsertion = "    "
  private val OriginCursor = CursorPosition(0, 0)

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
  ): ReducerResult =
    val result =
      if isExtendSelectionEvent(event) then
        reduceSingleCursorTextEvent(event, clearInFlightMultiCursorVerticalState(buffer), paneId, currentState)
      else if buffer.allSelections.nonEmpty then reduceMultiSelectionTextEvent(event, buffer, paneId, currentState)
      else if preservesInFlightMultiCursorVerticalState(event, buffer) then
        reduceMultiCursorTextEvent(event, buffer, paneId, currentState)
      else if buffer.cursors.size > 1 then reduceMultiCursorTextEvent(event, buffer, paneId, currentState)
      else reduceSingleCursorTextEvent(event, clearInFlightMultiCursorVerticalState(buffer), paneId, currentState)

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
      case ExtendSelectionLeft | ExtendSelectionRight | ExtendSelectionUp | ExtendSelectionDown => true
      case _                                                                                    => false

  private def invalidateFindState(state: AppState, bufferId: BufferId): AppState =
    state.buffers.get(bufferId) match
      case Some(buffer) =>
        state.copy(buffers = state.buffers + (bufferId -> buffer.copy(findState = None)))
      case _ =>
        state

  private def preservesInFlightMultiCursorVerticalState(event: TextEntryEvent, buffer: Buffer): Boolean =
    buffer.multiCursorVerticalStates.size > 1 && (event == MoveUp || event == MoveDown)

  private def clearInFlightMultiCursorVerticalState(buffer: Buffer): Buffer =
    if buffer.multiCursorVerticalStates.isEmpty then buffer
    else buffer.copy(multiCursorVerticalStates = Nil)

  /** All four deletions share a selection arm and differ only in the range they delete when there is none. */
  private def reduceDeletion(
    buffer: Buffer,
    currentState: AppState,
    withoutSelection: Buffer => Option[Buffer]
  ): ReducerResult =
    ReducerResult.fromTransition(
      currentState,
      Focused.modifyBufferWithId(buffer.id) { current =>
        current.primarySelection match
          case Some(selection) => deleteSelectedRange(current, selection, currentState)
          case None            => withoutSelection(current).getOrElse(current)
      }
    )

  private def graphemeBackwardDeletion(
    buffer: Buffer,
    cursor: CursorPosition,
    currentState: AppState
  ): Option[Buffer] =
    val offset = lineColumnToOffset(buffer.content, cursor.line, cursor.column)
    backwardGraphemeDeletionRange(buffer.content, offset).map {
      case (start, end) =>
        val newContent = buffer.content.delete(start, end)
        val newCursor  = offsetToCursorPosition(newContent, start)
        buffer.copy(
          content = newContent,
          isDirty = true,
          isNewEmpty = false,
          cursors = newCursor :: buffer.cursors.tail,
          selection = None,
          preferredColumn = Some(newCursor.column),
          preferredXPx = None,
          viewport = adjustViewportForCursor(buffer, currentState, newCursor),
          documentComments = adjustDocumentComments(
            buffer.documentComments,
            buffer.content,
            newContent,
            List(MultiCursorEdit(0, start, end, ""))
          ),
          richTextDocument = richTextDocumentAfterEdit(buffer, start, end, "")
        )
    }

  /** Forward deletion leaves the cursor where it is, so unlike the backward case it does not adjust the viewport. */
  private def graphemeForwardDeletion(buffer: Buffer, cursor: CursorPosition): Option[Buffer] =
    val offset = lineColumnToOffset(buffer.content, cursor.line, cursor.column)
    forwardGraphemeDeletionRange(buffer.content, offset).map {
      case (start, end) =>
        val newContent = buffer.content.delete(start, end)
        val newCursor  = offsetToCursorPosition(newContent, start)
        buffer.copy(
          content = newContent,
          isDirty = true,
          isNewEmpty = false,
          cursors = newCursor :: buffer.cursors.tail,
          preferredColumn = Some(newCursor.column),
          preferredXPx = None,
          documentComments = adjustDocumentComments(
            buffer.documentComments,
            buffer.content,
            newContent,
            List(MultiCursorEdit(0, start, end, ""))
          ),
          richTextDocument = richTextDocumentAfterEdit(buffer, start, end, "")
        )
    }

  private def wordBackwardDeletion(buffer: Buffer, cursor: CursorPosition, currentState: AppState): Option[Buffer] =
    val offset = lineColumnToOffset(buffer.content, cursor.line, cursor.column)
    val start  = previousWordBoundary(buffer.content, offset)
    Option.when(start < offset)(deleteOffsetRange(buffer, currentState, start, offset, start))

  private def wordForwardDeletion(buffer: Buffer, cursor: CursorPosition, currentState: AppState): Option[Buffer] =
    val offset = lineColumnToOffset(buffer.content, cursor.line, cursor.column)
    val end    = nextWordBoundary(buffer.content, offset)
    Option.when(offset < end)(deleteOffsetRange(buffer, currentState, offset, end, offset))

  private def insertAtCursor(
    buffer: Buffer,
    cursor: CursorPosition,
    text: String,
    currentState: AppState
  ): ReducerResult =
    ReducerResult.fromTransition(
      currentState,
      Focused.modifyBufferWithId(buffer.id) { current =>
        val (replaced, edit) = replaceSelectionOrInsert(current, cursor, text)
        val newCursor        = replaced.cursors.headOption.getOrElse(cursor)
        val viewport         = adjustViewportForCursor(replaced, currentState, newCursor)
        addInsertionAnimations(replaced.copy(viewport = viewport), currentState, List(edit))
      }
    )

  /** Callers adjust the buffer on the way in -- collapsing selections, clearing in-flight vertical state -- and the
    * arms read it back out of the state, so the adjusted buffer has to be seeded there or the adjustment is silently
    * lost.
    */
  private def reduceSingleCursorTextEvent(
    event: TextEntryEvent,
    buffer: Buffer,
    paneId: PaneId,
    incomingState: AppState
  ): ReducerResult =
    val currentState = Focused.replaceBuffer(incomingState, buffer)

    buffer.cursors.headOption match
      case Some(cursor) =>
        event match
          case InsertChar(char) =>
            insertAtCursor(buffer, cursor, char.toString, currentState)

          case TabKey =>
            insertAtCursor(buffer, cursor, TabInsertion, currentState)

          case ReverseTabKey =>
            ReducerResult.noEffects(
              Focused.replaceBuffer(currentState, applyLineUnindent(buffer, currentState, List(cursor.line)))
            )

          case DeleteBackward =>
            reduceDeletion(buffer, currentState, graphemeBackwardDeletion(_, cursor, currentState))

          case DeleteForward =>
            reduceDeletion(buffer, currentState, graphemeForwardDeletion(_, cursor))

          case DeleteWordBackward =>
            reduceDeletion(buffer, currentState, wordBackwardDeletion(_, cursor, currentState))

          case DeleteWordForward =>
            reduceDeletion(buffer, currentState, wordForwardDeletion(_, cursor, currentState))

          case MoveLeft =>
            reduceMovement(buffer, selectionFocusOrCursor(buffer, cursor), currentState)(leftTarget)

          case MoveRight =>
            reduceMovement(buffer, selectionFocusOrCursor(buffer, cursor), currentState)(rightTarget)

          case MoveWordLeft =>
            reduceMovement(buffer, selectionFocusOrCursor(buffer, cursor), currentState)(wordLeftTarget)

          case MoveWordRight =>
            reduceMovement(buffer, selectionFocusOrCursor(buffer, cursor), currentState)(wordRightTarget)

          case MoveUp =>
            reduceMovement(buffer, selectionFocusOrCursor(buffer, cursor), currentState)(
              verticalTarget(currentState, -1)
            )

          case MoveDown =>
            reduceMovement(buffer, selectionFocusOrCursor(buffer, cursor), currentState)(
              verticalTarget(currentState, 1)
            )

          case ExtendSelectionLeft =>
            reduceSelectionExtension(buffer, cursor, currentState)(leftTarget)

          case ExtendSelectionRight =>
            reduceSelectionExtension(buffer, cursor, currentState)(rightTarget)

          case ExtendSelectionUp =>
            reduceSelectionExtension(buffer, cursor, currentState)(verticalTarget(currentState, -1))

          case ExtendSelectionDown =>
            reduceSelectionExtension(buffer, cursor, currentState)(verticalTarget(currentState, 1))

          case NewLine | Enter =>
            insertAtCursor(buffer, cursor, "\n", currentState)

          case MoveToStart =>
            reduceMovement(buffer, cursor, currentState)((_, from) => horizontalTarget(from.copy(column = 0)))

          case MoveToEnd =>
            reduceMovement(buffer, cursor, currentState) { (current, from) =>
              horizontalTarget(from.copy(column = findLineEnd(current.content, from.line)))
            }

          case SelectAll =>
            ReducerResult.fromTransition(
              currentState,
              Focused.modifyBufferWithId(buffer.id) { current =>
                val lastLine  = math.max(0, countLines(current.content) - 1)
                val endCursor = CursorPosition(lastLine, findLineEnd(current.content, lastLine))
                current.copy(
                  cursors = List(endCursor),
                  selection = Some(Selection(CursorPosition(0, 0), endCursor)),
                  preferredColumn = Some(endCursor.column),
                  preferredXPx = None,
                  viewport = adjustViewportForCursor(current, currentState, endCursor)
                )
              }
            )

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

          case OpenReplace =>
            ModalStateReducer.show(Modal.ReplaceWorkflow(ReplaceWorkflowState()), currentState)

          case FindNext =>
            buffer.findState match
              case Some(FindState(query, storedResults, currentIndex)) if storedResults.nonEmpty =>
                val validResults = storedResults.filter { result =>
                  isWholeGraphemeMatch(
                    buffer.content,
                    lineColumnToOffset(buffer.content, result.line, result.column),
                    query.length
                  )
                }
                val resultSet = FindResultSet.normalized(query, validResults, currentIndex + 1)
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
            val clipboardText = buffer.content.getLine(cursor.line).getOrElse("")
            ReducerResult.noEffects(currentState.copy(clipboard = Some(clipboardText)))

          case Cut =>
            val lineText  = buffer.content.getLine(cursor.line).getOrElse("")
            val lineStart = lineColumnToOffset(buffer.content, cursor.line, 0)
            val lineEnd   = lineColumnToOffset(buffer.content, cursor.line, lineText.length)
            val lineCount = countLines(buffer.content)
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
                buffer.content,
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
                val (replacedBuffer, replacementEdit) = replaceSelectionOrInsert(buffer, cursor, text)
                val newCursor                         = replacedBuffer.cursors.headOption.getOrElse(cursor)
                val updatedBuffer = addInsertionAnimations(
                  buffer.copy(
                    content = replacedBuffer.content,
                    isDirty = replacedBuffer.isDirty,
                    isNewEmpty = replacedBuffer.isNewEmpty,
                    cursors = replacedBuffer.cursors,
                    selection = replacedBuffer.selection,
                    preferredColumn = Some(newCursor.column),
                    preferredXPx = None,
                    viewport = adjustViewportForCursor(buffer, currentState, newCursor)
                  ),
                  currentState,
                  List(replacementEdit)
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
    incomingState: AppState
  ): ReducerResult =
    val currentState = Focused.replaceBuffer(incomingState, buffer)
    event match
      case InsertChar(char) =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(currentState, applyMultiSelectionReplacement(buffer, currentState, char.toString))
        )
      case TabKey =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(currentState, applyLineIndent(buffer, currentState, selectionLines(buffer)))
        )
      case NewLine | Enter =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(currentState, applyMultiSelectionReplacement(buffer, currentState, "\n"))
        )
      case Paste =>
        currentState.clipboard.filter(_.nonEmpty) match
          case Some(text) =>
            ReducerResult.noEffects(
              Focused.replaceBuffer(currentState, applyMultiSelectionReplacement(buffer, currentState, text))
            )
          case None =>
            ReducerResult.noEffects(currentState)
      case ReverseTabKey =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(currentState, applyLineUnindent(buffer, currentState, selectionLines(buffer)))
        )
      case DeleteBackward | DeleteForward | DeleteWordBackward | DeleteWordForward =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(currentState, deleteSelectedRanges(buffer, currentState))
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
      case SelectAll | OpenGotoLine | OpenFind | OpenReplace | FindNext | Escape =>
        reduceGlobalTextEvent(event, buffer, paneId, currentState)
      case _ =>
        reduceGlobalTextEvent(event, buffer, paneId, currentState)

  private def reduceMultiCursorTextEvent(
    event: TextEntryEvent,
    buffer: Buffer,
    paneId: PaneId,
    incomingState: AppState
  ): ReducerResult =
    val currentState = Focused.replaceBuffer(incomingState, buffer)
    event match
      case InsertChar(char) =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(currentState, applyMultiCursorInsertion(buffer, currentState, char.toString))
        )
      case TabKey =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(currentState, applyMultiCursorInsertion(buffer, currentState, TabInsertion))
        )
      case NewLine | Enter =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(currentState, applyMultiCursorInsertion(buffer, currentState, "\n"))
        )
      case Paste =>
        currentState.clipboard.filter(_.nonEmpty) match
          case Some(text) =>
            ReducerResult.noEffects(
              Focused.replaceBuffer(currentState, applyMultiCursorInsertion(buffer, currentState, text))
            )
          case None =>
            ReducerResult.noEffects(currentState)
      case DeleteBackward =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(currentState, applyMultiCursorDeletion(buffer, currentState, backward = true))
        )
      case DeleteForward =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(currentState, applyMultiCursorDeletion(buffer, currentState, backward = false))
        )
      case DeleteWordBackward =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(currentState, applyMultiCursorWordDeletion(buffer, currentState, backward = true))
        )
      case DeleteWordForward =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(currentState, applyMultiCursorWordDeletion(buffer, currentState, backward = false))
        )
      case ReverseTabKey =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(currentState, applyLineUnindent(buffer, currentState, distinctCursorLines(buffer)))
        )
      case MoveLeft =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(
            currentState,
            applyMultiCursorNavigation(buffer, currentState)(cursor => moveCursorLeft(cursor, buffer.content))
          )
        )
      case MoveRight =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(
            currentState,
            applyMultiCursorNavigation(buffer, currentState)(cursor => moveCursorRight(cursor, buffer.content))
          )
        )
      case MoveToStart =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(
            currentState,
            applyMultiCursorNavigation(buffer, currentState)(cursor => cursor.copy(column = 0))
          )
        )
      case MoveToEnd =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(
            currentState,
            applyMultiCursorNavigation(buffer, currentState)(cursor =>
              cursor.copy(column = findLineEnd(buffer.content, cursor.line))
            )
          )
        )
      case MoveUp =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(currentState, applyMultiCursorVerticalNavigation(buffer, currentState, direction = -1))
        )
      case MoveDown =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(currentState, applyMultiCursorVerticalNavigation(buffer, currentState, direction = 1))
        )
      case PageUp =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(currentState, applyMultiCursorPageNavigation(buffer, direction = -1))
        )
      case PageDown =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(currentState, applyMultiCursorPageNavigation(buffer, direction = 1))
        )
      case MoveToStartOfFile =>
        ReducerResult.noEffects(
          Focused.replaceBuffer(currentState, applyMultiCursorNavigation(buffer, currentState)(_ => OriginCursor))
        )
      case MoveToEndOfFile =>
        val totalLines  = countLines(buffer.content)
        val lastLine    = totalLines - 1
        val lastLineEnd = findLineEnd(buffer.content, lastLine)
        val target      = CursorPosition(lastLine, lastLineEnd)
        ReducerResult.noEffects(
          Focused.replaceBuffer(currentState, applyMultiCursorNavigation(buffer, currentState)(_ => target))
        )
      case Copy =>
        val clipboardText = distinctCursorLines(buffer)
          .map(line => buffer.content.getLine(line).getOrElse(""))
          .mkString("\n")
        ReducerResult.noEffects(currentState.copy(clipboard = Some(clipboardText)))
      case Cut =>
        val targetLines   = distinctCursorLines(buffer)
        val clipboardText = targetLines.map(line => buffer.content.getLine(line).getOrElse("")).mkString("\n")
        val updatedBuffer = applyMultiCursorLineCut(buffer, currentState, targetLines)
        ReducerResult.noEffects(
          currentState.copy(
            buffers = currentState.buffers + (buffer.id -> updatedBuffer),
            clipboard = Some(clipboardText)
          )
        )
      case SelectAll | OpenGotoLine | OpenFind | OpenReplace | FindNext | Escape =>
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
        val bufferWithAnimation = addInsertionAnimations(
          buffer,
          currentState,
          List(MultiCursorEdit(0, 0, 0, char.toString))
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
  ): ReducerResult =
    reduceSingleCursorTextEvent(
      event,
      clearInFlightMultiCursorVerticalState(buffer.copy(selection = buffer.primarySelection, selections = Nil)),
      paneId,
      currentState
    )

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
    currentState: AppState,
    insertedText: String
  ): Buffer =
    val insertionOffsets =
      multiCursorEntries(buffer).map(entry => graphemeBoundaryAfterOrAt(buffer.content, entry.offset))
    val edits = insertionOffsets.zipWithIndex.map {
      case (offset, index) =>
        MultiCursorEdit(index, offset, offset, insertedText)
    }
    applyTrackedEdits(buffer, currentState, insertionOffsets, edits)

  private def applyMultiCursorDeletion(
    buffer: Buffer,
    currentState: AppState,
    backward: Boolean
  ): Buffer =
    val entries = multiCursorEntries(buffer)
    val edits = entries.zipWithIndex.flatMap {
      case (entry, index) =>
        val range =
          if backward then backwardGraphemeDeletionRange(buffer.content, entry.offset)
          else forwardGraphemeDeletionRange(buffer.content, entry.offset)
        range.map { case (start, end) => MultiCursorEdit(index, start, end, "") }
    }
    applyTrackedEdits(buffer, currentState, entries.map(_.offset), edits)

  private def applyMultiCursorWordDeletion(
    buffer: Buffer,
    currentState: AppState,
    backward: Boolean
  ): Buffer =
    val entries = multiCursorEntries(buffer)
    val edits = entries.zipWithIndex.flatMap {
      case (entry, index) =>
        if backward then
          val start = previousWordBoundary(buffer.content, entry.offset)
          Option.when(start < entry.offset)(MultiCursorEdit(index, start, entry.offset, ""))
        else
          val end = nextWordBoundary(buffer.content, entry.offset)
          Option.when(entry.offset < end)(MultiCursorEdit(index, entry.offset, end, ""))
    }
    applyMergedDeletionEdits(buffer, currentState, entries.map(_.offset), edits)

  private def applyLineIndent(
    buffer: Buffer,
    currentState: AppState,
    targetLines: List[Int]
  ): Buffer =
    val targetSet = targetLines.filter(line => line >= 0 && line < countLines(buffer.content)).toSet

    if targetSet.isEmpty then buffer
    else
      val edits = targetSet.toList.sorted.zipWithIndex.map {
        case (line, index) =>
          val offset = lineColumnToOffset(buffer.content, line, 0)
          MultiCursorEdit(index, offset, offset, TabInsertion)
      }
      val updatedContent = edits
        .sortBy(edit => (-edit.start, -edit.end))
        .foldLeft(buffer.content)((content, edit) => content.insert(edit.start, edit.insertedText))
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
        documentComments = adjustDocumentComments(buffer.documentComments, buffer.content, updatedContent, edits)
      )
      addInsertionAnimations(
        baseBuffer.copy(viewport = adjustViewportForCursor(baseBuffer, currentState, primaryCursor)),
        currentState,
        edits
      )

  private def applyLineUnindent(
    buffer: Buffer,
    currentState: AppState,
    targetLines: List[Int]
  ): Buffer =
    val targetSet = targetLines.filter(line => line >= 0 && line < countLines(buffer.content)).toSet
    val removals = targetSet.toList.sorted.map { line =>
      val (_, removed) = unindentLine(buffer.content.getLine(line).getOrElse(""))
      line -> removed
    }.toMap

    if removals.values.forall(_ == 0) then buffer
    else
      val edits = removals.toList.sortBy(_._1).zipWithIndex.collect {
        case ((line, removed), index) if removed > 0 =>
          val start = lineColumnToOffset(buffer.content, line, 0)
          MultiCursorEdit(index, start, start + removed, "")
      }
      val updatedContent = edits
        .sortBy(edit => (-edit.start, -edit.end))
        .foldLeft(buffer.content)((content, edit) => content.delete(edit.start, edit.end))
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
        documentComments = adjustDocumentComments(buffer.documentComments, buffer.content, updatedContent, edits)
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
    targetLines: List[Int]
  ): Buffer =
    if targetLines.isEmpty then buffer
    else
      val totalLines = countLines(buffer.content)
      val lineEdits = targetLines.distinct.sorted.map { line =>
        val lineText  = buffer.content.getLine(line).getOrElse("")
        val lineStart = lineColumnToOffset(buffer.content, line, 0)
        val lineEnd   = lineColumnToOffset(buffer.content, line, lineText.length)
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
        content = updatedContent,
        isDirty = true,
        isNewEmpty = false,
        cursors = finalCursors,
        selection = None,
        selections = Nil,
        preferredColumn = Some(primaryCursor.column),
        preferredXPx = None,
        multiCursorVerticalStates = Nil,
        documentComments = adjustDocumentComments(buffer.documentComments, buffer.content, updatedContent, edits)
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
      val trackedOffsets = initialOffsets.toArray
      val sortedEdits    = edits.sortBy(edit => (-edit.start, -edit.end))
      val (updatedContent, updatedRichTextDocument) = sortedEdits.foldLeft((buffer.content, buffer.richTextDocument)) {
        case ((content, document), edit) =>
          val deleted     = content.delete(edit.start, edit.end)
          val nextContent = deleted.insert(edit.start, edit.insertedText)
          val nextDocument = richTextDocumentAfterEdit(
            buffer.copy(content = content, richTextDocument = document),
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
        content = updatedContent,
        isDirty = true,
        isNewEmpty = false,
        cursors = finalCursors,
        selection = None,
        selections = Nil,
        preferredColumn = Some(primaryCursor.column),
        preferredXPx = None,
        multiCursorVerticalStates = Nil,
        documentComments = adjustDocumentComments(buffer.documentComments, buffer.content, updatedContent, edits),
        richTextDocument = updatedRichTextDocument
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
      val finalCursors = finalOffsets
        .map(offset => offsetToCursorPosition(updatedContent, offset))
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
        documentComments = adjustDocumentComments(buffer.documentComments, buffer.content, updatedContent, mergedEdits)
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
    currentState: AppState,
    insertedText: String
  ): Buffer =
    val ranges  = mergedActiveSelectionRanges(buffer, buffer.content)
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
    val ranges  = mergedActiveSelectionRanges(buffer, buffer.content)
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
    val (navSnap, navMetrics) = navigationSnapshot(buffer, currentState)
    val cursorStates          = multiCursorVerticalStates(buffer, navSnap, navMetrics)
    val movedStates = cursorStates.map { cursorState =>
      cursorState.copy(
        cursor = moveMultiCursorVertical(
          cursorState.cursor,
          buffer,
          currentState,
          navSnap,
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
    buffer.cursors.distinct
      .map(cursor => CursorEntry(cursor, lineColumnToOffset(buffer.content, cursor.line, cursor.column)))
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

  private def deleteOffsetRange(
    buffer: Buffer,
    currentState: AppState,
    startOffset: Int,
    endOffset: Int,
    cursorOffset: Int
  ): Buffer =
    val newContent = buffer.content.delete(startOffset, endOffset)
    val newCursor  = offsetToCursorPosition(newContent, cursorOffset)
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
        buffer.content,
        newContent,
        List(MultiCursorEdit(0, startOffset, endOffset, ""))
      ),
      richTextDocument = richTextDocumentAfterEdit(buffer, startOffset, endOffset, "")
    )
    val updatedViewport = adjustViewportForCursor(baseBuffer, currentState, newCursor)
    baseBuffer.copy(viewport = updatedViewport)

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
    navSnap: TextLayoutSnapshot,
    preferredColumn: Int,
    preferredXPx: Float,
    direction: Int
  ): CursorPosition =
    measuredVerticalMoveBySnapshot(currentState.config.wordWrapEnabled, cursor, navSnap, preferredXPx, direction)
      .getOrElse(fallbackVerticalMove(cursor, buffer, currentState, preferredColumn, direction))

  private def multiCursorVerticalStates(
    buffer: Buffer,
    navSnap: TextLayoutSnapshot,
    navMetrics: CellMetrics
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
        MultiCursorVerticalState(cursor, cursor.column, measuredCursorXPxFrom(navSnap, navMetrics, cursor))
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

  private def fallbackVerticalMove(
    cursor: CursorPosition,
    buffer: Buffer,
    currentState: AppState,
    preferredColumn: Int,
    direction: Int
  ): CursorPosition =
    if currentState.config.wordWrapEnabled then
      if direction < 0 then moveUpVisualLine(cursor, buffer.content, effectivePanelWidth(currentState), preferredColumn)
      else moveDownVisualLine(cursor, buffer.content, effectivePanelWidth(currentState), preferredColumn)
    else if direction < 0 then moveUpLogicalLine(cursor, buffer.content, preferredColumn)
    else moveDownLogicalLine(cursor, buffer.content, preferredColumn)

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

  private def addInsertionAnimations(
    buffer: Buffer,
    state: AppState,
    edits: List[MultiCursorEdit]
  ): Buffer =
    val sortedEdits = edits
      .filter(_.insertedText.nonEmpty)
      .sortBy(edit => (edit.start, edit.end))

    if sortedEdits.isEmpty then buffer
    else
      val insertedCells = insertedTransitionCells(buffer.content, sortedEdits, state)
      if insertedCells.isEmpty then buffer
      else
        val plan = ElementTransitionPlanner.plan(
          ElementTransitionRequest(TransitionScope.EditorInsertion),
          state.config.editorInsertionTransitionSettings
        )
        if plan.kind == TransitionKind.Disabled then buffer
        else if plan.kind == TransitionKind.Fade then
          state.config.scaledCharacterAnimation match
            case Some(animConfig) =>
              if insertedCells.size == 1 then
                val (key, cell) = insertedCells.head
                buffer.copy(
                  animations = buffer.animations.addCharacterAnimation(
                    cell.char,
                    key.column,
                    key.line,
                    cell.startColor,
                    cell.endColor,
                    animConfig.steps
                  )
                )
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
                buffer.copy(animations = buffer.animations.mergeAnimations(staggeredCells))
            case None =>
              buffer
        else
          val animationState = ElementTransitionLowerer.lower(
            plan,
            ElementTransitionCells(content = insertedCells),
            tickRateMs = 16
          )
          buffer.copy(animations = buffer.animations.mergeAnimations(animationState.animations))

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
    horizontalTarget(moveCursorLeft(from, buffer.content))

  private def rightTarget(buffer: Buffer, from: CursorPosition): CursorTarget =
    horizontalTarget(moveCursorRight(from, buffer.content))

  private def wordLeftTarget(buffer: Buffer, from: CursorPosition): CursorTarget =
    horizontalTarget(wordBoundaryFrom(buffer, from, previousWordBoundary))

  private def wordRightTarget(buffer: Buffer, from: CursorPosition): CursorTarget =
    horizontalTarget(wordBoundaryFrom(buffer, from, nextWordBoundary))

  private def wordBoundaryFrom(buffer: Buffer, from: CursorPosition, boundary: (Rope, Int) => Int): CursorPosition =
    val offset = lineColumnToOffset(buffer.content, from.line, from.column)
    offsetToCursorPosition(buffer.content, boundary(buffer.content, offset))

  private def verticalTarget(currentState: AppState, direction: Int)(
    buffer: Buffer,
    from: CursorPosition
  ): CursorTarget =
    val preferredColumn       = buffer.preferredColumn.getOrElse(from.column)
    val (navSnap, navMetrics) = navigationSnapshot(buffer, currentState)
    val preferredXPx          = buffer.preferredXPx.getOrElse(measuredCursorXPxFrom(navSnap, navMetrics, from))
    val landed =
      measuredVerticalMoveBySnapshot(currentState.config.wordWrapEnabled, from, navSnap, preferredXPx, direction)
        .getOrElse(fallbackVerticalMove(from, buffer, currentState, preferredColumn, direction))

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
          cursors = landed.cursor :: current.cursors.tail,
          selection = None,
          preferredColumn = Some(landed.preferredColumn),
          preferredXPx = landed.preferredXPx,
          viewport = adjustViewportForCursor(current, currentState, landed.cursor)
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
        extendSelection(current, currentState, cursor, landed.cursor, Some(landed.preferredColumn), landed.preferredXPx)
      }
    )

  private def selectionFocusOrCursor(buffer: Buffer, cursor: CursorPosition): CursorPosition =
    buffer.primarySelection.map(_.focus).getOrElse(cursor)

  private def extendSelection(
    buffer: Buffer,
    currentState: AppState,
    anchor: CursorPosition,
    focus: CursorPosition,
    preferredColumn: Option[Int],
    preferredXPx: Option[Float]
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
    wordWrapEnabled: Boolean,
    cursor: CursorPosition,
    snap: TextLayoutSnapshot,
    preferredXPx: Float,
    direction: Int
  ): Option[CursorPosition] =
    Option
      .when(wordWrapEnabled) {
        moveVerticalBySnapshot(cursor, snap, preferredXPx, direction)
      }
      .flatten

  private def previewFontForBuffer(
    buffer: Buffer,
    config: com.serenity.ui.fonts.FontLoader.FontConfig
  ): java.awt.Font =
    FontLoader.previewFontForRole(config, buffer.typographyRole)

  private def replaceSelectionOrInsert(
    buffer: Buffer,
    cursor: CursorPosition,
    insertedText: String
  ): (Buffer, MultiCursorEdit) =
    val (baseContent, insertionStart, startOffset, endOffset) = buffer.primarySelection match
      case Some(selection) =>
        val startOffset = selectionStartOffset(selection, buffer.content)
        val endOffset   = selectionEndOffset(selection, buffer.content)
        (
          buffer.content.delete(startOffset, endOffset),
          offsetToCursorPosition(buffer.content, startOffset),
          startOffset,
          endOffset
        )
      case None =>
        val startOffset =
          graphemeBoundaryAfterOrAt(buffer.content, lineColumnToOffset(buffer.content, cursor.line, cursor.column))
        (
          buffer.content,
          offsetToCursorPosition(buffer.content, startOffset),
          startOffset,
          startOffset
        )

    val newContent      = baseContent.insert(startOffset, insertedText)
    val newCursor       = cursorAfterInsertion(insertionStart, insertedText)
    val replacementEdit = MultiCursorEdit(0, startOffset, endOffset, insertedText)

    (
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
          buffer.content,
          newContent,
          List(replacementEdit)
        ),
        richTextDocument = richTextDocumentAfterEdit(buffer, startOffset, endOffset, insertedText)
      ),
      replacementEdit
    )

  private def deleteSelectedRange(
    buffer: Buffer,
    selection: Selection,
    currentState: AppState
  ): Buffer =
    val startOffset = selectionStartOffset(selection, buffer.content)
    val endOffset   = selectionEndOffset(selection, buffer.content)
    val newContent  = buffer.content.delete(startOffset, endOffset)
    val newCursor   = offsetToCursorPosition(newContent, startOffset)
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
        buffer.content,
        newContent,
        List(MultiCursorEdit(0, startOffset, endOffset, ""))
      ),
      richTextDocument = richTextDocumentAfterEdit(buffer, startOffset, endOffset, "")
    )
    val updatedViewport = adjustViewportForCursor(baseBuffer, currentState, newCursor)

    baseBuffer.copy(viewport = updatedViewport)

  private def selectedText(buffer: Buffer, selection: Selection): String =
    val startOffset = selectionStartOffset(selection, buffer.content)
    val endOffset   = selectionEndOffset(selection, buffer.content)
    buffer.content.sliceString(startOffset, endOffset)

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
    mergedActiveSelectionRanges(buffer, buffer.content).map {
      case (start, end) =>
        buffer.content.sliceString(start, end)
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
    buffer.richTextDocument.flatMap { document =>
      Option.when(document.matchesPlainText(buffer.content.collect())) {
        val updatedDocument = document
          .replaceRange(
            RichTextRange(
              richTextPositionForOffset(buffer.content, startOffset),
              richTextPositionForOffset(buffer.content, endOffset)
            ),
            insertedText
          )
          .normalized
        buffer.insertionRichTextStyle
          .filter(_ => insertedText.nonEmpty)
          .map { style =>
            val updatedContent = buffer.content.delete(startOffset, endOffset).insert(startOffset, insertedText)
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
