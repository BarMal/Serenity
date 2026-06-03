package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Rope
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot}

object EditorEventReducer:

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
            val newViewport   = buffer.viewport.copy(topLine = newTopLine)
            val updatedBuffer = buffer.copy(viewport = newViewport)
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))
          case None => ReducerResult.noEffects(currentState)

      case ScrollUp(lines) =>
        pane.bufferId.flatMap(currentState.buffers.get) match
          case Some(buffer) =>
            val newTopLine    = math.max(0, buffer.viewport.topLine - lines)
            val newViewport   = buffer.viewport.copy(topLine = newTopLine)
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
    if buffer.allSelections.nonEmpty then
      reduceMultiSelectionTextEvent(event, buffer, paneId, currentState)
    else if buffer.cursors.size > 1 then
      reduceMultiCursorTextEvent(event, buffer, paneId, currentState)
    else
      reduceSingleCursorTextEvent(event, buffer, paneId, currentState)

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
            val updatedViewport = adjustViewportForCursor(buffer, currentState, newCursor)
            val updatedBuffer = addCharacterAnimationToBuffer(
              replacedBuffer.copy(viewport = updatedViewport),
              currentState,
              char,
              cursor.line,
              cursor.column
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case TabKey =>
            reduceTextEventForBuffer(InsertChar('\t'), buffer, paneId, currentState)

          case ReverseTabKey =>
            reduceTextEventForBuffer(DeleteBackward, buffer, paneId, currentState)

          case DeleteBackward =>
            buffer.primarySelection match
              case Some(selection) =>
                val updatedBuffer = deleteSelectedRange(buffer, selection, currentState)
                ReducerResult.noEffects(
                  currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer))
                )
              case None =>
                val offset = lineColumnToOffset(buffer.content, cursor.line, cursor.column)
                if offset > 0 then
                  val newContent = buffer.content.delete(offset - 1, offset)
                  val newCursor =
                    if cursor.column > 0 then cursor.copy(column = cursor.column - 1)
                    else if cursor.line > 0 then
                      val prevLineEnd = findLineEnd(buffer.content, cursor.line - 1)
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
                    viewport = updatedViewport
                  )
                  ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))
                else ReducerResult.noEffects(currentState)

          case DeleteForward =>
            buffer.primarySelection match
              case Some(selection) =>
                val updatedBuffer = deleteSelectedRange(buffer, selection, currentState)
                ReducerResult.noEffects(
                  currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer))
                )
              case None =>
                val offset = lineColumnToOffset(buffer.content, cursor.line, cursor.column)
                if offset < buffer.content.weight then
                  val newContent    = buffer.content.delete(offset, offset + 1)
                  val updatedBuffer = buffer.copy(
                    content = newContent,
                    isDirty = true,
                    isNewEmpty = false,
                    preferredColumn = Some(cursor.column),
                    preferredXPx = None
                  )
                  ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))
                else ReducerResult.noEffects(currentState)

          case MoveLeft =>
            val movementStart = selectionFocusOrCursor(buffer, cursor)
            val newCursor =
              if movementStart.column > 0 then movementStart.moveLeft
              else if movementStart.line > 0 then
                val prevLineEnd = findLineEnd(buffer.content, movementStart.line - 1)
                movementStart.copy(line = movementStart.line - 1, column = prevLineEnd)
              else movementStart
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
            val movementStart = selectionFocusOrCursor(buffer, cursor)
            val currentLineEnd = findLineEnd(buffer.content, movementStart.line)
            val newCursor =
              if movementStart.column < currentLineEnd then movementStart.moveRight
              else
                val totalLines = countLines(buffer.content)
                if movementStart.line < totalLines - 1 then movementStart.copy(line = movementStart.line + 1, column = 0)
                else movementStart
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
            val movementStart = selectionFocusOrCursor(buffer, cursor)
            val preferredColumn = buffer.preferredColumn.getOrElse(movementStart.column)
            val preferredXPx = preferredVisualXPx(buffer, currentState, movementStart)
            val newCursor = moveVerticalByLayout(
              movementStart,
              buffer,
              currentState,
              preferredXPx,
              direction = -1
            ).getOrElse(moveUpVisualLine(movementStart, buffer.content, effectivePanelWidth(currentState), preferredColumn))
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
            val movementStart = selectionFocusOrCursor(buffer, cursor)
            val preferredColumn = buffer.preferredColumn.getOrElse(movementStart.column)
            val preferredXPx = preferredVisualXPx(buffer, currentState, movementStart)
            val newCursor = moveVerticalByLayout(
              movementStart,
              buffer,
              currentState,
              preferredXPx,
              direction = 1
            ).getOrElse(moveDownVisualLine(movementStart, buffer.content, effectivePanelWidth(currentState), preferredColumn))
            val updatedViewport = adjustViewportForCursor(buffer, currentState, newCursor)
            val updatedBuffer = buffer.copy(
              cursors = newCursor :: buffer.cursors.tail,
              selection = None,
              preferredColumn = Some(preferredColumn),
              preferredXPx = Some(preferredXPx),
              viewport = updatedViewport
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case NewLine | Enter =>
            val updatedBuffer   = replaceSelectionOrInsert(buffer, cursor, "\n")
            val newCursor       = updatedBuffer.cursors.headOption.getOrElse(cursor)
            val updatedViewport = adjustViewportForCursor(buffer, currentState, newCursor)
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
            val lastLine    = math.max(0, countLines(buffer.content) - 1)
            val lastColumn  = findLineEnd(buffer.content, lastLine)
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
            val newViewport   = buffer.viewport.copy(topLine = newTopLine)
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
            val newViewport = buffer.viewport.copy(topLine = newTopLine)
            val updatedBuffer = buffer.copy(
              cursors = newCursor :: buffer.cursors.tail,
              preferredColumn = Some(newCursor.column),
              preferredXPx = None,
              viewport = newViewport
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case OpenGotoLine =>
            ModalStateReducer.show(Modal.GotoLine(""), currentState)

          case OpenFind =>
            ModalStateReducer.show(Modal.Find("", Nil, 0), currentState)

          case FindNext =>
            buffer.findState match
              case Some(FindState(query, resultLines, currentIndex)) if resultLines.nonEmpty =>
                val nextIndex   = (currentIndex + 1) % resultLines.size
                val targetLine  = resultLines(nextIndex)
                val halfVisible = buffer.viewport.visibleLines / 2
                val newTopLine  = math.max(0, targetLine - halfVisible)
                val updatedBuffer = buffer.copy(
                  cursors = List(CursorPosition(targetLine, 0)),
                  preferredColumn = Some(0),
                  preferredXPx = None,
                  viewport = buffer.viewport.copy(topLine = newTopLine),
                  findState = Some(FindState(query, resultLines, nextIndex))
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
            val (newContent, newCursor) =
              if cursor.line == 0 && countLines(buffer.content) == 1 then
                (buffer.content.delete(0, lineEnd), CursorPosition(0, 0))
              else if cursor.line < countLines(buffer.content) - 1 then
                // delete including the trailing newline
                (buffer.content.delete(lineStart, lineEnd + 1), CursorPosition(cursor.line, 0))
              else
                // last line — delete preceding newline
                (buffer.content.delete(lineStart - 1, lineEnd), CursorPosition(cursor.line - 1, 0))
            val updatedBuffer = buffer.copy(
              content = newContent,
              isDirty = true,
              isNewEmpty = false,
              cursors = newCursor :: buffer.cursors.tail,
              preferredColumn = Some(newCursor.column),
              preferredXPx = None,
              viewport = adjustViewportForCursor(buffer, currentState, newCursor)
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
          updateBufferInState(currentState, applyMultiSelectionReplacement(buffer, currentState, "\t"))
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
      case DeleteBackward | DeleteForward =>
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
      case MoveLeft | MoveRight | MoveUp | MoveDown | MoveToStart | MoveToEnd =>
        reduceMultiCursorTextEvent(event, collapseSelectionsToFocus(buffer, currentState), paneId, currentState)
      case _ =>
        reduceSingleCursorTextEvent(event, buffer.copy(selection = buffer.primarySelection, selections = Nil), paneId, currentState)

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
          updateBufferInState(currentState, applyMultiCursorInsertion(buffer, currentState, "\t"))
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
      case MoveLeft =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiCursorNavigation(buffer, currentState)(cursor =>
            moveCursorLeft(cursor, buffer.content)
          ))
        )
      case MoveRight =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiCursorNavigation(buffer, currentState)(cursor =>
            moveCursorRight(cursor, buffer.content)
          ))
        )
      case MoveToStart =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiCursorNavigation(buffer, currentState)(cursor =>
            cursor.copy(column = 0)
          ))
        )
      case MoveToEnd =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiCursorNavigation(buffer, currentState)(cursor =>
            cursor.copy(column = findLineEnd(buffer.content, cursor.line))
          ))
        )
      case MoveUp =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiCursorNavigation(buffer, currentState)(cursor =>
            moveMultiCursorVertical(cursor, buffer, currentState, direction = -1)
          ))
        )
      case MoveDown =>
        ReducerResult.noEffects(
          updateBufferInState(currentState, applyMultiCursorNavigation(buffer, currentState)(cursor =>
            moveMultiCursorVertical(cursor, buffer, currentState, direction = 1)
          ))
        )
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
        handleEventWithoutBuffer(InsertChar('\t'), paneId, pane, currentState)

      case _ =>
        ReducerResult.noEffects(currentState)

  private def lineColumnToOffset(rope: Rope, line: Int, column: Int): Int =
    val content = rope.collect()
    if content.isEmpty then math.min(column, 0)
    else
      case class LineState(currentLine: Int, offset: Int, i: Int)

      val finalState = (0 until content.length).foldLeft(LineState(0, 0, 0)) { (state, i) =>
        if state.currentLine >= line then state
        else if content(i) == '\n' then LineState(state.currentLine + 1, i + 1, i + 1)
        else state.copy(i = i + 1)
      }

      val result = if finalState.currentLine == line then finalState.offset + column else content.length
      math.min(result, rope.weight)

  private def findLineEnd(rope: Rope, line: Int): Int =
    val content = rope.collect()
    val lines   = content.split('\n')
    if line >= 0 && line < lines.length then lines(line).length else 0

  private def countLines(rope: Rope): Int =
    val content = rope.collect()
    if content.isEmpty then 1 else content.count(_ == '\n') + 1

  private case class CursorEntry(cursor: CursorPosition, offset: Int)
  private case class MultiCursorEdit(ownerIndex: Int, start: Int, end: Int, insertedText: String)

  private def applyMultiCursorInsertion(
    buffer: Buffer,
    currentState: AppState,
    insertedText: String
  ): Buffer =
    val entries = multiCursorEntries(buffer)
    val edits = entries.zipWithIndex.map { case (entry, index) =>
      MultiCursorEdit(index, entry.offset, entry.offset, insertedText)
    }
    applyTrackedEdits(buffer, currentState, entries.map(_.offset), edits)

  private def applyMultiCursorDeletion(
    buffer: Buffer,
    currentState: AppState,
    backward: Boolean
  ): Buffer =
    val entries = multiCursorEntries(buffer)
    val edits = entries.zipWithIndex.flatMap { case (entry, index) =>
      if backward then
        Option.when(entry.offset > 0)(MultiCursorEdit(index, entry.offset - 1, entry.offset, ""))
      else
        Option.when(entry.offset < buffer.content.weight)(MultiCursorEdit(index, entry.offset, entry.offset + 1, ""))
    }
    applyTrackedEdits(buffer, currentState, entries.map(_.offset), edits)

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
        preferredXPx = None
      )
      baseBuffer.copy(viewport = adjustViewportForCursor(baseBuffer, currentState, primaryCursor))

  private def applyMultiSelectionReplacement(
    buffer: Buffer,
    currentState: AppState,
    insertedText: String
  ): Buffer =
    val selections = activeSelections(buffer)
    val offsets    = selections.map(selectionStartOffset(buffer, _))
    val edits = selections.zipWithIndex.map { case (selection, index) =>
      MultiCursorEdit(
        index,
        selectionStartOffset(buffer, selection),
        selectionEndOffset(buffer, selection),
        insertedText
      )
    }
    applyTrackedEdits(buffer, currentState, offsets, edits)

  private def deleteSelectedRanges(
    buffer: Buffer,
    currentState: AppState
  ): Buffer =
    val selections = activeSelections(buffer)
    val offsets    = selections.map(selectionStartOffset(buffer, _))
    val edits = selections.zipWithIndex.map { case (selection, index) =>
      MultiCursorEdit(index, selectionStartOffset(buffer, selection), selectionEndOffset(buffer, selection), "")
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
      preferredXPx = None
    )
    baseBuffer.copy(viewport = adjustViewportForCursor(baseBuffer, currentState, primaryCursor))

  private def multiCursorEntries(buffer: Buffer): List[CursorEntry] =
    buffer.cursors
      .distinct
      .map(cursor => CursorEntry(cursor, lineColumnToOffset(buffer.content, cursor.line, cursor.column)))
      .sortBy(_.offset)

  private def updateBufferInState(state: AppState, buffer: Buffer): AppState =
    state.copy(buffers = state.buffers + (buffer.id -> buffer))

  private def offsetToCursorPosition(content: Rope, offset: Int): CursorPosition =
    val clamped = math.max(0, math.min(offset, content.weight))
    val text    = content.collect()
    val scanned = text.take(clamped).foldLeft(CursorPosition(0, 0)) { (cursor, char) =>
      if char == '\n' then CursorPosition(cursor.line + 1, 0)
      else cursor.copy(column = cursor.column + 1)
    }
    scanned

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
    direction: Int
  ): CursorPosition =
    val preferredColumn = cursor.column
    val preferredXPx = measuredCursorXPx(buffer, currentState, cursor)
    moveVerticalByLayout(
      cursor,
      buffer,
      currentState,
      preferredXPx,
      direction
    ).getOrElse {
      if direction < 0 then moveUpVisualLine(cursor, buffer.content, effectivePanelWidth(currentState), preferredColumn)
      else moveDownVisualLine(cursor, buffer.content, effectivePanelWidth(currentState), preferredColumn)
    }

  private def adjustViewportForCursor(
    buffer: Buffer,
    currentState: AppState,
    cursor: CursorPosition
  ): Viewport =
    val viewport           = buffer.viewport
    val halfVisibleLines   = viewport.visibleLines / 2
    val targetTopLine      = cursor.line - halfVisibleLines
    val clampedTopLine     = math.max(0, targetTopLine)
    val font               = previewFontForBuffer(buffer, currentState.config.fontConfig)
    val visibleWidthPx     = effectivePanelWidth(currentState) * CellMetrics.fromFont(font).charWidth
    val lineText           = buffer.content.getLine(cursor.line).getOrElse("")
    val clampedLeftColumn  =
      TextLayoutSnapshot.leftColumnForCursorVisibility(lineText, cursor.column, visibleWidthPx, font)

    viewport.copy(
      topLine = clampedTopLine,
      leftColumn = clampedLeftColumn
    )

  private def moveUpVisualLine(
    cursor: CursorPosition,
    rope: Rope,
    panelWidth: Int,
    preferredColumn: Int
  ): CursorPosition =
    if cursor.line == 0 && cursor.column < panelWidth then cursor.copy(column = 0)
    else
      val currentLineContent        = rope.getLine(cursor.line).getOrElse("")
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

  private def effectivePanelWidth(currentState: AppState): Int =
    val viewportSize = currentState.viewportSize.getOrElse(com.serenity.ui.layout.ViewportSize(80, 24))
    val layout       = com.serenity.ui.layout.LayoutEngine.calculateLayout(currentState, viewportSize)
    layout.editorPanelRect.width

  private def preferredVisualXPx(buffer: Buffer, currentState: AppState, cursor: CursorPosition): Float =
    buffer.preferredXPx.getOrElse {
      measuredCursorXPx(buffer, currentState, cursor)
    }

  private def measuredCursorXPx(buffer: Buffer, currentState: AppState, cursor: CursorPosition): Float =
    val font         = previewFontForBuffer(buffer, currentState.config.fontConfig)
    val metrics      = CellMetrics.fromFont(font)
    val panelWidthPx = effectivePanelWidth(currentState) * metrics.charWidth
    val snapshot     = TextLayoutSnapshot.fromBuffer(buffer.copy(viewport = buffer.viewport.copy(leftColumn = 0)), panelWidthPx, font)
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
    val snapshot     = TextLayoutSnapshot.fromBuffer(buffer.copy(viewport = buffer.viewport.copy(leftColumn = 0)), panelWidthPx, font)
    snapshot.moveVertical(cursor, direction, preferredXPx)

  private def previewFontForBuffer(
    buffer: Buffer,
    config: com.serenity.ui.fonts.FontLoader.FontConfig
  ): java.awt.Font =
    buffer.language match
      case Some(LanguageId.Markdown) => FontLoader.previewTextFont(config)
      case _                         => FontLoader.previewCodeFont(config)

  private def replaceSelectionOrInsert(buffer: Buffer, cursor: CursorPosition, text: String)(using
    balance: com.serenity.rope.Balance
  ): Buffer =
    val (baseContent, insertionStart) = buffer.primarySelection match
      case Some(selection) =>
        val startOffset = selectionStartOffset(buffer, selection)
        val endOffset   = selectionEndOffset(buffer, selection)
        (
          buffer.content.delete(startOffset, endOffset),
          selection.start
        )
      case None =>
        (
          buffer.content,
          cursor
        )

    val startOffset = lineColumnToOffset(baseContent, insertionStart.line, insertionStart.column)
    val newContent  = baseContent.insert(startOffset, text)
    val newCursor   = cursorAfterInsertion(insertionStart, text)

    buffer.copy(
      content = newContent,
      isDirty = true,
      isNewEmpty = false,
      cursors = newCursor :: buffer.cursors.tail,
      selection = None,
      selections = Nil,
      preferredColumn = Some(newCursor.column),
      preferredXPx = None
    )

  private def deleteSelectedRange(
    buffer: Buffer,
    selection: Selection,
    currentState: AppState
  ): Buffer =
    val startOffset = selectionStartOffset(buffer, selection)
    val endOffset   = selectionEndOffset(buffer, selection)
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
      preferredXPx = None
    )
    val updatedViewport = adjustViewportForCursor(baseBuffer, currentState, newCursor)

    baseBuffer.copy(viewport = updatedViewport)

  private def selectedText(buffer: Buffer, selection: Selection): String =
    val startOffset = selectionStartOffset(buffer, selection)
    val endOffset   = selectionEndOffset(buffer, selection)
    buffer.content.collect().slice(startOffset, endOffset)

  private def activeSelections(buffer: Buffer): List[Selection] =
    buffer.allSelections
      .distinct
      .sortBy(selection =>
        (
          selection.start.line,
          selection.start.column,
          selection.end.line,
          selection.end.column
        )
      )

  private def selectedTexts(buffer: Buffer): List[String] =
    activeSelections(buffer).map(selectedText(buffer, _))

  private def selectionStartOffset(buffer: Buffer, selection: Selection): Int =
    lineColumnToOffset(buffer.content, selection.start.line, selection.start.column)

  private def selectionEndOffset(buffer: Buffer, selection: Selection): Int =
    lineColumnToOffset(buffer.content, selection.end.line, selection.end.column)

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
