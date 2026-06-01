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
    buffer.cursors.headOption match
      case Some(cursor) =>
        event match
          case InsertChar(char) =>
            val replacedBuffer  = replaceSelectionOrInsert(buffer, cursor, char.toString)
            val newCursor       = replacedBuffer.cursors.headOption.getOrElse(cursor)
            val updatedViewport = adjustViewportForCursor(buffer.viewport, newCursor)
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
            buffer.selection match
              case Some(selection) =>
                val updatedBuffer = deleteSelectedRange(buffer, selection)
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
                  val updatedViewport = adjustViewportForCursor(buffer.viewport, newCursor)
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
            buffer.selection match
              case Some(selection) =>
                val updatedBuffer = deleteSelectedRange(buffer, selection)
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
            val updatedViewport = adjustViewportForCursor(buffer.viewport, newCursor)
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
            val updatedViewport = adjustViewportForCursor(buffer.viewport, newCursor)
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
            val updatedViewport = adjustViewportForCursor(buffer.viewport, newCursor)
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
            val updatedViewport = adjustViewportForCursor(buffer.viewport, newCursor)
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
            val updatedViewport = adjustViewportForCursor(buffer.viewport, newCursor)
            val updatedBufferWithViewport = updatedBuffer.copy(viewport = updatedViewport)
            ReducerResult.noEffects(
              currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBufferWithViewport))
            )

          case MoveToStart =>
            val newCursor       = cursor.copy(column = 0)
            val updatedViewport = adjustViewportForCursor(buffer.viewport, newCursor)
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
            val updatedViewport = adjustViewportForCursor(buffer.viewport, newCursor)
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
              viewport = adjustViewportForCursor(buffer.viewport, endCursor)
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
            currentState.findState match
              case Some(FindState(query, resultLines, currentIndex)) if resultLines.nonEmpty =>
                val nextIndex   = (currentIndex + 1) % resultLines.size
                val targetLine  = resultLines(nextIndex)
                val halfVisible = buffer.viewport.visibleLines / 2
                val newTopLine  = math.max(0, targetLine - halfVisible)
                val updatedBuffer = buffer.copy(
                  cursors = List(CursorPosition(targetLine, 0)),
                  preferredColumn = Some(0),
                  preferredXPx = None,
                  viewport = buffer.viewport.copy(topLine = newTopLine)
                )
                ReducerResult.noEffects(
                  currentState.copy(
                    findState = Some(FindState(query, resultLines, nextIndex)),
                    buffers = currentState.buffers + (buffer.id -> updatedBuffer)
                  )
                )
              case _ =>
                ReducerResult.noEffects(currentState)

          case Copy if buffer.selection.isDefined =>
            val selection = buffer.selection.get
            ReducerResult.noEffects(
              currentState.copy(clipboard = Some(selectedText(buffer, selection)))
            )

          case Cut if buffer.selection.isDefined =>
            val selection     = buffer.selection.get
            val updatedBuffer = deleteSelectedRange(buffer, selection)
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
              viewport = adjustViewportForCursor(buffer.viewport, newCursor)
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
                  viewport = adjustViewportForCursor(buffer.viewport, newCursor)
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

  private def adjustViewportForCursor(viewport: Viewport, cursor: CursorPosition): Viewport =
    val halfVisibleLines   = viewport.visibleLines / 2
    val targetTopLine      = cursor.line - halfVisibleLines
    val clampedTopLine     = math.max(0, targetTopLine)
    val halfVisibleColumns = viewport.visibleColumns / 2
    val targetLeftColumn   = cursor.column - halfVisibleColumns
    val clampedLeftColumn  = math.max(0, targetLeftColumn)

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
    buffer.selection.map(_.focus).getOrElse(cursor)

  private def effectivePanelWidth(currentState: AppState): Int =
    val viewportSize = currentState.viewportSize.getOrElse(com.serenity.ui.layout.ViewportSize(80, 24))
    val layout       = com.serenity.ui.layout.LayoutEngine.calculateLayout(currentState, viewportSize)
    layout.editorPanelRect.width

  private def preferredVisualXPx(buffer: Buffer, currentState: AppState, cursor: CursorPosition): Float =
    buffer.preferredXPx.getOrElse {
      val font         = previewFontForBuffer(buffer, currentState.config.fontConfig)
      val panelWidthPx = effectivePanelWidth(currentState) * CellMetrics.fromFont(font).charWidth
      val snapshot     = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx, font)
      snapshot.xPxForCursor(cursor).getOrElse(cursor.column.toFloat * CellMetrics.fromFont(font).charWidth.toFloat)
    }

  private def moveVerticalByLayout(
    cursor: CursorPosition,
    buffer: Buffer,
    currentState: AppState,
    preferredXPx: Float,
    direction: Int
  ): Option[CursorPosition] =
    val font         = previewFontForBuffer(buffer, currentState.config.fontConfig)
    val panelWidthPx = effectivePanelWidth(currentState) * CellMetrics.fromFont(font).charWidth
    val snapshot     = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx, font)
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
    val (baseContent, insertionStart) = buffer.selection match
      case Some(selection) =>
        val startOffset = lineColumnToOffset(buffer.content, selection.start.line, selection.start.column)
        val endOffset   = lineColumnToOffset(buffer.content, selection.end.line, selection.end.column)
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
      preferredColumn = Some(newCursor.column),
      preferredXPx = None
    )

  private def deleteSelectedRange(buffer: Buffer, selection: Selection): Buffer =
    val startOffset = lineColumnToOffset(buffer.content, selection.start.line, selection.start.column)
    val endOffset   = lineColumnToOffset(buffer.content, selection.end.line, selection.end.column)
    val newContent  = buffer.content.delete(startOffset, endOffset)
    val newCursor   = selection.start

    buffer.copy(
      content = newContent,
      isDirty = true,
      isNewEmpty = false,
      cursors = newCursor :: buffer.cursors.tail,
      selection = None,
      preferredColumn = Some(newCursor.column),
      preferredXPx = None,
      viewport = adjustViewportForCursor(buffer.viewport, newCursor)
    )

  private def selectedText(buffer: Buffer, selection: Selection): String =
    val startOffset = lineColumnToOffset(buffer.content, selection.start.line, selection.start.column)
    val endOffset   = lineColumnToOffset(buffer.content, selection.end.line, selection.end.column)
    buffer.content.collect().slice(startOffset, endOffset)

  private def cursorAfterInsertion(start: CursorPosition, insertedText: String): CursorPosition =
    val lines = insertedText.split("\n", -1)
    if lines.length == 1 then start.copy(column = start.column + insertedText.length)
    else CursorPosition(start.line + lines.length - 1, lines.last.length)
