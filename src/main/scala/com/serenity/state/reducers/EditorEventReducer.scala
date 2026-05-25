package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Rope
import com.serenity.state.models.*

object EditorEventReducer:

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
            val offset          = lineColumnToOffset(buffer.content, cursor.line, cursor.column)
            val newContent      = buffer.content.insert(offset, char.toString)
            val newCursor       = cursor.copy(column = cursor.column + 1)
            val updatedViewport = adjustViewportForCursor(buffer.viewport, newCursor)
            val updatedBuffer = addCharacterAnimationToBuffer(
              buffer.copy(
                content = newContent,
                isDirty = true,
                isNewEmpty = false,
                cursors = newCursor :: buffer.cursors.tail,
                viewport = updatedViewport
              ),
              currentState,
              char,
              cursor.line,
              cursor.column
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case DeleteBackward =>
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
                viewport = updatedViewport
              )
              ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))
            else ReducerResult.noEffects(currentState)

          case DeleteForward =>
            val offset = lineColumnToOffset(buffer.content, cursor.line, cursor.column)
            if offset < buffer.content.weight then
              val newContent    = buffer.content.delete(offset, offset + 1)
              val updatedBuffer = buffer.copy(content = newContent, isDirty = true, isNewEmpty = false)
              ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))
            else ReducerResult.noEffects(currentState)

          case MoveLeft =>
            val newCursor =
              if cursor.column > 0 then cursor.moveLeft
              else if cursor.line > 0 then
                val prevLineEnd = findLineEnd(buffer.content, cursor.line - 1)
                cursor.copy(line = cursor.line - 1, column = prevLineEnd)
              else cursor
            val updatedViewport = adjustViewportForCursor(buffer.viewport, newCursor)
            val updatedBuffer = buffer.copy(
              cursors = newCursor :: buffer.cursors.tail,
              viewport = updatedViewport
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case MoveRight =>
            val currentLineEnd = findLineEnd(buffer.content, cursor.line)
            val newCursor =
              if cursor.column < currentLineEnd then cursor.moveRight
              else
                val totalLines = countLines(buffer.content)
                if cursor.line < totalLines - 1 then cursor.copy(line = cursor.line + 1, column = 0)
                else cursor
            val updatedViewport = adjustViewportForCursor(buffer.viewport, newCursor)
            val updatedBuffer = buffer.copy(
              cursors = newCursor :: buffer.cursors.tail,
              viewport = updatedViewport
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case MoveUp =>
            val terminalSize = currentState.terminalSize.getOrElse(com.serenity.ui.layout.TerminalSize(80, 24))
            val layout       = com.serenity.ui.layout.LayoutEngine.calculateLayout(currentState, terminalSize)
            val panelWidth   = layout.editorPanelRect.width
            val newCursor       = moveUpVisualLine(cursor, buffer.content, panelWidth)
            val updatedViewport = adjustViewportForCursor(buffer.viewport, newCursor)
            val updatedBuffer = buffer.copy(
              cursors = newCursor :: buffer.cursors.tail,
              viewport = updatedViewport
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case MoveDown =>
            val terminalSize = currentState.terminalSize.getOrElse(com.serenity.ui.layout.TerminalSize(80, 24))
            val layout       = com.serenity.ui.layout.LayoutEngine.calculateLayout(currentState, terminalSize)
            val panelWidth   = layout.editorPanelRect.width
            val newCursor       = moveDownVisualLine(cursor, buffer.content, panelWidth)
            val updatedViewport = adjustViewportForCursor(buffer.viewport, newCursor)
            val updatedBuffer = buffer.copy(
              cursors = newCursor :: buffer.cursors.tail,
              viewport = updatedViewport
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case NewLine | Enter =>
            val offset          = lineColumnToOffset(buffer.content, cursor.line, cursor.column)
            val newContent      = buffer.content.insert(offset, "\n")
            val newCursor       = cursor.copy(line = cursor.line + 1, column = 0)
            val updatedViewport = adjustViewportForCursor(buffer.viewport, newCursor)
            val updatedBuffer = buffer.copy(
              content = newContent,
              isDirty = true,
              isNewEmpty = false,
              cursors = newCursor :: buffer.cursors.tail,
              viewport = updatedViewport
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case MoveToStart =>
            val newCursor       = cursor.copy(column = 0)
            val updatedViewport = adjustViewportForCursor(buffer.viewport, newCursor)
            val updatedBuffer = buffer.copy(
              cursors = newCursor :: buffer.cursors.tail,
              viewport = updatedViewport
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case MoveToEnd =>
            val lineEnd         = findLineEnd(buffer.content, cursor.line)
            val newCursor       = cursor.copy(column = lineEnd)
            val updatedViewport = adjustViewportForCursor(buffer.viewport, newCursor)
            val updatedBuffer = buffer.copy(
              cursors = newCursor :: buffer.cursors.tail,
              viewport = updatedViewport
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
              viewport = newViewport
            )
            ReducerResult.noEffects(currentState.copy(buffers = currentState.buffers + (buffer.id -> updatedBuffer)))

          case OpenGotoLine =>
            ReducerResult.noEffects(
              currentState.copy(
                modal = Some(Modal.GotoLine("")),
                focus = Focus.Modal(ModalType.GotoLine)
              )
            )

          case OpenFind =>
            ReducerResult.noEffects(
              currentState.copy(
                modal = Some(Modal.Find("", Nil, 0)),
                focus = Focus.Modal(ModalType.Find)
              )
            )

          case FindNext =>
            currentState.findState match
              case Some(FindState(query, resultLines, currentIndex)) if resultLines.nonEmpty =>
                val nextIndex   = (currentIndex + 1) % resultLines.size
                val targetLine  = resultLines(nextIndex)
                val halfVisible = buffer.viewport.visibleLines / 2
                val newTopLine  = math.max(0, targetLine - halfVisible)
                val updatedBuffer = buffer.copy(
                  cursors = List(CursorPosition(targetLine, 0)),
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

  private def moveUpVisualLine(cursor: CursorPosition, rope: Rope, panelWidth: Int): CursorPosition =
    if cursor.line == 0 && cursor.column < panelWidth then cursor.copy(column = 0)
    else
      val currentLineContent        = rope.getLine(cursor.line).getOrElse("")
      val currentVisualLineInBuffer = cursor.column / panelWidth

      if currentVisualLineInBuffer > 0 then
        val newColumn = cursor.column - panelWidth
        cursor.copy(column = math.max(0, newColumn))
      else if cursor.line > 0 then
        val prevLineContent = rope.getLine(cursor.line - 1).getOrElse("")
        if prevLineContent.length <= panelWidth then
          val newColumn = math.min(cursor.column, prevLineContent.length)
          cursor.copy(line = cursor.line - 1, column = newColumn)
        else
          val lastVisualLineInPrev   = (prevLineContent.length - 1) / panelWidth
          val baseColumnInLastVisual = lastVisualLineInPrev * panelWidth
          val newColumn = math.min(baseColumnInLastVisual + (cursor.column % panelWidth), prevLineContent.length)
          cursor.copy(line = cursor.line - 1, column = newColumn)
      else cursor

  private def moveDownVisualLine(cursor: CursorPosition, rope: Rope, panelWidth: Int): CursorPosition =
    val currentLineContent        = rope.getLine(cursor.line).getOrElse("")
    val currentVisualLineInBuffer = cursor.column / panelWidth
    val totalVisualLinesInCurrent = math.max(1, (currentLineContent.length + panelWidth - 1) / panelWidth)

    if currentVisualLineInBuffer < totalVisualLinesInCurrent - 1 then
      val newColumn = cursor.column + panelWidth
      cursor.copy(column = math.min(newColumn, currentLineContent.length))
    else if cursor.line < rope.lineCount - 1 then
      val nextLineContent      = rope.getLine(cursor.line + 1).getOrElse("")
      val targetColumnInVisual = cursor.column % panelWidth
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
        val durationMs = animConfig.totalDuration.toMillis.toInt
        val animatedChar = com.serenity.animation.AnimatedCharacter.createFadeAnimation(
          char,
          state.theme.backgroundColor,
          state.theme.foregroundColor,
          durationMs,
          16
        )
        val updatedAnimations = buffer.animations.copy(
          animations = buffer.animations.animations +
            (com.serenity.animation.CharacterKey(cursorColumn, cursorLine) -> animatedChar)
        )
        buffer.copy(animations = updatedAnimations)
      case None =>
        buffer

