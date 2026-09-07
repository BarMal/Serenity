package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.*
import com.serenity.state.models.*

/** Up/Down (and their selection-extending forms), split out of [[EditorEventReducer]] because they are the one editor
  * reduction whose result depends on measured text geometry -- the geometry the effect boundary produced for this pane,
  * passed in rather than reached for. Routing mirrors the horizontal event dispatch in
  * `EditorEventReducer.reduceCursorsTextEvent`: extend-selection stays single-cursor, multi-selection collapses to its
  * focuses, and a multi-cursor buffer (or one carrying in-flight vertical state) moves every cursor.
  */
private[reducers] object EditorVerticalNavigationReducer:
  import EditorCursorMovement.*

  def reduce(
    event: VerticalNavigationEvent,
    paneId: PaneId,
    currentState: AppState,
    geometry: EditorGeometry
  ): ReducerResult =
    currentState.persisted.layout.editorPanes
      .get(paneId)
      .flatMap(_.bufferId)
      .flatMap(currentState.persisted.buffers.get) match
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

  final private case class MultiCursorVerticalState(cursor: CursorPosition, preferredColumn: Int, preferredXPx: Float)

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
    val primaryCursor = visibleCursors.primaryCursor
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

  private def moveMultiCursorVertical(
    cursor: CursorPosition,
    buffer: Buffer,
    currentState: AppState,
    geometry: EditorGeometry,
    preferredColumn: Int,
    preferredXPx: Float,
    direction: Int
  ): CursorPosition =
    val useVisualLineNavigation =
      currentState.persisted.config.surfaceConfig.wordWrapEnabled &&
        currentState.persisted.config.surfaceConfig.visualLineCursorNavigation
    measuredVerticalMoveBySnapshot(
      useVisualLineNavigation,
      cursor,
      geometry.navigation,
      preferredXPx,
      direction
    )
      .getOrElse(
        fallbackVerticalMove(
          cursor,
          buffer,
          geometry,
          useVisualLineNavigation,
          preferredColumn,
          direction
        )
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

  private def verticalTarget(currentState: AppState, geometry: EditorGeometry, direction: Int)(
    buffer: Buffer,
    from: CursorPosition
  ): CursorTarget =
    val preferredColumn = buffer.editing.preferredColumn.getOrElse(from.column)
    val preferredXPx    = buffer.editing.preferredXPx.getOrElse(measuredCursorXPxFrom(geometry, from))
    // Visual-row movement only makes sense with wrap on, and is independently toggleable on top of it (default on,
    // matching wrap-follows-wrap behaviour before this setting existed).
    val useVisualLineNavigation =
      currentState.persisted.config.surfaceConfig.wordWrapEnabled &&
        currentState.persisted.config.surfaceConfig.visualLineCursorNavigation
    val landed =
      measuredVerticalMoveBySnapshot(
        useVisualLineNavigation,
        from,
        geometry.navigation,
        preferredXPx,
        direction
      )
        .getOrElse(
          fallbackVerticalMove(
            from,
            buffer,
            geometry,
            useVisualLineNavigation,
            preferredColumn,
            direction
          )
        )

    CursorTarget(landed, preferredColumn, Some(preferredXPx))
