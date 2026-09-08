package com.serenity.state.reducers

import com.serenity.rope.*
import com.serenity.state.models.*

/** Primitives shared by every family of [[EditorEventReducer]] event handling: the per-event context bundle, tab
  * insertion text, line-offset/line-end resolution and the Home/End/visual-line-navigation targets. Extracted from
  * `EditorEventReducer` (which grew past its 600-line target) rather than owned by any one family, since more than one
  * family depends on these staying in one place instead of reaching back into the dispatch file that used to hold them.
  */
private[reducers] object EditorCursorSupport:

  val TabInsertion = "    "

  /** Bundles the per-event state `EditorEventReducer.reduceCursorsTextEvent` computes once (the seeded buffer/state and
    * the cardinality flags every family function below branches on) so each family function takes one parameter instead
    * of five.
    */
  final case class CursorEventContext(
      buffer: Buffer,
      head: CursorPosition,
      hasSelection: Boolean,
      isMulti: Boolean,
      currentState: AppState,
      paneId: PaneId
  )

  def lineColumnToOffset(rope: Rope, line: Int, column: Int): Int =
    rope.lineColumnToOffset(line, column)

  def findLineEnd(content: Rope, line: Int): Int =
    val lineStart = lineColumnToOffset(content, line, 0)
    val lineEnd   = lineColumnToOffset(content, line, Int.MaxValue)
    lineEnd - lineStart

  /** Whether Home/End (and Up/Down, via `EditorVerticalNavigationReducer`) should move by visual row rather than
    * logical line -- word wrap has to be on for "visual row" to mean anything different from the logical line at all,
    * and the setting is independently toggleable on top of it.
    */
  def useVisualLineNavigation(state: AppState): Boolean =
    state.persisted.config.surfaceConfig.wordWrapEnabled &&
      state.persisted.config.surfaceConfig.visualLineCursorNavigation

  /** Home's landing column: the start of the cursor's current *visual* row when visual-line navigation applies,
    * otherwise column 0 of the logical line (`MoveToStartOfFile`-style callers that want the true buffer start
    * regardless of wrapping use `MoveToStartOfFile`, not this). Falls back to the logical start if no geometry is
    * available for this pane (e.g. no buffer content yet) even when the setting is on, matching `verticalTarget`'s own
    * `fallbackVerticalMove` fallback pattern for the same situation.
    */
  def homeTarget(state: AppState, paneId: PaneId, cursor: CursorPosition): CursorPosition =
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
  def endTarget(
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

  def countLines(rope: Rope): Int =
    rope.lineCount
