package com.serenity.state.manager

import com.serenity.animation.Interpolator.given
import com.serenity.animation.{TransitionDirection, Tween}
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot}

/** Java2D/font measurement for cursor-visibility scrolling belongs at the effect boundary, not in a reducer -- a
  * reducer runs mid-edit against content the effect boundary has not seen yet. `adjustForCursor` is the shared
  * measurement (also used directly by mouse-click and vertical-navigation effect handlers); `ensureVisibleCursors` is
  * the boundary pass that re-applies it after a pure reduce, for every buffer whose primary cursor moved.
  */
object CursorViewport:

  def ensureVisibleCursors(before: AppState, after: AppState): AppState =
    after.persisted.buffers.foldLeft(after) {
      case (state, (bufferId, buffer)) =>
        val beforeBuffer = before.persisted.buffers.get(bufferId)
        val headMoved =
          beforeBuffer.exists(_.editing.cursorPositions.headOption != buffer.editing.cursorPositions.headOption)
        val stateAfterViewport =
          if !headMoved then state
          else
            buffer.editing.cursorPositions.headOption match
              case Some(cursor) =>
                val surfaceConfig    = state.persisted.config.surfaceConfig
                val columnModeActive = surfaceConfig.columnModeEnabled && surfaceConfig.wordWrapEnabled
                val placement =
                  if columnModeActive then adjustForCursorColumnMode(buffer, state, cursor)
                  else adjustForCursor(buffer, state, cursor)
                val updatedBuffer = buffer.copy(viewport = placement)
                val updatedState = state.copy(persisted =
                  state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> updatedBuffer))
                )
                if columnModeActive then seedColumnTransition(bufferId, buffer.viewport, placement, updatedState)
                else updatedState
              case None => state
        val stateAfterGlide = beforeBuffer.fold(stateAfterViewport)(seedCursorGlide(bufferId, _, stateAfterViewport))
        beforeBuffer.fold(stateAfterGlide)(seedSelectionGeometry(bufferId, _, stateAfterGlide))
    }

  /** Caret-glide (issue #1085 phase 2): seeds/retargets `Cursor.glide` for every cursor in `bufferId` whose position
    * changed between `beforeBuffer` and the buffer now in `state` -- any change (typing, navigation, mouse click,
    * search jump), matched positionally the same way `RendererCursorGlyphs` already indexes cursors for painting, not
    * only when the primary cursor moves (unlike the viewport placement above, which only re-centres on the primary
    * cursor). Gated by the `Cursor` motion family (`AppConfig.scaledCursorGlideAnimation` already folds in
    * accessibility and the `Reduced` preset) and by `state.runtime.isTuiMode`: TUI's caret snaps instantly, since a
    * terminal cursor can't glide sub-cell (`RendererCursorOverlay.presentHardwareCursor`'s existing GUI/TUI split).
    *
    * Retargets an in-flight glide (`Tween.retarget`) rather than reseeding at progress zero when a cursor moves again
    * before its previous glide finishes -- the same jump-cut fix `seedColumnTransition` already applies to the
    * column-sweep tween.
    */
  private def seedCursorGlide(bufferId: BufferId, beforeBuffer: Buffer, state: AppState): AppState =
    if state.runtime.isTuiMode then state
    else
      state.persisted.config.scaledCursorGlideAnimation match
        case None => state
        case Some(animation) =>
          state.persisted.buffers.get(bufferId) match
            case None => state
            case Some(afterBuffer) =>
              val beforeCursors = beforeBuffer.editing.cursors.toList
              val updatedCursors = afterBuffer.editing.cursors.zipWithIndex.map {
                case (cursor, index) =>
                  beforeCursors.lift(index) match
                    case Some(previous) if previous.position != cursor.position =>
                      val newPixel =
                        CursorGlideGeometry.paneRelativePosition(afterBuffer, state.persisted.config, cursor.position)
                      // The reducer that moved this cursor typically rebuilds `EditingState` from bare `CursorPosition`s
                      // (`EditingState.apply`/`Cursor.apply(position)`), which wipes `cursor.glide` back to `None` before
                      // this ever runs -- so whether a glide was already in flight has to be read from `previous` (the
                      // pre-reducer `before` state this pass diffs against, which still carries whatever the last
                      // `seedCursorGlide` call set), never from `cursor` itself.
                      val tween = previous.glide.filterNot(_.isComplete) match
                        case Some(existing) => existing.retarget(newPixel)
                        case None =>
                          val oldPixel =
                            CursorGlideGeometry.paneRelativePosition(
                              afterBuffer,
                              state.persisted.config,
                              previous.position
                            )
                          Tween(start = oldPixel, end = newPixel, curve = animation.curve, steps = animation.steps)
                      cursor.copy(glide = Some(tween))
                    case _ => cursor
              }
              if updatedCursors == afterBuffer.editing.cursors then state
              else
                val updatedBuffer = afterBuffer.withCursorList(updatedCursors)
                state.copy(persisted =
                  state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> updatedBuffer))
                )

  /** Selection grow/settle (issue #1085 phase 3): seeds/retargets `Cursor.selectionGeometry` for every cursor in
    * `bufferId` whose selection changed between `beforeBuffer` and the buffer now in `state` -- any change (extend,
    * shrink, create, clear), matched positionally the same way `seedCursorGlide` above does. Gated by the
    * `SelectionGeometry` motion family (`AppConfig.scaledSelectionGeometryAnimation` already folds in accessibility and
    * the `Reduced` preset). Unlike `seedCursorGlide`, this runs regardless of `state.runtime.isTuiMode` --
    * `SelectionGeometryState`'s column-granular model serves both the GUI's measured painting and TUI's cell painting
    * (see its own doc comment), so there is nothing GUI-only about it here.
    *
    * Retargets an in-flight geometry (`SelectionGeometryState.diff`'s own `Tween.retarget` handling) rather than
    * reseeding at progress zero when a selection changes again before its previous animation finishes -- the same
    * jump-cut fix `seedCursorGlide`/`seedColumnTransition` already apply to their own tweens.
    */
  private def seedSelectionGeometry(bufferId: BufferId, beforeBuffer: Buffer, state: AppState): AppState =
    state.persisted.config.scaledSelectionGeometryAnimation match
      case None => state
      case Some(animation) =>
        state.persisted.buffers.get(bufferId) match
          case None => state
          case Some(afterBuffer) =>
            val beforeCursors = beforeBuffer.editing.cursors.toList
            val updatedCursors = afterBuffer.editing.cursors.zipWithIndex.map {
              case (cursor, index) =>
                beforeCursors.lift(index) match
                  case Some(previous) if previous.selection != cursor.selection =>
                    val beforeRects = previous.selection
                      .map(SelectionGeometry.rectsForSelection(afterBuffer, state.persisted.config, _))
                      .getOrElse(Map.empty)
                    val afterRects = cursor.selection
                      .map(SelectionGeometry.rectsForSelection(afterBuffer, state.persisted.config, _))
                      .getOrElse(Map.empty)
                    val geometry = SelectionGeometryState.diff(
                      previous.selectionGeometry.filterNot(_.isComplete),
                      beforeRects,
                      afterRects,
                      animation.curve,
                      animation.steps
                    )
                    cursor.copy(selectionGeometry = geometry)
                  case _ => cursor
            }
            if updatedCursors == afterBuffer.editing.cursors then state
            else
              val updatedBuffer = afterBuffer.withCursorList(updatedCursors)
              state.copy(persisted =
                state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> updatedBuffer))
              )

  /** Column-based document layout (issue #1338, Phase 1 animation): seeds `Runtime.columnTransitions` whenever
    * [[adjustForCursorColumnMode]] actually moved which column is showing -- whatever moved the cursor there, not only
    * `ColumnLeft`/`ColumnRight`, since this effect boundary has no narrower notion of "why" the cursor moved than any
    * other placement it applies. Gated by the `ColumnTransitions` motion family
    * (`AppConfig.scaledColumnTransitionAnimation` already folds in accessibility and the `Reduced` preset): a `None`
    * there means "snap instantly," so no transition is recorded at all -- the viewport still moves to the new column,
    * there is just nothing to animate between.
    *
    * If a transition for this buffer is already in flight, this retargets it (issue #1083's `Tween.retarget`) rather
    * than reseeding at progress 0: the previous behaviour snapped the sweep back to its start whenever the cursor
    * crossed another column boundary before the current sweep finished, a visible jump-cut. Retargeting keeps the
    * in-flight transition's own `direction`/`previousTop*` -- the column being swept away is still the same one -- and
    * just lets the sweep continue smoothly the rest of the way to full progress.
    */
  private def seedColumnTransition(
    bufferId: BufferId,
    previousViewport: Viewport,
    placedViewport: Viewport,
    state: AppState
  ): AppState =
    val columnChanged =
      previousViewport.topLine != placedViewport.topLine || previousViewport.topVisualLine != placedViewport.topVisualLine
    if !columnChanged then state
    else
      state.persisted.config.scaledColumnTransitionAnimation match
        case None => state
        case Some(animation) =>
          val inFlight = state.runtime.columnTransitions.get(bufferId).filterNot(_.isComplete)
          val transition = inFlight match
            case Some(existing) => existing.retarget
            case None =>
              val movedForward =
                placedViewport.topLine > previousViewport.topLine ||
                  (placedViewport.topLine == previousViewport.topLine &&
                    placedViewport.topVisualLine > previousViewport.topVisualLine)
              ColumnTransitionState.seeded(
                steps = animation.steps,
                curve = animation.curve,
                direction = if movedForward then TransitionDirection.RightToLeft else TransitionDirection.LeftToRight,
                previousTopLine = previousViewport.topLine,
                previousTopVisualLine = previousViewport.topVisualLine
              )
          state.copy(runtime =
            state.runtime.copy(columnTransitions = state.runtime.columnTransitions.updated(bufferId, transition))
          )

  def adjustForCursor(
    buffer: Buffer,
    currentState: AppState,
    cursor: CursorPosition
  ): Viewport =
    val wordWrapEnabled            = currentState.persisted.config.surfaceConfig.wordWrapEnabled
    val typewriterScrollingEnabled = currentState.persisted.config.surfaceConfig.typewriterScrollingEnabled
    val isTui                      = currentState.runtime.isTuiMode
    val viewport                   = buffer.viewport
    val fontConfig                 = currentState.persisted.config.editorConfig.fontConfig
    val font                       = previewFontForBuffer(buffer, fontConfig)
    val gridWidthPx                = TextLayoutSnapshot.gridWrapWidthPx(viewport.visibleColumns, fontConfig)
    // `viewport.visibleLines` is a code-grid row count (`LayoutEngine.updateViewportDimensions` sets it from the
    // panel's grid rows, the same convention `gridWrapWidthPx` uses for columns) -- it does not vary with which font a
    // pane actually draws with. A document-font pane whose font has a taller line height than the code font fits fewer
    // of its own rows into that same pixel height than a code-font pane would, exactly what
    // `RendererPaneSetup.snapshotForBuffer`'s own `visibleLines = panelHeightPx / bufferMetrics.lineHeight` computes
    // for the pane actually painted. Using the code-grid count unadjusted let this centred/bottom-aligned scroll math
    // assume more rows fit than the pane's own font renders, scrolling the cursor below the pane's real bottom edge on
    // a long wrapped line (#1041).
    val effectiveVisibleLines =
      if isTui then viewport.visibleLines
      else
        val codeLineHeightPx =
          CellMetrics.fromFont(FontLoader.previewFontForRole(fontConfig, TypographyRole.Code)).lineHeight
        val panelHeightPx = viewport.visibleLines * codeLineHeightPx
        math.max(1, panelHeightPx / math.max(1, CellMetrics.fromFont(font).lineHeight))
    val halfVisibleLines = effectiveVisibleLines / 2
    // Count wrapped rows the way the terminal actually drew them: TUI wraps on a fixed 1px-per-cell grid
    // (`CellMetrics.cellUnit` + forceCellLayout), never a pixel measurement of the inert proportional prose font, which
    // folds each line across a different number of rows and so mis-places the centred viewport. Mirrors
    // `EditorGeometryProducer`.
    val cellMetricsOverride = if isTui then Some(CellMetrics.cellUnit) else None
    val forceCellLayout     = isTui
    val wrapWidthPx         = if isTui then viewport.visibleColumns * CellMetrics.cellUnit.charWidth else gridWidthPx
    val lineText            = buffer.document.content.getLine(cursor.line).getOrElse("")
    val cursorVisualLine =
      if !wordWrapEnabled then 0
      else
        TextLayoutSnapshot.visualLineIndexForCursor(
          lineText,
          cursor.column,
          wrapWidthPx,
          font,
          wordWrapEnabled = true,
          cellMetricsOverride = cellMetricsOverride,
          forceCellLayout = forceCellLayout,
          rowAffinity = cursor.rowAffinity
        )

    // The number of visual rows a logical line occupies on screen -- 1 unless word wrap folds it across several
    // rows, in which case it must be measured the same way `cursorVisualLine` above was, or the two disagree.
    def visualRowCountForLine(lineIndex: Int): Int =
      if !wordWrapEnabled then 1
      else
        val text = buffer.document.content.getLine(lineIndex).getOrElse("")
        TextLayoutSnapshot
          .boundedVisualLinesForText(
            text,
            lineIndex,
            wrapWidthPx,
            font,
            cellMetricsOverride = cellMetricsOverride,
            forceCellLayout = forceCellLayout
          )
          .length
          .max(1)

    // Desired top: walk backward from the cursor's own line in visual rows (not logical lines) until halfVisibleLines
    // rows of context above the cursor's own visual row have been accounted for, or the buffer start is reached,
    // carrying the partial offset into whatever line the walk lands on so the cursor stays centred. Forcing that offset
    // to 0 (as before) whenever the top wasn't the cursor's own line let the cursor drift off-centre by up to a full
    // wrapped line's worth of rows.
    val scrollUpBudget = halfVisibleLines - cursorVisualLine
    def walkBackward(line: Int, remainingBudget: Int): (Int, Int) =
      if line <= 0 then (0, 0)
      else
        val previousLineRows = visualRowCountForLine(line - 1)
        if previousLineRows >= remainingBudget then (line - 1, previousLineRows - remainingBudget)
        else walkBackward(line - 1, remainingBudget - previousLineRows)
    val (rawTopLine, rawTopVisualLine) =
      if scrollUpBudget <= 0 then (cursor.line, math.max(0, cursorVisualLine - halfVisibleLines))
      else walkBackward(cursor.line, scrollUpBudget)

    // Bottom clamp: the latest (line, visual-row) start that still fills the viewport with real content, found by
    // walking backward from the buffer's last line until visibleLines rows of content have been accounted for.
    // Without this, a cursor near the end of a short-ish document can leave blank rows below the last line. Typewriter
    // scrolling deliberately skips this clamp: its entire point is to hold the cursor's line at its centred row even
    // while typing at the very end of the document, which means padding with blank rows below rather than showing as
    // much real content as fits (#1204, #1293).
    val lineCount = buffer.document.content.lineCount
    def bottomAlignedWindow(line: Int, remaining: Int): (Int, Int) =
      val rows = visualRowCountForLine(line)
      if remaining <= rows || line == 0 then (line, math.max(0, rows - remaining))
      else bottomAlignedWindow(line - 1, remaining - rows)
    val (bottomLine, bottomVisualLine) =
      if lineCount <= 0 then (0, 0) else bottomAlignedWindow(lineCount - 1, effectiveVisibleLines)

    val exceedsBottom =
      rawTopLine > bottomLine || (rawTopLine == bottomLine && rawTopVisualLine > bottomVisualLine)
    val (clampedTopLine, topVisualLine) =
      if typewriterScrollingEnabled then (rawTopLine, rawTopVisualLine)
      else if exceedsBottom then (bottomLine, bottomVisualLine)
      else (rawTopLine, rawTopVisualLine)
    val clampedLeftColumn =
      if wordWrapEnabled then 0
      else
        // Same TUI-vs-font split as the wrap measurement above: horizontal scrolling has to be measured on the grid
        // the terminal actually draws on, or a line of wide glyphs scrolls to the wrong column.
        val measuredLeftColumn =
          TextLayoutSnapshot.leftColumnForCursorVisibility(
            lineText,
            cursor.column,
            if isTui then wrapWidthPx else gridWidthPx,
            font,
            cellMetricsOverride = cellMetricsOverride,
            forceCellLayout = forceCellLayout
          )
        val minimumVisibleColumn = math.max(0, cursor.column - viewport.visibleColumns + 1)
        math.max(minimumVisibleColumn, measuredLeftColumn)

    viewport.copy(
      topLine = clampedTopLine,
      leftColumn = clampedLeftColumn,
      topVisualLine = topVisualLine
    )

  /** Column-based document layout (issue #1338, Phase 1): fixed/discrete snapping only -- `topVisualLine` always lands
    * on an exact `activeColumnIndex * visibleLines` boundary (`activeColumnIndex` derived here as
    * `absoluteCursorRow / visibleLines`, never stored), so the column holding the cursor's visual row is always shown
    * from its own top row. A sibling to [[adjustForCursor]] rather than a branch inside it -- that function's centring/
    * typewriter/bottom-clamp logic does not apply here at all, so branching it in would only add complexity to both
    * paths for no shared benefit.
    */
  def adjustForCursorColumnMode(
    buffer: Buffer,
    currentState: AppState,
    cursor: CursorPosition
  ): Viewport =
    val isTui               = currentState.runtime.isTuiMode
    val viewport            = buffer.viewport
    val fontConfig          = currentState.persisted.config.editorConfig.fontConfig
    val font                = previewFontForBuffer(buffer, fontConfig)
    val gridWidthPx         = TextLayoutSnapshot.gridWrapWidthPx(viewport.visibleColumns, fontConfig)
    val cellMetricsOverride = if isTui then Some(CellMetrics.cellUnit) else None
    val forceCellLayout     = isTui
    val wrapWidthPx         = if isTui then viewport.visibleColumns * CellMetrics.cellUnit.charWidth else gridWidthPx

    def visualRowCountForLine(lineIndex: Int): Int =
      val text = buffer.document.content.getLine(lineIndex).getOrElse("")
      TextLayoutSnapshot
        .boundedVisualLinesForText(
          text,
          lineIndex,
          wrapWidthPx,
          font,
          cellMetricsOverride = cellMetricsOverride,
          forceCellLayout = forceCellLayout
        )
        .length
        .max(1)

    val lineText = buffer.document.content.getLine(cursor.line).getOrElse("")
    val cursorVisualRowInLine =
      TextLayoutSnapshot.visualLineIndexForCursor(
        lineText,
        cursor.column,
        wrapWidthPx,
        font,
        wordWrapEnabled = true,
        cellMetricsOverride = cellMetricsOverride,
        forceCellLayout = forceCellLayout,
        rowAffinity = cursor.rowAffinity
      )
    val cumulativeRowsBeforeCursorLine = (0 until cursor.line).map(visualRowCountForLine).sum
    val absoluteCursorRow              = cumulativeRowsBeforeCursorLine + cursorVisualRowInLine

    val visibleLines      = math.max(1, viewport.visibleLines)
    val activeColumnIndex = absoluteCursorRow / visibleLines
    val targetVisualRow   = activeColumnIndex * visibleLines
    val lineCount         = buffer.document.content.lineCount

    @annotation.tailrec
    def findTop(line: Int, consumedRows: Int): (Int, Int) =
      if line >= lineCount then (math.max(0, lineCount - 1), 0)
      else
        val rows = visualRowCountForLine(line)
        if consumedRows + rows > targetVisualRow then (line, targetVisualRow - consumedRows)
        else findTop(line + 1, consumedRows + rows)
    val (topLine, topVisualLine) = if lineCount <= 0 then (0, 0) else findTop(0, 0)

    viewport.copy(topLine = topLine, leftColumn = 0, topVisualLine = topVisualLine)

  private def previewFontForBuffer(
    buffer: Buffer,
    config: FontLoader.FontConfig
  ): java.awt.Font =
    FontLoader.previewFontForRole(config, buffer.typographyRole)
