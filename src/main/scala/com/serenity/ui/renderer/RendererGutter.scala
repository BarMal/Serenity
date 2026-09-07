package com.serenity.ui.renderer

import com.serenity.config.{AppMode, CornerPosition, CursorInfoBarPlacement}
import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Paints the line-number column and the bottom gutter row (cursor position, language, filename, and the optional
  * mode/tab corner widget), and the small chrome-text helpers those two share.
  *
  * It owns the whole mode/tab corner widget (issue #1307), both corners, which is why [[applyModeTabWidgetToTopCorner]]
  * is public rather than private: `TopLeft`/`TopRight` fold into the active pane's header text, painted by
  * [[RendererPaneContent]], and the two corners must agree on the glyph and the segment format or the indicator changes
  * shape when the user moves it.
  */
object RendererGutter:

  def renderLineNumbers(state: AppState, context: RenderContext, renderPlan: EditorPaneRenderPlan): Unit =
    if state.persisted.config.surfaceConfig.showLineNumbers then
      context.surface.text.setFont(context.uiFont)
      renderPlan.layoutContract.lineNumberRect foreach { lineRect =>
        val surface = context.surface

        surface.setBackgroundColor(state.persisted.theme.panel.background)
        surface.setForegroundColor(state.persisted.theme.muted)

        surface.fillRect(lineRect.x, lineRect.y, lineRect.width, lineRect.height, ' ')
        state.persisted.layout.activeEditorPaneId
          .flatMap(state.persisted.layout.editorPanes.get)
          .foreach { pane =>
            val buffer = pane.bufferId.flatMap(state.persisted.buffers.get)
            val snapshot =
              state.persisted.layout.activeEditorPaneId
                .flatMap(renderPlan.snapshots.get)
                .orElse {
                  for
                    paneLayout <- state.persisted.layout.activeEditorPaneId.flatMap(renderPlan.paneLayouts.get)
                    buf        <- buffer
                  yield RendererPaneSetup.snapshotForBuffer(buf, paneLayout.contentRect, state, context)
                }
            snapshot.foreach { snapshot =>
              renderPlan.layoutContract.lineNumberRowSlots(snapshot.visualLines.length).foreach {
                case SurfaceContentRowSlot(SurfaceContentRowKind.Item(index), rowY)
                    if RendererPaneContent.visualLineFits(lineRect, index, context, snapshot) =>
                  snapshot.visualLines.lift(index).foreach { visualLine =>
                    val lineTopPx = RendererPaneContent.visualLineTopPx(lineRect, index, context, snapshot)
                    val rendersLineNumber =
                      shouldRenderLineNumberForVisualLine(
                        visualLine,
                        state.persisted.config.surfaceConfig.wordWrapEnabled
                      )
                    val lineNumberText =
                      if rendersLineNumber then
                        val numberWidth = math.max(1, lineRect.width - 1)
                        (visualLine.bufferLine + 1).toString.reverse.padTo(numberWidth, ' ').reverse + " "
                      else continuationIndicatorText(lineRect.width)
                    val measuredLineNumberFont = buffer.filter(useMeasuredLineNumberFont(_, context))
                    if RendererPaneSetup.usesMeasuredDrawing(snapshot, context) && measuredLineNumberFont.nonEmpty then
                      measuredLineNumberFont.foreach(buf => surface.text.setFont(context.fontForBuffer(buf)))
                      surface.text.drawRunPx(
                        context.cellMetrics.toPixelX(lineRect.x).toFloat,
                        lineTopPx,
                        lineRect.width * context.cellMetrics.charWidth.toFloat,
                        snapshot.lineHeightPx,
                        snapshot.ascentPx,
                        lineNumberText
                      )
                      surface.text.setFont(context.uiFont)
                    else surface.putString(lineRect.x, rowY, lineNumberText)
                    if rendersLineNumber then
                      for
                        bufferId   <- pane.bufferId
                        annotation <- renderPlan.annotations.get(bufferId)
                      do
                        renderDiagnosticIndicator(
                          surface,
                          lineRect,
                          rowY,
                          annotation.diagnosticsByLine.getOrElse(visualLine.bufferLine, Nil),
                          state
                        )
                  }
                case _ => ()
              }
            }
          }
      }

  private def useMeasuredLineNumberFont(buffer: Buffer, context: RenderContext): Boolean =
    buffer.typographyRole != TypographyRole.Code && context.fontForBuffer(buffer) != context.codeFont

  private def shouldRenderLineNumberForVisualLine(visualLine: TextVisualLine, wordWrapEnabled: Boolean): Boolean =
    !wordWrapEnabled || visualLine.startColumn == 0

  private def continuationIndicatorText(width: Int): String =
    val safeWidth = math.max(1, width)
    val leftWidth = (safeWidth - 1) / 2
    " " * leftWidth + "│" + " " * (safeWidth - leftWidth - 1)

  private def renderDiagnosticIndicator(
    surface: RenderSurface,
    lineRect: LayoutRect,
    screenY: Int,
    lineDiags: List[com.serenity.lsp.model.Diagnostic],
    state: AppState
  ): Unit =
    if lineDiags.nonEmpty then
      val worstCode = lineDiags.flatMap(_.severity).map(_.code).minOption
      val color = worstCode match
        case Some(1) => state.persisted.theme.error.foreground
        case Some(2) => state.persisted.theme.warning.foreground
        case _       => state.persisted.theme.muted
      surface.setForegroundColor(color)
      surface.setBackgroundColor(state.persisted.theme.panel.background)
      surface.putString(lineRect.x + lineRect.width - 1, screenY, "!")

  def renderGutter(state: AppState, context: RenderContext, contract: EditorLayoutContract): Unit =
    contract.gutterRect.foreach { gutterRect =>
      context.surface.text.setFont(context.uiFont)
      val surface = context.surface

      // #1295: scoped to the pinned-bottom cursor info bar the same way TextOverlayRenderer scopes its own colour
      // override to the floating cursor info bar surface -- the legacy gutter (no info bar text to show) keeps the
      // theme's own panel colours unconditionally.
      val showsCursorInfoBar =
        state.persisted.config.cursorInfoBarPlacement == CursorInfoBarPlacement.PinnedBottom &&
          state.cursorInfoBarText.nonEmpty
      val infoBarColors = state.persisted.config.cursorInfoBarColors
      val gutterBackground =
        if showsCursorInfoBar then infoBarColors.backgroundOr(state.persisted.theme.panel.background)
        else state.persisted.theme.panel.background
      val gutterForeground =
        if showsCursorInfoBar then infoBarColors.foregroundOr(state.persisted.theme.panel.foreground)
        else state.persisted.theme.panel.foreground

      surface.setBackgroundColor(gutterBackground)
      surface.setForegroundColor(gutterForeground)

      surface.fillRect(gutterRect.x, gutterRect.y, gutterRect.width, gutterRect.height, ' ')

      val gutterContent = buildGutterContent(state)
      val displayContent =
        if gutterContent.length > gutterRect.width then gutterContent.take(gutterRect.width - 3) + "..."
        else gutterContent

      drawUiTextInCellRect(surface, context, gutterRect, displayContent)
    }

  private def drawUiTextInCellRect(
    surface: RenderSurface,
    context: RenderContext,
    rect: LayoutRect,
    text: String
  ): Unit =
    surface.text.fontRenderContext match
      case Some(frc) =>
        val rowHeightPx  = math.max(1, rect.height * context.cellMetrics.lineHeight)
        val lineHeightPx = math.max(1, math.min(context.uiMetrics.lineHeight, rowHeightPx - 2))
        val ascentPx     = math.max(1, math.min(context.uiMetrics.ascent, lineHeightPx))
        val placement = TextAlignment.placeLine(
          text = text,
          area = TextAreaPx(
            xPx = context.cellMetrics.toPixelX(rect.x).toFloat,
            yPx = context.cellMetrics.toPixelY(rect.y),
            widthPx = rect.width * context.cellMetrics.charWidth.toFloat,
            heightPx = rowHeightPx
          ),
          font = context.uiFont,
          lineHeightPx = lineHeightPx,
          ascentPx = ascentPx,
          horizontal = TextHorizontalAlignment.Left,
          vertical = TextVerticalAlignment.Middle,
          fontRenderContext = frc
        )

        surface.text.drawRunPx(
          xPx = placement.xPx,
          yPx = placement.yPx,
          bgWidthPx = placement.widthPx,
          lineHeightPx = placement.lineHeightPx,
          ascentPx = placement.ascentPx,
          s = text
        )
      case None =>
        val middleRow = rect.y + math.max(0, (rect.height - 1) / 2)
        CharacterRenderer.renderString(surface, rect.x, middleRow, text.take(math.max(0, rect.width)))

  private def buildGutterContent(state: AppState): String =
    val base =
      if state.persisted.config.cursorInfoBarPlacement == CursorInfoBarPlacement.PinnedBottom then
        state.cursorInfoBarText.map(text => s" $text ").getOrElse(legacyGutterContent(state))
      else legacyGutterContent(state)
    // Opt-in (`surfaceConfig.showWordCount`, off by default): appended as its own segment rather than folded into
    // `legacyGutterContent`/`cursorInfoBarText`, both of which existing callers assert on as exact strings.
    val withWordCount = state.wordCountStatusText.fold(base)(segment => s" ${base.trim} | $segment ")
    applyModeTabWidgetToBottomCorner(state, withWordCount)

  /** Folds the mode indicator (issue #1307) into the gutter row's own already-reserved, already-dynamic text for
    * `BottomLeft`/`BottomRight` rather than painting a separate rect over it -- the gutter is the only screen row every
    * render already treats as free to overwrite each frame, so sharing it (the same way `wordCountStatusText` shares it
    * above) is what keeps the indicator from clobbering whatever else happens to occupy that corner.
    * `TopLeft`/`TopRight` fold into the active pane's header instead -- see `RendererPaneContent.renderBufferHeader`.
    * Only the glyph is folded in, not the tab title: whichever chrome text it joins already shows that (the gutter's
    * own filename segment, or the header's title verbatim), so repeating it here would just show the same name twice.
    */
  private def applyModeTabWidgetToBottomCorner(state: AppState, gutterText: String): String =
    val segment = modeTabWidgetSegment(state)
    state.persisted.config.modeTabWidgetCornerPosition match
      case CornerPosition.BottomLeft                        => s" $segment ${gutterText.trim} "
      case CornerPosition.BottomRight                       => s" ${gutterText.trim} $segment "
      case CornerPosition.TopLeft | CornerPosition.TopRight => gutterText

  private def modeTabWidgetSegment(state: AppState): String =
    s"[${modeTabWidgetGlyph(state.persisted.config.appMode)}]"

  private def modeTabWidgetGlyph(mode: AppMode): String =
    mode match
      case AppMode.Code  => "C"
      case AppMode.Prose => "P"

  /** The `TopLeft`/`TopRight` half of the mode/tab corner widget (issue #1307): folded into the active pane's own
    * header text (which already shows "the current tab name" the issue asks the indicator to sit alongside) rather than
    * a separate rect, for the same collision-avoidance reason as `applyModeTabWidgetToBottomCorner`.
    */
  def applyModeTabWidgetToTopCorner(state: AppState, title: String): String =
    val segment = modeTabWidgetSegment(state)
    state.persisted.config.modeTabWidgetCornerPosition match
      case CornerPosition.TopLeft                                 => s"$segment $title"
      case CornerPosition.TopRight                                => s"$title $segment"
      case CornerPosition.BottomLeft | CornerPosition.BottomRight => title

  private def legacyGutterContent(state: AppState): String =
    state.persisted.layout.activeEditorPaneId.flatMap(state.persisted.layout.editorPanes.get) match
      case Some(pane) =>
        pane.bufferId.flatMap(state.persisted.buffers.get) match
          case Some(buffer) =>
            val cursor   = buffer.editing.cursors.headOption.getOrElse(CursorPosition(0, 0))
            val position = s"Line ${cursor.line + 1}, Col ${cursor.column + 1}"
            val language = buffer.document.language.fold("Plain Text")(_.displayName)

            val filePath = buffer.document.filePath match
              case Some(path) => s" | ${path.getFileName}"
              case None       => " | Not saved to file yet"

            s" $position | Language: $language$filePath "
          case None => " No active buffer "
      case None => " No active editor pane "
