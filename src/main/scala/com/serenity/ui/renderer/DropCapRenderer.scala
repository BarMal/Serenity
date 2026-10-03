package com.serenity.ui.renderer

import java.awt.font.FontRenderContext
import java.awt.{Color, Font}

import com.serenity.lsp.model.SemanticToken
import com.serenity.richtext.{ParagraphRole, RichTextDocument}
import com.serenity.state.models.{AppState, Buffer, TextVisualLine}
import com.serenity.ui.layout.{DropCapLayout, TextLayoutSnapshot}
import com.serenity.ui.theme.{RichTextStyling, StyledText, TextStyle, Theme}

/** Draws a paragraph's drop cap glyph: the oversized first character a [[com.serenity.richtext.ParagraphRole.DropCap]]
  * paragraph singles out (see `RichTextStyling.dropCapSplitFontSpans`), spanning several of the paragraph's normal
  * visual lines. Kept as a sibling of `CharacterRenderer` rather than added to it (which is already at this codebase's
  * size ceiling) -- `RendererPaneContent` calls into this module once per drop-cap paragraph instead of growing either
  * of those files.
  *
  * Measured (GUI) drawing only: [[renderGlyphPx]] draws one pixel-run spanning the glyph's full multi-line height, the
  * same `surface.text.drawRunPx` primitive `CharacterRenderer.renderMeasuredLine` uses per run. The fixed-cell (TUI)
  * grid has no font-size concept to spread a glyph across rows with, so [[renderGlyphCell]] instead marks the one cell
  * the first character already occupies as bold in a highlighted colour -- never attempting multi-row spanning there,
  * per this codebase's TUI degradation convention.
  */
object DropCapRenderer:

  /** Draw the glyph as one oversized pixel run, top-aligned at the paragraph's first visual line and tall enough to
    * reach the foot of its last spanned line.
    *
    * @param xOriginPx
    *   left edge of the paragraph's normal text origin (the glyph draws flush with it; callers add the glyph's own
    *   measured width, via `DropCapLayout.leftInsetPx`, before drawing the body text that wraps in beside it)
    * @param yTopPx
    *   top of the paragraph's first visual line
    * @param glyphHeightPx
    *   total pixel height the glyph should span (`DropCapLayout.glyphHeightPx`)
    * @param glyphAscentPx
    *   the glyph font's own ascent, so its baseline sits correctly within `glyphHeightPx`
    */
  def renderGlyphPx(
    surface: RenderSurface,
    xOriginPx: Float,
    yTopPx: Int,
    glyphWidthPx: Float,
    glyphHeightPx: Int,
    glyphAscentPx: Int,
    glyphText: String,
    glyphStyle: TextStyle,
    foreground: Color,
    background: Color
  ): Unit =
    if glyphText.nonEmpty && glyphWidthPx > 0.0f && glyphHeightPx > 0 then
      surface.setForegroundColor(foreground)
      surface.setBackgroundColor(background)
      surface.enableStyle(glyphStyle)
      try surface.text.drawRunPx(xOriginPx, yTopPx, glyphWidthPx, glyphHeightPx, glyphAscentPx, glyphText)
      finally surface.disableStyle(glyphStyle)

  /** TUI fallback: highlight the first character's own cell as bold in `accentForeground`, with no attempt at multi-row
    * spanning (the fixed-cell grid has no font-size concept to do that with).
    */
  def renderGlyphCell(
    surface: RenderSurface,
    x: Int,
    y: Int,
    glyphText: String,
    accentForeground: Color,
    background: Color
  ): Unit =
    if glyphText.nonEmpty then
      surface.setForegroundColor(accentForeground)
      surface.setBackgroundColor(background)
      val boldStyle = TextStyle(isBold = true)
      surface.enableStyle(boldStyle)
      try surface.putString(x, y, glyphText)
      finally surface.disableStyle(boldStyle)

  /** Measured (GUI) home-line hook: paints the paragraph's glyph spanning `dropCap.lines` of its own normal-height
    * visual lines from `lineTopPx`, then returns the visual line and styled segments the caller's own ordinary
    * body-text draw call should paint instead of its originals -- both with the split-out first grapheme removed, so
    * the glyph is never drawn twice (see the type doc). Returns the inputs unchanged when the paragraph's first run is
    * empty or carries no drop cap glyph style (`RichTextStyling.dropCapGlyphStyle`'s own `None` cases).
    */
  def paintMeasuredHomeLine(
    surface: RenderSurface,
    document: RichTextDocument,
    dropCap: ParagraphRole.DropCap,
    visualLine: TextVisualLine,
    styledSegments: Option[List[StyledText]],
    baseFont: Font,
    frc: FontRenderContext,
    proseScale: Float,
    theme: Theme,
    xOriginPx: Float,
    lineTopPx: Int,
    normalLineHeightPx: Int
  ): (TextVisualLine, Option[List[StyledText]]) =
    val glyph = document
      .paragraphAt(visualLine.bufferLine)
      .flatMap(_.runs.headOption)
      .filter(_.text.nonEmpty)
      .flatMap { firstRun =>
        val charCount = Character.charCount(firstRun.text.codePointAt(0))
        val glyphText = firstRun.text.take(charCount)
        RichTextStyling
          .dropCapGlyphStyle(firstRun.style, dropCap, RichTextStyling.ProseZoomBaselinePx, proseScale)
          .map(style => (charCount, glyphText, style))
      }
    glyph match
      case None => (visualLine, styledSegments)
      case Some((charCount, glyphText, glyphStyle)) =>
        val glyphFont     = TextStyle.styledFont(baseFont, glyphStyle)
        val glyphWidthPx  = DropCapLayout.glyphWidthPx(glyphFont, frc, glyphText)
        val glyphHeightPx = DropCapLayout.glyphHeightPx(dropCap, normalLineHeightPx)
        val glyphAscentPx = math.ceil(glyphFont.getLineMetrics(glyphText, frc).getAscent.toDouble).toInt
        renderGlyphPx(
          surface,
          xOriginPx,
          lineTopPx,
          glyphWidthPx,
          glyphHeightPx,
          glyphAscentPx,
          glyphText,
          glyphStyle,
          theme.foreground,
          theme.background
        )
        (dropFirstGrapheme(visualLine, charCount), styledSegments.map(dropLeadingChars(_, charCount)))

  /** Cell-grid (TUI) home-line hook: re-styles the one cell the caller's own ordinary text draw call already painted --
    * see [[renderGlyphCell]]'s own doc comment for why the fixed grid never attempts multi-row spanning.
    */
  def paintCellHomeLine(surface: RenderSurface, visualLine: TextVisualLine, x: Int, y: Int, theme: Theme): Unit =
    visualLine.text.headOption.foreach { firstChar =>
      renderGlyphCell(surface, x, y, firstChar.toString, theme.highlighted.foreground, theme.background)
    }

  /** Drops the paragraph's split-out glyph character from a visual line's own drawable geometry: its text and the caret
    * stops at or past its new (shifted) start column -- the earlier stops belonged to the character the glyph itself
    * now paints, and reusing their (already glyph-inset-shifted) `xPx` rather than rebasing to 0 keeps the remaining
    * text exactly where `TextLayoutSnapshot`'s own drop cap left-inset already placed it.
    */
  private def dropFirstGrapheme(visualLine: TextVisualLine, charCount: Int): TextVisualLine =
    val newStartColumn = visualLine.startColumn + charCount
    visualLine.copy(
      text = visualLine.text.drop(charCount),
      startColumn = newStartColumn,
      caretStops = visualLine.caretStops.filter(_.column >= newStartColumn),
      xSortedCaretStops = visualLine.xSortedCaretStops.filter(_.column >= newStartColumn)
    )

  /** Drops the same leading `count` characters from a parallel list of styled draw segments -- the coloured-text twin
    * of [[dropFirstGrapheme]], so the two stay aligned for the caller's draw call.
    */
  @annotation.tailrec
  private def dropLeadingChars(segments: List[StyledText], count: Int): List[StyledText] =
    segments match
      case StyledText(content, _, _, _) :: rest if count > 0 && content.length <= count =>
        dropLeadingChars(rest, count - content.length)
      case StyledText(content, style, fg, bg) :: rest if count > 0 =>
        StyledText(content.drop(count), style, fg, bg) :: rest
      case other => other

  /** Shared home-line detection for the two hooks below: the drop cap role (if any) `visualLine`'s own paragraph should
    * paint a glyph for, honouring the `document.drop_caps_enabled` config toggle exactly as
    * `RichTextStyling.effectiveRole` does. `None` when there's no rich document, the toggle is off, the paragraph isn't
    * a drop cap, or this isn't its home line (`DropCapLayout.homeLineRole`).
    */
  private def homeLineRoleFor(
    snapshot: TextLayoutSnapshot,
    state: AppState,
    visualLine: TextVisualLine
  ): Option[ParagraphRole.DropCap] =
    snapshot.richTextDocument.flatMap(doc =>
      DropCapLayout.homeLineRole(
        RichTextStyling.effectiveRole(
          doc.paragraphAt(visualLine.bufferLine).map(_.role).getOrElse(ParagraphRole.Body),
          state.persisted.config.documentConfig.dropCapsEnabled
        ),
        visualLine.startColumn
      )
    )

  /** `RendererPaneContent`'s one hook for the ordinary body-text draw call: on a drop cap paragraph's home line, when
    * painting through the measured (GUI) path (`measured`), this also paints the glyph itself (see
    * [[paintMeasuredHomeLine]]) and returns the split visual line/segments to draw instead. Every other case (cell/TUI
    * path, no drop cap, config toggle off, wrong line, no `FontRenderContext`) returns `(visualLine, styledSegments)`
    * unchanged -- the cell path's own glyph overlay is the separate, later [[paintCellHomeLineIfNeeded]] hook, since it
    * paints on top of the ordinary text instead of replacing it.
    */
  def adjustHomeLineDraw(
    snapshot: TextLayoutSnapshot,
    state: AppState,
    visualLine: TextVisualLine,
    styledSegments: Option[List[StyledText]],
    measured: Boolean,
    surface: RenderSurface,
    baseFont: Font,
    frc: Option[FontRenderContext],
    theme: Theme,
    xOriginPx: Float,
    lineTopPx: Int,
    normalLineHeightPx: Int
  ): (TextVisualLine, Option[List[StyledText]]) =
    (for
      _         <- Option.when(measured)(())
      document  <- snapshot.richTextDocument
      dropCap   <- homeLineRoleFor(snapshot, state, visualLine)
      renderFrc <- frc
    yield paintMeasuredHomeLine(
      surface,
      document,
      dropCap,
      visualLine,
      styledSegments,
      baseFont,
      renderFrc,
      snapshot.proseScale,
      theme,
      xOriginPx,
      lineTopPx,
      normalLineHeightPx
    )).getOrElse((visualLine, styledSegments))

  /** `RendererPaneContent`'s single call for the measured (GUI) path: computes the drop-cap-adjusted line/segments
    * ([[adjustHomeLineDraw]]) and paints them via `CharacterRenderer.renderMeasuredLine` -- keeping that whole call out
    * of `RendererPaneContent`, which is already at this codebase's size ceiling.
    */
  def renderMeasuredHomeAware(
    snapshot: TextLayoutSnapshot,
    state: AppState,
    visualLine: TextVisualLine,
    styledSegments: Option[List[StyledText]],
    context: RenderContext,
    buffer: Buffer,
    xOriginPx: Float,
    lineTopPx: Int,
    lineHeightPx: Int,
    ascentPx: Int,
    clipRightXPx: Float,
    semanticTokens: Option[List[SemanticToken]]
  ): Unit =
    val theme = state.persisted.theme
    val (drawLine, drawSegments) = adjustHomeLineDraw(
      snapshot,
      state,
      visualLine,
      styledSegments,
      true,
      context.surface,
      context.fontForBuffer(buffer),
      context.surface.text.fontRenderContext,
      theme,
      xOriginPx,
      lineTopPx,
      lineHeightPx
    )
    CharacterRenderer.renderMeasuredLine(
      context.surface,
      xOriginPx,
      lineTopPx,
      lineHeightPx,
      ascentPx,
      drawLine,
      theme,
      state.syntaxHighlightingEnabled,
      buffer.document.language,
      drawSegments,
      clipRightXPx = Some(clipRightXPx),
      semanticTokens = semanticTokens,
      highlightCache = context.caches.themeHighlightCache,
      graphemeCache = context.caches.graphemeSegmentationCache
    )

  /** `RendererPaneContent`'s post-draw hook for the cell/TUI path: paints the drop cap glyph overlay
    * ([[paintCellHomeLine]]) on a drop cap paragraph's home line, a no-op everywhere else.
    */
  def paintCellHomeLineIfNeeded(
    snapshot: TextLayoutSnapshot,
    state: AppState,
    visualLine: TextVisualLine,
    surface: RenderSurface,
    x: Int,
    y: Int
  ): Unit =
    homeLineRoleFor(snapshot, state, visualLine).foreach(_ =>
      paintCellHomeLine(surface, visualLine, x, y, state.persisted.theme)
    )
