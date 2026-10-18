package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.lsp.model.SemanticToken
import com.serenity.richtext.{ParagraphRole, RichTextDocument}
import com.serenity.state.models.{AppState, Buffer, TextVisualLine}
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{DropCapGlyph, DropCapLayout, TextLayoutSnapshot}
import com.serenity.ui.openings.LetterShape
import com.serenity.ui.theme.{StyledText, TextStyle, Theme}

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

  /** Draw the glyph as one oversized pixel run starting at the paragraph's first visual line and tall enough to cover
    * the whole glyph, whatever hangs below its last spanned line.
    *
    * @param xOriginPx
    *   left edge of the paragraph's normal text origin (the glyph draws flush with it; the body text wraps in beside it
    *   at the indent `DropCapLayout.leftInsetPx` reserves)
    * @param yTopPx
    *   top of the paragraph's first visual line
    * @param glyphHeightPx
    *   total pixel height the glyph covers (`DropCapGlyph.heightPx`)
    * @param glyphAscentPx
    *   how far below `yTopPx` the glyph's baseline sits (`DropCapGlyph.baselinePx`), which is the baseline of the last
    *   line the glyph spans
    * @param clipBottomPx
    *   the lowest pixel row the pane draws rows on: a glyph that would reach past it is cut there, since nothing below
    *   it is a row to repaint the glyph's rest from
    * @param overhangPx
    *   how far the letter reaches above `yTopPx` into the row above: it is painted over whatever that row holds rather
    *   than on a background of its own, which would wipe the row's text
    * @param clipTopPx
    *   the highest pixel row the pane draws rows on: a letter that would reach above it is cut there, and only then
    *   does the glyph's own background extend above `yTopPx`
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
    foreground: RenderColor,
    background: RenderColor,
    clipBottomPx: Int = Int.MaxValue,
    overhangPx: Int = 0,
    clipTopPx: Int = 0
  ): Unit =
    val cutAtTop      = overhangPx > 0 && yTopPx - overhangPx < clipTopPx
    val boxTopPx      = if cutAtTop then clipTopPx.min(yTopPx) else yTopPx
    val boxHeightPx   = glyphHeightPx + (yTopPx - boxTopPx)
    val drawnHeightPx = boxHeightPx.min(clipBottomPx - boxTopPx)
    if glyphText.nonEmpty && glyphWidthPx > 0.0f && drawnHeightPx > 0 then
      surface.setForegroundColor(foreground)
      surface.setBackgroundColor(background)
      surface.enableStyle(glyphStyle)
      try
        surface.text.drawRunPx(
          xOriginPx,
          boxTopPx,
          glyphWidthPx,
          drawnHeightPx,
          glyphAscentPx + (yTopPx - boxTopPx),
          glyphText,
          clipGlyphToRun = drawnHeightPx < boxHeightPx || cutAtTop
        )
      finally surface.disableStyle(glyphStyle)

  /** TUI fallback: highlight the first character's own cell as bold in `accentForeground`, with no attempt at multi-row
    * spanning (the fixed-cell grid has no font-size concept to do that with).
    */
  def renderGlyphCell(
    surface: RenderSurface,
    x: Int,
    y: Int,
    glyphText: String,
    accentForeground: RenderColor,
    background: RenderColor
  ): Unit =
    if glyphText.nonEmpty then
      surface.setForegroundColor(accentForeground)
      surface.setBackgroundColor(background)
      val boldStyle = TextStyle(isBold = true)
      surface.enableStyle(boldStyle)
      try surface.putString(x, y, glyphText)
      finally surface.disableStyle(boldStyle)

  /** Measured (GUI) home-line hook: paints the paragraph's glyph, fitted to the `dropCap.lines` lines it spans from
    * `lineTopPx` ([[DropCapGlyph]]), then returns the visual line and styled segments the caller's own ordinary
    * body-text draw call should paint instead of its originals -- both with the split-out first grapheme removed, so
    * the glyph is never drawn twice (see the type doc). Returns the inputs unchanged when the paragraph's first run is
    * empty or carries no drop cap glyph style.
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
    glyphColour: RenderColor,
    xOriginPx: Float,
    lineTopPx: Int,
    clipBottomPx: Int = Int.MaxValue,
    shape: LetterShape = LetterShape.Drop,
    clipTopPx: Int = 0
  ): (TextVisualLine, Option[List[StyledText]]) =
    DropCapGlyph.forParagraph(baseFont, frc, Some(document), visualLine.bufferLine, dropCap, proseScale, shape) match
      case None => (visualLine, styledSegments)
      case Some(glyph) =>
        renderGlyphPx(
          surface,
          xOriginPx,
          lineTopPx,
          glyph.widthPx,
          glyph.heightPx + glyph.growthPx,
          glyph.baselinePx + glyph.growthPx,
          glyph.text,
          glyph.style,
          glyphColour,
          theme.background,
          clipBottomPx,
          glyph.overhangPx,
          clipTopPx
        )
        (dropFirstGrapheme(visualLine, glyph.charCount), styledSegments.map(dropLeadingChars(_, glyph.charCount)))

  /** The colour the configured `document.drop_cap_colour` makes of the glyph, in the theme and heading settings `state`
    * carries.
    */
  private def glyphColourOf(state: AppState): RenderColor =
    val config = state.persisted.config
    config.documentConfig.dropCapColour.resolve(state.persisted.theme, config.headingSettings)

  /** Cell-grid (TUI) home-line hook: re-styles the one cell the caller's own ordinary text draw call already painted --
    * see [[renderGlyphCell]]'s own doc comment for why the fixed grid never attempts multi-row spanning.
    */
  def paintCellHomeLine(
    surface: RenderSurface,
    visualLine: TextVisualLine,
    x: Int,
    y: Int,
    theme: Theme,
    glyphColour: RenderColor
  ): Unit =
    visualLine.text.headOption.foreach { firstChar =>
      renderGlyphCell(surface, x, y, firstChar.toString, glyphColour, theme.background)
    }

  /** Drops the paragraph's split-out glyph character from a visual line's own drawable geometry: its text and the caret
    * stops at or past its new (shifted) start column -- the earlier stops belonged to the character the glyph itself
    * now paints. Keeping their `xPx` and the line's `xOffsetPx` rather than rebasing to 0 keeps the remaining text
    * exactly where `TextLayoutSnapshot` placed it: that layout takes the dropped character's width off the home row's
    * offset, so the text that remains starts at the indent every other spanned row starts at.
    */
  private def dropFirstGrapheme(visualLine: TextVisualLine, charCount: Int): TextVisualLine =
    val newStartColumn = visualLine.startColumn + charCount
    val row            = visualLine.row
    def keptFrom(xs: IArray[Float], offsets: IArray[Int]): (IArray[Float], IArray[Int]) =
      val kept = offsets.indices.filter(offsets(_) >= charCount)
      (IArray.from(kept.map(xs(_))), IArray.from(kept.map(offsets(_) - charCount)))
    val (caretXs, caretOffsets)     = keptFrom(row.caretXs, row.caretOffsets)
    val (landingXs, landingOffsets) = keptFrom(row.landingXs, row.landingOffsets)
    visualLine.copy(row =
      row.copy(
        text = row.text.drop(charCount),
        startColumn = newStartColumn,
        caretXs = caretXs,
        caretOffsets = caretOffsets,
        landingXs = landingXs,
        landingOffsets = landingOffsets
      )
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
    * paint a glyph for, as the snapshot's role resolver resolved it (so honouring the `document.drop_caps_enabled`
    * config toggle). `None` when there's no rich document, the paragraph isn't a drop cap, or this isn't its home line
    * (`DropCapLayout.homeLineRole`).
    */
  private def homeLineRoleFor(snapshot: TextLayoutSnapshot, visualLine: TextVisualLine): Option[ParagraphRole.DropCap] =
    snapshot.richTextDocument.flatMap(doc =>
      DropCapLayout.homeLineRole(snapshot.roles.layoutRole(doc, visualLine.bufferLine), visualLine.startColumn)
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
    visualLine: TextVisualLine,
    styledSegments: Option[List[StyledText]],
    measured: Boolean,
    surface: RenderSurface,
    baseFont: Font,
    frc: Option[FontRenderContext],
    theme: Theme,
    glyphColour: RenderColor,
    xOriginPx: Float,
    lineTopPx: Int,
    clipBottomPx: Int = Int.MaxValue,
    clipTopPx: Int = 0
  ): (TextVisualLine, Option[List[StyledText]]) =
    (for
      _         <- Option.when(measured)(())
      document  <- snapshot.richTextDocument
      dropCap   <- homeLineRoleFor(snapshot, visualLine)
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
      glyphColour,
      xOriginPx,
      lineTopPx,
      clipBottomPx,
      snapshot.roles.shapeAt(document, visualLine.bufferLine),
      clipTopPx
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
    semanticTokens: Option[List[SemanticToken]],
    clipBottomPx: Int = Int.MaxValue,
    clipTopPx: Int = 0
  ): Unit =
    val theme = state.persisted.theme
    val (drawLine, drawSegments) = adjustHomeLineDraw(
      snapshot,
      visualLine,
      styledSegments,
      true,
      context.surface,
      context.fontForBuffer(buffer),
      context.surface.text.fontRenderContext,
      theme,
      glyphColourOf(state),
      xOriginPx,
      lineTopPx,
      clipBottomPx,
      clipTopPx
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
    homeLineRoleFor(snapshot, visualLine).foreach(_ =>
      paintCellHomeLine(surface, visualLine, x, y, state.persisted.theme, glyphColourOf(state))
    )
