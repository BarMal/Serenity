package com.serenity.ui.renderer

import java.awt.Color

import com.serenity.ui.theme.TextStyle

/** Draws a paragraph's drop cap glyph: the oversized first character a [[com.serenity.richtext.ParagraphRole.DropCap]]
  * paragraph singles out (see `RichTextStyling.dropCapSplitFontSpans`), spanning several of the paragraph's normal
  * visual lines. Kept as a sibling of `CharacterRenderer` rather than added to it (which is already at this
  * codebase's size ceiling) -- `RendererPaneContent` calls into this module once per drop-cap paragraph instead of
  * growing either of those files.
  *
  * Measured (GUI) drawing only: [[renderGlyphPx]] draws one pixel-run spanning the glyph's full multi-line height, the
  * same `surface.text.drawRunPx` primitive `CharacterRenderer.renderMeasuredLineWithAnimation` uses per run. The
  * fixed-cell (TUI) grid has no font-size concept to spread a glyph across rows with, so [[renderGlyphCell]] instead
  * marks the one cell the first character already occupies as bold in a highlighted colour -- never attempting
  * multi-row spanning there, per this codebase's TUI degradation convention.
  */
object DropCapRenderer:

  /** Draw the glyph as one oversized pixel run, top-aligned at the paragraph's first visual line and tall enough to
    * reach the foot of its last spanned line.
    *
    * @param xOriginPx left edge of the paragraph's normal text origin (the glyph draws flush with it; callers add
    *                  the glyph's own measured width, via `DropCapLayout.leftInsetPx`, before drawing the body text
    *                  that wraps in beside it)
    * @param yTopPx    top of the paragraph's first visual line
    * @param glyphHeightPx total pixel height the glyph should span (`DropCapLayout.glyphHeightPx`)
    * @param glyphAscentPx the glyph font's own ascent, so its baseline sits correctly within `glyphHeightPx`
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

  /** TUI fallback: highlight the first character's own cell as bold in `accentForeground`, with no attempt at
    * multi-row spanning (the fixed-cell grid has no font-size concept to do that with).
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
