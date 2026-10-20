package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.FontRenderContext

/** A glyph's own advance, which `GlyphVector.getGlyphMetrics` also reports but only after measuring the glyph's
  * outline: a native call per glyph on macOS, which made measuring a page of text there cost minutes. A vector of the
  * one glyph ends where its advance does.
  */
object GlyphAdvance:

  def of(font: Font, frc: FontRenderContext, glyphCode: Int): Float =
    FontFirstUse.settle(font, frc)
    font.createGlyphVector(frc, Array(glyphCode)).getGlyphPosition(1).getX.toFloat
