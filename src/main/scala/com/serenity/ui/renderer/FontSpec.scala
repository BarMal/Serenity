package com.serenity.ui.renderer

import java.awt.Font

/** The font a [[RenderSurface]] draws text with and the renderer entry points are handed. Owned by the render seam so
  * frontends and renderers name a font without naming `java.awt` (#1812).
  *
  * Fonts are still loaded, registered and measured through Java2D (bundled TTFs, ligature attributes, the
  * `FontRenderContext` the measurer shares), so the spec is the loaded `java.awt.Font` itself: it carries everything a
  * derived font holds, and wrapping it per `setFont` costs nothing. A backend that loads fonts elsewhere reads
  * [[family]]/[[sizePt]]/[[isBold]]/[[isItalic]] rather than unwrapping it.
  */
opaque type FontSpec = Font

object FontSpec:

  def fromAwt(font: Font): FontSpec = font

  extension (font: FontSpec)
    def family: String    = font.getFamily
    def sizePt: Float     = font.getSize2D
    def isBold: Boolean   = font.isBold
    def isItalic: Boolean = font.isItalic
    def toAwt: Font       = font
