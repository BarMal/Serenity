package com.serenity.ui.renderer

/** The colour a [[RenderSurface]] paints with: one packed `0xAARRGGBB` sRGB value, alpha included. Owned by the render
  * seam so a surface backend never has to construct a `java.awt.Color` to receive one (#1812).
  *
  * The `Awt` conversions exist because the theme and config still hold `java.awt.Color`; they go away once those move
  * onto this type.
  */
opaque type RenderColor = Int

object RenderColor:

  def fromArgb(argb: Int): RenderColor = argb

  def fromAwt(color: java.awt.Color): RenderColor = color.getRGB

  extension (color: RenderColor)
    def argb: Int  = color
    def alpha: Int = (color >>> 24) & 0xff
    def red: Int   = (color >>> 16) & 0xff
    def green: Int = (color >>> 8) & 0xff
    def blue: Int  = color & 0xff

    def toAwt: java.awt.Color = new java.awt.Color(color, true)
