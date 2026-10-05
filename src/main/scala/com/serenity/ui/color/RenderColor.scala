package com.serenity.ui.color

/** The colour the theme holds and a `RenderSurface` paints with: one packed `0xAARRGGBB` sRGB value, alpha included, so
  * neither the theme nor a surface backend has to construct a `java.awt.Color` (#1812). It lives outside `ui.renderer`
  * so the theme can hold it without depending on the render seam.
  *
  * The `Awt` conversions are for the boundaries that genuinely speak AWT: the Java2D surface, Swing window chrome, and
  * app config colours that are still `java.awt.Color`.
  */
opaque type RenderColor = Int

object RenderColor:

  def fromArgb(argb: Int): RenderColor = argb

  /** Channels outside `0..255` are clamped, where `java.awt.Color` would throw. */
  def fromRgba(red: Int, green: Int, blue: Int, alpha: Int = 255): RenderColor =
    (channel(alpha) << 24) | (channel(red) << 16) | (channel(green) << 8) | channel(blue)

  def fromAwt(color: java.awt.Color): RenderColor = color.getRGB

  val Black: RenderColor = fromArgb(0xff000000)
  val White: RenderColor = fromArgb(0xffffffff)

  // Makes `==` against a `java.awt.Color` or a bare `Int` a compile error instead of a silently-false comparison.
  given CanEqual[RenderColor, RenderColor] = CanEqual.derived

  private def channel(value: Int): Int = value.max(0).min(255)

  extension (color: RenderColor)
    def argb: Int  = color
    def alpha: Int = (color >>> 24) & 0xff
    def red: Int   = (color >>> 16) & 0xff
    def green: Int = (color >>> 8) & 0xff
    def blue: Int  = color & 0xff

    def withAlpha(newAlpha: Int): RenderColor = (color & 0x00ffffff) | (channel(newAlpha) << 24)

    /** Component-wise linear blend toward `target`, `factor` clamped to `[0, 1]`; keeps this colour's alpha. */
    def blendToward(target: RenderColor, factor: Double): RenderColor =
      val t                                    = factor.max(0.0).min(1.0)
      def component(start: Int, end: Int): Int = math.round(start + (end - start) * t).toInt
      fromRgba(component(red, target.red), component(green, target.green), component(blue, target.blue), alpha)

    /** Opaque weighted average: `weight` (clamped to `[0, 1]`) of this colour, the rest of `background`. Both alphas
      * are ignored, so the result is what the mix looks like painted, not a translucent overlay.
      */
    def mixOver(background: RenderColor, weight: Double): RenderColor =
      val w                                     = weight.max(0.0).min(1.0)
      def component(top: Int, bottom: Int): Int = math.round(top * w + bottom * (1.0 - w)).toInt
      fromRgba(component(red, background.red), component(green, background.green), component(blue, background.blue))

    def toAwt: java.awt.Color = new java.awt.Color(color, true)
