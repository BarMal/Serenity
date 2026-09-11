package com.serenity.ui.theme

/** Opacity in `[0.0, 1.0]`, the scale `ThemeColor.alpha` blends on. Distinct from `java.awt.Color`'s native 0-255 alpha
  * byte -- `toByteScale`/`fromByteScale` are the only sanctioned crossing points between the two scales, so a raw
  * `* 255` or `/ 255.0` elsewhere is a sign the wrong scale snuck across a boundary.
  */
opaque type NormalizedAlpha = Double

object NormalizedAlpha:

  /** Clamps into `[0.0, 1.0]` rather than rejecting -- this is a rendering value, and clamping is the sensible recovery
    * for an out-of-range input, not a hard failure.
    */
  def apply(value: Double): NormalizedAlpha = value.max(0.0).min(1.0)

  val Opaque: NormalizedAlpha      = NormalizedAlpha(1.0)
  val Transparent: NormalizedAlpha = NormalizedAlpha(0.0)

  /** From `java.awt.Color`'s native 0-255 alpha byte. */
  def fromByteScale(byte: Int): NormalizedAlpha = NormalizedAlpha(byte / 255.0)

  extension (a: NormalizedAlpha)
    def value: Double = a

    /** To `java.awt.Color`'s native 0-255 alpha byte. */
    def toByteScale: Int = math.round(a * 255).toInt
